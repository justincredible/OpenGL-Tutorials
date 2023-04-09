use glam::{Mat4, Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, mem::size_of};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100_000.0;

pub mod camera;
use camera::camera::{Camera, Movement};
pub mod mesh;
use mesh::mesh::{stbi_flip_vertical, Image, VertexArray};
pub mod shader;
use shader::shader::Program;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw
        .create_window(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_cursor_mode(CursorMode::Disabled);
    window.set_framebuffer_size_polling(true);
    window.set_cursor_pos_polling(true);
    window.set_scroll_polling(true);
    window.focus();

    gl::load_with(|s| window.get_proc_address(s) as *const _);

    ogl::depth_enable(true);

    stbi_flip_vertical(true);

    let height_map_shader = Program::new().link("src/8.3.cpuheight.vs", "src/8.3.cpuheight.fs");

    let data = Image::new("resources/heightmaps/iceland_heightmap.png", false);

    let mut vertices = Vec::new();
    let y_scale = 0.25;
    let y_shift = 16.0;
    let rez = 1;
    let byte_per_pixel = data.components;
    for i in 0..data.height {
        for j in 0..data.width {
            let y;
            unsafe {
                let pixel_offset = data.data().offset(((j + data.width * i) * byte_per_pixel) as isize);
                y = *pixel_offset;
            }

            vertices.push(-data.height as f32 / 2.0 + (data.height * i) as f32 / data.height as f32); // vx
            vertices.push(((y as f32 * y_scale - y_shift) as i32) as f32); // vy
            vertices.push(-data.width as f32 / 2.0 + (data.width * j) as f32 / data.width as f32);
            // vz
        }
    }
    println!("Loaded {} vertices", vertices.len() / 3);

    let mut indices = Vec::new();
    for i in 0..data.height - 1 {
        for j in 0..data.width {
            for k in 0..2 {
                indices.push((j + data.width * (i + k * rez)) as u32);
            }
        }
    }
    println!("Loaded {} indices", indices.len());

    let num_strips = (data.height - 1) / rez;
    let num_tris_per_strip = (data.width / rez) * 2 - 2;
    println!("Created lattice of {} strips with {} triangles each", num_strips, num_tris_per_strip);
    println!("Created {} triangles total", num_strips * num_tris_per_strip);

    let terrain = VertexArray::new_with(vertices, indices);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(67.0, 627.5, 169.9), -427.0 * PI / 600.0, -53.0 * PI / 225.0, x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        ogl::clear_color(0.1, 0.1, 0.1, 1.0);
        ogl::clear();

        height_map_shader.apply();

        let projection = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, SCR_NEAR, SCR_FAR);
        let view = camera.view_matrix();
        height_map_shader.set_mat4("projection", projection);
        height_map_shader.set_mat4("view", view);
        height_map_shader.set_mat4("model", Mat4::IDENTITY);

        terrain.bind();
        for strip in 0..num_strips {
            unsafe {
                gl::DrawElements(
                    gl::TRIANGLE_STRIP,
                    num_tris_per_strip + 2,
                    gl::UNSIGNED_INT,
                    ((size_of::<u32>() as i32 * (num_tris_per_strip + 2) * strip) as *const usize).cast(),
                );
            }
        }

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&mut camera, &mut window, event);
        }
    }
}

fn process_input(camera: &mut Camera, window: &mut Window, delta_time: f32) {
    if window.get_key(Key::Escape) == Action::Press {
        window.set_should_close(true);
    }

    if window.get_key(Key::W) == Action::Press {
        camera.process_keyboard(Movement::Forward, delta_time);
    }
    if window.get_key(Key::S) == Action::Press {
        camera.process_keyboard(Movement::Backward, delta_time);
    }
    if window.get_key(Key::A) == Action::Press {
        camera.process_keyboard(Movement::Left, delta_time);
    }
    if window.get_key(Key::D) == Action::Press {
        camera.process_keyboard(Movement::Right, delta_time);
    }
}

fn handle_window_event(camera: &mut Camera, window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        WindowEvent::FramebufferSize(width, height) => ogl::viewport(width, height),
        WindowEvent::Scroll(_, y_offset) => camera.process_scroll(y_offset as f32),
        WindowEvent::CursorPos(_x_pos, _y_pos) => {
            let (x_pos, y_pos) = window.get_cursor_pos();

            camera.process_mouse(x_pos as f32, y_pos as f32, true);
        }
        _ => {}
    }
}

mod ogl {
    pub fn clear_color(red: f32, green: f32, blue: f32, alpha: f32) {
        unsafe {
            gl::ClearColor(red, green, blue, alpha);
        }
    }

    pub fn clear() {
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
        }
    }

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }

    pub fn depth_enable(on: bool) {
        unsafe {
            if on {
                gl::Enable(gl::DEPTH_TEST);
            } else {
                gl::Disable(gl::DEPTH_TEST);
            }
        }
    }
}
