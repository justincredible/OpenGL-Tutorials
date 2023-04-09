use gfx_maths::{mat4::Mat4, vec2::Vec2, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, fs::File, io::Read, mem::size_of, ptr, rc::Rc};

const SCR_WIDTH: u32 = 1280;
const SCR_HEIGHT: u32 = 720;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod camera;
use camera::camera::Camera;
use camera::camera::Movement;
pub mod mesh;
use mesh::mesh::{Texture, VertexArray};
pub mod shader;
use shader::shader::Program;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::Samples(Some(4)));
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

    let gl = Rc::new(gl::Gl::load_with(|s| window.get_proc_address(s).cast()));

    gl.depth_enable(true);

    let shader = Program::new(Rc::clone(&gl)).link("src/1.2.pbr.vs", "src/1.2.pbr.fs");

    shader.apply();
    shader.set_int("albedoMap", 0);
    shader.set_int("normalMap", 1);
    shader.set_int("metallicMap", 2);
    shader.set_int("roughnessMap", 3);
    shader.set_int("aoMap", 4);

    let albedo = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/pbr/rusted_iron/albedo.png", false);
    let normal = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/pbr/rusted_iron/normal.png", false);
    let metallic = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/pbr/rusted_iron/metallic.png", false);
    let roughness = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/pbr/rusted_iron/roughness.png", false);
    let ao = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/pbr/rusted_iron/ao.png", false);

    let sphere = VertexArray::new_sphere(Rc::clone(&gl));

    let light_positions = vec![Vec3::new(0.0, 0.0, 10.0)];
    let light_colors = vec![Vec3::new(150.0, 150.0, 150.0)];
    let nr_rows = 7;
    let nr_columns = 7;
    let spacing = 2.5;

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
    shader.set_mat4("projection", projection);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear_color(0.1, 0.1, 0.1, 1.0);
        gl.clear();

        let view = camera.view_matrix();
        shader.set_mat4("view", view);
        shader.set_vec3("camPos", camera.position());

        gl.active_texture(0);
        albedo.bind();
        gl.active_texture(1);
        normal.bind();
        gl.active_texture(2);
        metallic.bind();
        gl.active_texture(3);
        roughness.bind();
        gl.active_texture(4);
        ao.bind();

        sphere.bind();
        for row in 0..nr_rows {
            for col in 0..nr_columns {
                let model = Mat4::translate(Vec3::new(spacing * (col - nr_columns / 2) as f32, spacing * (row - nr_rows / 2) as f32, 0.0));
                shader.set_mat4("model", model);
                sphere.draw();
            }
        }

        for i in 0..light_positions.len() {
            let new_pos = *light_positions.get(i).unwrap();
            shader.set_vec3(&("lightPositions[".to_string() + &(i.to_string() + "]")), new_pos);
            shader.set_vec3(&("lightColors[".to_string() + &(i.to_string() + "]")), *light_colors.get(i).unwrap());
            let model = Mat4::translate(new_pos) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
            shader.set_mat4("model", model);
            sphere.draw();
        }

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut camera, &mut window, event);
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

fn handle_window_event(gl: &gl::Gl, camera: &mut Camera, window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        WindowEvent::FramebufferSize(width, height) => unsafe {
            gl.Viewport(0, 0, width, height);
        },
        WindowEvent::Scroll(_, y_offset) => camera.process_scroll(y_offset as f32),
        WindowEvent::CursorPos(_x_pos, _y_pos) => {
            let (x_pos, y_pos) = window.get_cursor_pos();

            camera.process_mouse(x_pos as f32, y_pos as f32, true);
        }
        _ => {}
    }
}

mod gl {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

    impl self::Gl {
        pub fn clear_color(&self, red: f32, green: f32, blue: f32, alpha: f32) {
            unsafe {
                self.ClearColor(red, green, blue, alpha);
            }
        }

        pub fn clear(&self) {
            unsafe {
                self.Clear(self::COLOR_BUFFER_BIT | self::DEPTH_BUFFER_BIT);
            }
        }

        pub fn unbind_vao(&self) {
            unsafe {
                self.BindVertexArray(0);
            }
        }

        pub fn unbind_framebuffer(&self) {
            unsafe {
                self.BindFramebuffer(self::FRAMEBUFFER, 0);
            }
        }

        pub fn depth_enable(&self, on: bool) {
            unsafe {
                if on {
                    self.Enable(self::DEPTH_TEST);
                } else {
                    self.Disable(self::DEPTH_TEST);
                }
            }
        }

        pub fn active_texture(&self, unit: u32) {
            assert!(unit <= self::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

            unsafe {
                self.ActiveTexture(self::TEXTURE0 + unit);
            }
        }
    }

    pub fn c_name(name: &str) -> Vec<i8> {
        c_finish(&mut name.as_bytes().iter())
    }

    pub fn c_named(name: &str, number: &str) -> Vec<i8> {
        c_finish(&mut name.as_bytes().iter().chain(number.as_bytes().iter()))
    }

    fn c_finish(iter: &mut dyn Iterator<Item = &u8>) -> Vec<i8> {
        iter.filter(|&u| *u < 128u8).map(|u| *u as i8).chain(std::iter::once(0)).collect::<Vec<_>>()
    }
}
