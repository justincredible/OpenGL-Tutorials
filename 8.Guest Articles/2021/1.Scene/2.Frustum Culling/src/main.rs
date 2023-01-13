use glam::{Mat4, Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use stb_image::stb_image::bindgen;
use std::rc::Rc;

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod camera;
use camera::camera::{Camera, Movement};
pub mod entity;
use entity::entity::{create_frustum_from_camera, Entity};
pub mod mesh;
use mesh::mesh::{stbi_flip_vertical, Mesh, Texture, VertexArray};
pub mod model;
use model::model::{Model, Vertex};
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

    stbi_flip_vertical(true);

    ogl::depth_enable(true);

    let _quad = VertexArray::new_quad();
    let _cube = VertexArray::new_cube();

    let shader = Program::new().link("src/1.model_loading.vs", "src/1.model_loading.fs");

    let planet = Rc::new(Model::new().load_model("resources/objects/planet/planet.obj"));
    let mut entity = Entity::from(&planet);
    entity.transform.set_local_position(Vec3::new(0.0, 0.0, 0.0));
    let scale = 1.0;
    entity.transform.set_local_scale(Vec3::splat(scale));

    for x in 0..20 {
        for z in 0..20 {
            let mut child = Entity::from(&planet);
            child.transform.set_local_position(Vec3::new(x as f32 * 10.0 - 100.0, 0.0, z as f32 * 10.0 - 100.0));
            entity.add_child(child);
        }
    }
    entity.update_self_and_child();

    //ogl::polygon_mode();

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 10.0, 0.0), x_pos as f32, y_pos as f32);
    let mut camera_spy = Camera::new(Vec3::new(0.0, 10.0, 0.0), x_pos as f32, y_pos as f32);
    camera.speed = 20.0;

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        ogl::clear_color(0.05, 0.05, 0.05, 1.0);
        ogl::clear();

        shader.apply();

        let projection = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, SCR_NEAR, SCR_FAR);
        let cam_frustum = create_frustum_from_camera(&camera, SCR_WIDTH as f32 / SCR_HEIGHT as f32, camera.zoom(), SCR_NEAR, SCR_FAR);

        camera_spy.process_mouse(2.0, 0.0, true);

        let view = camera.view_matrix();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);

        let mut total = 0;
        let mut display = 0;
        entity.draw_self_and_child(&cam_frustum, &shader, &mut display, &mut total);
        println!("Total process in CPU : {} / Total send to GPU : {}", total, display);

        //entity.transform.set_local_rotation(Vec3::new(0.0, entity.transform.get_local_rotation().y + PI / 9.0 * delta_time, 0.0));
        entity.update_self_and_child();

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

    pub fn depth_enable(on: bool) {
        unsafe {
            if on {
                gl::Enable(gl::DEPTH_TEST);
            } else {
                gl::Disable(gl::DEPTH_TEST);
            }
        }
    }

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }

    pub fn polygon_mode() {
        unsafe {
            gl::PolygonMode(gl::FRONT_AND_BACK, gl::LINE);
        }
    }
}
