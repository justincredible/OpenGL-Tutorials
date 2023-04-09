use glam::{Mat4, Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use stb_image::stb_image::bindgen;

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod animation;
use animation::animation::{Animation, Animator};
pub mod bone;
use bone::bone::Bone;
pub mod camera;
use camera::camera::{Camera, Movement};
pub mod mesh;
use mesh::mesh::{stbi_flip_vertical, Mesh, Texture};
pub mod model;
use model::model::{convert_matrix, BoneInfo, Model, Vertex};
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

    let shader = Program::new().link("src/anim_model.vs", "src/anim_model.fs");

    let vampire = Model::new(false).load_model("resources/objects/vampire/dancing_vampire.dae");
    let dance_animation = Animation::new("resources/objects/vampire/dancing_vampire.dae", &vampire);
    let mut animator = Animator::new(dance_animation);

    //ogl::polygon_mode();

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);
        animator.update_animation(delta_time);

        ogl::clear_color(0.05, 0.05, 0.05, 1.0);
        ogl::clear();

        shader.apply();

        let projection = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, SCR_NEAR, SCR_FAR);
        let view = camera.view_matrix();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);

        let transforms = animator.final_bone_matrices();
        for i in 0..transforms.len() {
            shader.set_mat4(&("finalBonesMatrices[".to_string() + &(i.to_string() + "]")), transforms[i]);
        }

        let model = Mat4::from_translation(Vec3::new(0.0, -0.4, 0.0)) * Mat4::from_scale(Vec3::new(0.5, 0.5, 0.5));
        shader.set_mat4("model", model);
        vampire.draw(&shader);

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
