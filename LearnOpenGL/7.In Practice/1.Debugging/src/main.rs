use gfx_maths::{mat4::Mat4, quaternion::Quaternion, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, ffi::c_void, fs::File, io::Read, mem::size_of, ptr, slice, str};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 10.0;

pub mod camera;
use camera::camera::Camera;
use camera::camera::Movement;
pub mod mesh;
use mesh::mesh::{Texture, VertexArray};
pub mod shader;
use shader::shader::Program;

fn gl_check_error_(file: &str, line: u32) -> u32 {
    let mut error_code = unsafe { gl::GetError() };
    while error_code != gl::NO_ERROR {
        let error = match error_code {
            gl::INVALID_ENUM => "INVALID_ENUM",
            gl::INVALID_VALUE => "INVALID_VALUE",
            gl::INVALID_OPERATION => "INVALID_OPERATION",
            gl::STACK_OVERFLOW => "STACK_OVERFLOW",
            gl::STACK_UNDERFLOW => "STACK_UNDERFLOW",
            gl::OUT_OF_MEMORY => "OUT_OF_MEMORY",
            gl::INVALID_FRAMEBUFFER_OPERATION => "INVALID_FRAMEBUFFER_OPERATION",
            _ => "UNKNOWN",
        };
        println!("{} | {} ({})", error, file, line);
        error_code = unsafe { gl::GetError() };
    }
    error_code
}

macro_rules! gl_check_error {
    () => {
        gl_check_error_(file!(), line!())
    };
}

extern "system" fn gl_debug_output(
    source: gl::types::GLenum,
    err_type: gl::types::GLenum,
    id: u32,
    severity: gl::types::GLenum,
    length: gl::types::GLsizei,
    c_message: *const i8,
    _user_param: *mut c_void,
) {
    if id == 131169 || id == 131185 || id == 131218 || id == 131204 {
        return;
    }

    println!("---------------");
    let message = unsafe {
        slice::from_raw_parts(c_message, length as usize)
            .iter()
            .take_while(|&i| *i >= 0)
            .map(|&i| i as u8)
            .collect::<Vec<u8>>()
    };
    match str::from_utf8(&message) {
        Err(error) => println!("Debug message ({}): {:?}\n{}", id, message, error),
        Ok(message) => println!("Debug message ({}): {}", id, message),
    }

    match source {
        gl::DEBUG_SOURCE_API => println!("Source: API"),
        gl::DEBUG_SOURCE_WINDOW_SYSTEM => println!("Souce: Window System"),
        gl::DEBUG_SOURCE_SHADER_COMPILER => println!("Source: Shader Compiler"),
        gl::DEBUG_SOURCE_THIRD_PARTY => println!("Source: Third Party"),
        gl::DEBUG_SOURCE_APPLICATION => println!("Source: Application"),
        gl::DEBUG_SOURCE_OTHER => println!("Source: Other"),
        _ => println!("Source: Unknown"),
    };
    println!("");

    match err_type {
        gl::DEBUG_TYPE_ERROR => println!("Type: Error"),
        gl::DEBUG_TYPE_DEPRECATED_BEHAVIOR => println!("Type: Deprecated Behaviour"),
        gl::DEBUG_TYPE_UNDEFINED_BEHAVIOR => println!("Type: Undefined Behaviour"),
        gl::DEBUG_TYPE_PORTABILITY => println!("Type: Portability"),
        gl::DEBUG_TYPE_PERFORMANCE => println!("Type: Performance"),
        gl::DEBUG_TYPE_MARKER => println!("Type: Marker"),
        gl::DEBUG_TYPE_PUSH_GROUP => println!("Type: Push Group"),
        gl::DEBUG_TYPE_POP_GROUP => println!("Type: Pop Group"),
        gl::DEBUG_TYPE_OTHER => println!("Type: Other"),
        _ => println!("Source: Unknown"),
    };
    println!("");

    match severity {
        gl::DEBUG_SEVERITY_HIGH => println!("Severity: high"),
        gl::DEBUG_SEVERITY_MEDIUM => println!("Severity: medium"),
        gl::DEBUG_SEVERITY_LOW => println!("Severity: low"),
        gl::DEBUG_SEVERITY_NOTIFICATION => println!("Severity: notification"),
        _ => println!("Type: Other"),
    };
    println!("");
    println!("");
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::Samples(Some(4)));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));
    glfw.window_hint(WindowHint::OpenGlDebugContext(true));

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

    let mut flags = 0;
    unsafe {
        gl::GetIntegerv(gl::CONTEXT_FLAGS, &mut flags);
        if flags & gl::CONTEXT_FLAG_DEBUG_BIT as i32 > 0 {
            gl::Enable(gl::DEBUG_OUTPUT);
            gl::Enable(gl::DEBUG_OUTPUT_SYNCHRONOUS);
            gl::DebugMessageCallback(Some(gl_debug_output), ptr::null());
            gl::DebugMessageControl(gl::DONT_CARE, gl::DONT_CARE, gl::DONT_CARE, 0, ptr::null(), gl::TRUE);
        }
    }

    ogl::depth_enable(true);
    ogl::cull_enable();

    let _quad = VertexArray::new_quad();
    let cube = VertexArray::new_cube();

    let shader = Program::new().link("src/debugging.vs", "src/debugging.fs");
    let texture = Texture::new().load("resources/textures/wood.png");

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let projection = Mat4::perspective_opengl(PI / 4.0, SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
    shader.apply();
    shader.set_int("tex", 0);

    ogl::viewport(SCR_WIDTH as i32, SCR_HEIGHT as i32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        gl_check_error!();

        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        shader.set_mat4("projection", projection * camera.view_matrix());

        ogl::clear_color(0.0, 0.0, 0.0, 1.0);
        ogl::clear();

        shader.apply();
        let rotation_speed = 10.0;
        let angle = current_frame * rotation_speed;
        let model = Mat4::translate(Vec3::new(0.0, 0.0, -2.5)) * Mat4::rotate(Quaternion::axis_angle(Vec3::one(), angle * PI / 180.0));
        shader.set_mat4("model", model);
        ogl::active_texture(0);
        texture.bind();
        cube.bind();
        cube.draw();
        ogl::unbind_vao();

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
        WindowEvent::FramebufferSize(width, height) => unsafe {
            gl::Viewport(0, 0, width, height);
        },
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

    pub fn unbind_vao() {
        unsafe {
            gl::BindVertexArray(0);
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

    pub fn cull_enable() {
        unsafe {
            gl::Enable(gl::CULL_FACE);
        }
    }

    pub fn active_texture(unit: u32) {
        assert!(unit <= gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            gl::ActiveTexture(gl::TEXTURE0 + unit);
        }
    }

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
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
