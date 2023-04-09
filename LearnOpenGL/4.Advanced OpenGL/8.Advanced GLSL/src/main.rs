use gfx_maths::{mat4::Mat4, vec2::Vec2, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, fs::File, io::Read, mem::size_of, ptr, rc::Rc};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod camera;
use camera::camera::Camera;
use camera::camera::Movement;
pub mod mesh;
use mesh::mesh::VertexArray;
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

    let gl = Rc::new(gl::Gl::load_with(|s| window.get_proc_address(s).cast()));

    gl.depth_enable(true);

    let shader_red = Program::new(Rc::clone(&gl)).link("src/8.advanced_glsl.vs", "src/8.red.fs");
    let shader_green = Program::new(Rc::clone(&gl)).link("src/8.advanced_glsl.vs", "src/8.green.fs");
    let shader_blue = Program::new(Rc::clone(&gl)).link("src/8.advanced_glsl.vs", "src/8.blue.fs");
    let shader_yellow = Program::new(Rc::clone(&gl)).link("src/8.advanced_glsl.vs", "src/8.yellow.fs");

    let cube = VertexArray::new_pos(Rc::clone(&gl), Vec::from(&CUBE_VERTICES[..]), Vec::from(&INDICES[..]));

    shader_red.set_block("Matrices", 0);
    shader_green.set_block("Matrices", 0);
    shader_blue.set_block("Matrices", 0);
    shader_yellow.set_block("Matrices", 0);

    let matrices = UniformBuffer::new(Rc::clone(&gl));

    let projection = Mat4::perspective_opengl(PI / 4.0, SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
    matrices.buffer_data(0, projection);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear(0.1, 0.1, 0.1, 1.0);

        let view = camera.view_matrix();
        matrices.buffer_data(size_of::<Mat4>(), view);

        cube.bind();
        shader_red.apply();
        shader_red.set_mat4("model", Mat4::translate(Vec3::new(-0.75, 0.75, 0.0)));
        cube.draw();
        shader_green.apply();
        shader_green.set_mat4("model", Mat4::translate(Vec3::new(0.75, 0.75, 0.0)));
        cube.draw();
        shader_yellow.apply();
        shader_yellow.set_mat4("model", Mat4::translate(Vec3::new(-0.75, -0.75, 0.0)));
        cube.draw();
        shader_blue.apply();
        shader_blue.set_mat4("model", Mat4::translate(Vec3::new(0.75, -0.75, 0.0)));
        cube.draw();

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
        pub fn clear(&self, red: f32, green: f32, blue: f32, alpha: f32) {
            unsafe {
                self.ClearColor(red, green, blue, alpha);
                self.Clear(self::COLOR_BUFFER_BIT | self::DEPTH_BUFFER_BIT);
            }
        }

        pub fn unbind_vao(&self) {
            unsafe {
                self.BindVertexArray(0);
            }
        }

        pub fn depth_enable(&self, on: bool) {
            if on {
                unsafe {
                    self.Enable(self::DEPTH_TEST);
                }
            } else {
                unsafe {
                    self.Disable(self::DEPTH_TEST);
                }
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

const CUBE_COUNT: usize = 24;
const CUBE_COMPONENTS: usize = 3;
const CUBE_VERTICES: [f32; CUBE_COMPONENTS * CUBE_COUNT] = [
    -0.5, -0.5, -0.5, -0.5, 0.5, -0.5, 0.5, -0.5, -0.5, 0.5, 0.5, -0.5, // back
    -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, -0.5, 0.5, 0.5, 0.5, 0.5, 0.5, // front
    -0.5, 0.5, 0.5, -0.5, 0.5, -0.5, -0.5, -0.5, 0.5, -0.5, -0.5, -0.5, // left
    0.5, 0.5, 0.5, 0.5, -0.5, 0.5, 0.5, 0.5, -0.5, 0.5, -0.5, -0.5, // right
    -0.5, -0.5, -0.5, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, // bottom
    -0.5, 0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0.5, -0.5, 0.5, 0.5, 0.5, // top
];
const INDEX_COUNT: usize = 36;
const INDICES: [u8; INDEX_COUNT] = [
    0, 1, 2, 2, 1, 3, // first quad
    4, 5, 6, 6, 5, 7, // second quad
    8, 9, 10, 10, 9, 11, // third quad
    12, 13, 14, 14, 13, 15, // fourth quad
    16, 17, 18, 18, 17, 19, // fifth quad
    20, 21, 22, 22, 21, 23, // sixth quad
];

pub struct UniformBuffer {
    gl: Rc<gl::Gl>,
    ubo: gl::types::GLuint,
}

impl UniformBuffer {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut ubo = 0;
        let buffer_size = (2 * size_of::<Mat4>()) as isize;

        unsafe {
            gl.GenBuffers(1, &mut ubo);
            gl.BindBuffer(gl::UNIFORM_BUFFER, ubo);
            gl.BufferData(gl::UNIFORM_BUFFER, buffer_size, ptr::null_mut(), gl::STATIC_DRAW);
            gl.BindBuffer(gl::UNIFORM_BUFFER, 0);
            gl.BindBufferRange(gl::UNIFORM_BUFFER, 0, ubo, 0, buffer_size);
        }

        UniformBuffer { gl, ubo }
    }

    pub fn buffer_data(&self, offset: usize, matrix: Mat4) {
        let gl = &self.gl;

        unsafe {
            gl.BindBuffer(gl::UNIFORM_BUFFER, self.ubo);
            gl.BufferSubData(gl::UNIFORM_BUFFER, offset as isize, size_of::<Mat4>() as isize, matrix.as_ptr().cast());
        }
    }
}

impl Drop for UniformBuffer {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteBuffers(1, &self.ubo);
        }
    }
}
