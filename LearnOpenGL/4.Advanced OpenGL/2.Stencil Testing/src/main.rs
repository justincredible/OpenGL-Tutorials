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
use mesh::mesh::{stbi_flip_vertical, Texture, VertexArray};
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

    stbi_flip_vertical(true);

    gl.depth_enable(true);
    gl.stencil_enable();
    gl.stencil_op();
    gl.stencil_func(gl::NOTEQUAL, 1, 0xFF);

    let shader = Program::new(Rc::clone(&gl)).link("src/2.stencil_testing.vs", "src/2.stencil_testing.fs");
    let shader_single_color = Program::new(Rc::clone(&gl)).link("src/2.stencil_testing.vs", "src/2.stencil_single_color.fs");
    let cube = VertexArray::new_shape(Rc::clone(&gl), Vec::from(&CUBE_VERTICES[..]), Vec::from(&INDICES[..]));
    let plane = VertexArray::new_shape(Rc::clone(&gl), Vec::from(&PLANE_VERTICES[..]), Vec::from(&INDICES[..6]));

    let cube_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/marble.jpg");
    let floor_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/metal.png");

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    shader.apply();
    shader.set_int("texture1", 0);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear(0.1, 0.1, 0.1, 1.0);

        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();

        shader_single_color.apply();
        shader_single_color.set_mat4("projection", projection);
        shader_single_color.set_mat4("view", view);

        shader.apply();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);

        gl.stencil_mask(0x00);

        plane.bind();
        floor_tex.bind_active(gl::TEXTURE0);
        shader.set_mat4("model", Mat4::identity());
        plane.draw();
        gl.unbind_vao();

        gl.stencil_func(gl::ALWAYS, 1, 0xFF);
        gl.stencil_mask(0xFF);

        cube.bind();
        cube_tex.bind_active(gl::TEXTURE0);
        shader.set_mat4("model", Mat4::translate(Vec3::new(-1.0, 0.0, -1.0)));
        cube.draw();
        shader.set_mat4("model", Mat4::translate(Vec3::new(2.0, 0.0, 0.0)));
        cube.draw();

        gl.stencil_func(gl::NOTEQUAL, 1, 0xFF);
        gl.stencil_mask(0x00);
        gl.depth_enable(false);

        let scale = Mat4::scale(Vec3::new(1.1, 1.1, 1.1));

        shader_single_color.apply();
        shader_single_color.set_mat4("model", Mat4::translate(Vec3::new(-1.0, 0.0, -1.0)) * scale);
        cube.draw();
        shader_single_color.set_mat4("model", Mat4::translate(Vec3::new(2.0, 0.0, 0.0)) * scale);
        cube.draw();

        gl.unbind_vao();
        gl.stencil_mask(0xFF);
        gl.stencil_func(gl::ALWAYS, 0, 0xFF);
        gl.depth_enable(true);

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
                self.Clear(self::COLOR_BUFFER_BIT | self::DEPTH_BUFFER_BIT | self::STENCIL_BUFFER_BIT);
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

        pub fn stencil_enable(&self) {
            unsafe {
                self.Enable(self::STENCIL_TEST);
            }
        }

        pub fn stencil_func(&self, func: self::types::GLenum, ref_val: self::types::GLint, mask: self::types::GLuint) {
            unsafe {
                self.StencilFunc(func, ref_val, mask);
            }
        }

        pub fn stencil_op(&self) {
            unsafe {
                self.StencilOp(self::KEEP, self::KEEP, self::REPLACE);
            }
        }

        pub fn stencil_mask(&self, mask: self::types::GLuint) {
            unsafe {
                self.StencilMask(mask);
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
const CUBE_COMPONENTS: usize = 5;
const CUBE_VERTICES: [f32; CUBE_COMPONENTS * CUBE_COUNT] = [
    -0.5, -0.5, -0.5, 0.0, 0.0, 0.5, -0.5, -0.5, 1.0, 0.0, -0.5, 0.5, -0.5, 0.0, 1.0, 0.5, 0.5, -0.5, 1.0, 1.0, // front face
    -0.5, -0.5, 0.5, 0.0, 0.0, -0.5, 0.5, 0.5, 0.0, 1.0, 0.5, -0.5, 0.5, 1.0, 0.0, 0.5, 0.5, 0.5, 1.0, 1.0, // back face
    -0.5, 0.5, 0.5, 1.0, 0.0, -0.5, -0.5, 0.5, 0.0, 0.0, -0.5, 0.5, -0.5, 1.0, 1.0, -0.5, -0.5, -0.5, 0.0, 1.0, // left face
    0.5, 0.5, 0.5, 1.0, 0.0, 0.5, 0.5, -0.5, 1.0, 1.0, 0.5, -0.5, 0.5, 0.0, 0.0, 0.5, -0.5, -0.5, 0.0, 1.0, // right face
    -0.5, -0.5, -0.5, 0.0, 1.0, -0.5, -0.5, 0.5, 0.0, 0.0, 0.5, -0.5, -0.5, 1.0, 1.0, 0.5, -0.5, 0.5, 1.0, 0.0, // bottom face
    -0.5, 0.5, -0.5, 0.0, 1.0, 0.5, 0.5, -0.5, 1.0, 1.0, -0.5, 0.5, 0.5, 0.0, 0.0, 0.5, 0.5, 0.5, 1.0, 0.0, // top face
];
const PLANE_COUNT: usize = 4;
const PLANE_COMPONENTS: usize = CUBE_COMPONENTS;
const PLANE_VERTICES: [f32; PLANE_COMPONENTS * PLANE_COUNT] = [-5.0, -0.5, -5.0, 0.0, 0.0, 5.0, -0.5, -5.0, 2.0, 0.0, -5.0, -0.5, 5.0, 0.0, 2.0, 5.0, -0.5, 5.0, 2.0, 2.0];
const INDEX_COUNT: usize = 36;
const INDICES: [u8; INDEX_COUNT] = [
    0, 1, 2, 2, 1, 3, // first quad
    4, 5, 6, 6, 5, 7, // second quad
    8, 9, 10, 10, 9, 11, // third quad
    12, 13, 14, 14, 13, 15, // fourth quad
    16, 17, 18, 18, 17, 19, // fifth quad
    20, 21, 22, 22, 21, 23, // sixth quad
];
