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

    let shader = Program::new(Rc::clone(&gl)).link("src/5.1.framebuffers.vs", "src/5.1.framebuffers.fs");
    let screen_shader = Program::new(Rc::clone(&gl)).link("src/5.1.framebuffers_screen.vs", "src/5.1.framebuffers_screen.fs");

    let cube = VertexArray::new_shape(Rc::clone(&gl), Vec::from(&CUBE_VERTICES[..]), Vec::from(&INDICES[..]));
    let plane = VertexArray::new_shape(Rc::clone(&gl), Vec::from(&PLANE_VERTICES[..]), Vec::from(&INDICES[..6]));
    let quad = VertexArray::new_shape(Rc::clone(&gl), Vec::from(&QUAD_VERTICES[..]), Vec::from(&INDICES[..6]));

    // the texture used for this tutorial does not appear in the github source
    let cube_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/container.jpg");
    let floor_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/metal.png");

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    shader.apply();
    shader.set_int("texture1", 0);

    screen_shader.apply();
    screen_shader.set_int("screenTexture", 0);

    let framebuffer = Framebuffer::new(Rc::clone(&gl)).complete();

    //unsafe { gl.PolygonMode(gl::FRONT_AND_BACK, gl::LINE); }

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        framebuffer.bind();
        gl.depth_enable(true);

        gl.clear(0.1, 0.1, 0.1, 1.0);

        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();

        shader.apply();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);

        cube.bind();
        cube_tex.bind_active(gl::TEXTURE0);
        shader.set_mat4("model", Mat4::translate(Vec3::new(-1.0, 0.0, -1.0)));
        cube.draw();
        shader.set_mat4("model", Mat4::translate(Vec3::new(2.0, 0.0, 0.0)));
        cube.draw();

        plane.bind();
        floor_tex.bind_active(gl::TEXTURE0);
        shader.set_mat4("model", Mat4::identity());
        plane.draw();
        gl.unbind_vao();

        gl.unbind_framebuffer();
        gl.depth_enable(false);
        gl.clear(1.0, 1.0, 1.0, 1.0);

        screen_shader.apply();
        quad.bind();
        framebuffer.bind_texture();
        quad.draw();

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

        pub fn unbind_framebuffer(&self) {
            unsafe {
                self.BindFramebuffer(self::FRAMEBUFFER, 0);
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
const CUBE_COMPONENTS: usize = 5;
const CUBE_VERTICES: [f32; CUBE_COMPONENTS * CUBE_COUNT] = [
    -0.5, -0.5, 0.5, 0.0, 0.0, 0.5, -0.5, 0.5, 1.0, 0.0, -0.5, 0.5, 0.5, 0.0, 1.0, 0.5, 0.5, 0.5, 1.0, 1.0, // front face
    -0.5, -0.5, -0.5, 0.0, 0.0, -0.5, 0.5, -0.5, 0.0, 1.0, 0.5, -0.5, -0.5, 1.0, 0.0, 0.5, 0.5, -0.5, 1.0, 1.0, // back face
    -0.5, 0.5, -0.5, 1.0, 0.0, -0.5, -0.5, -0.5, 0.0, 0.0, -0.5, 0.5, 0.5, 1.0, 1.0, -0.5, -0.5, 0.5, 0.0, 1.0, // left face
    0.5, 0.5, -0.5, 1.0, 0.0, 0.5, 0.5, 0.5, 1.0, 1.0, 0.5, -0.5, -0.5, 0.0, 0.0, 0.5, -0.5, 0.5, 0.0, 1.0, // right face
    -0.5, -0.5, 0.5, 0.0, 1.0, -0.5, -0.5, -0.5, 0.0, 0.0, 0.5, -0.5, 0.5, 1.0, 1.0, 0.5, -0.5, -0.5, 1.0, 0.0, // bottom face
    -0.5, 0.5, 0.5, 0.0, 1.0, 0.5, 0.5, 0.5, 1.0, 1.0, -0.5, 0.5, -0.5, 0.0, 0.0, 0.5, 0.5, -0.5, 1.0, 0.0, // top face
];
const PLANE_COUNT: usize = 4;
const PLANE_COMPONENTS: usize = CUBE_COMPONENTS;
const PLANE_VERTICES: [f32; PLANE_COMPONENTS * PLANE_COUNT] = [-5.0, -0.5, -5.0, 0.0, 0.0, 5.0, -0.5, -5.0, 2.0, 0.0, -5.0, -0.5, 5.0, 0.0, 2.0, 5.0, -0.5, 5.0, 2.0, 2.0];
const QUAD_COUNT: usize = 4;
const QUAD_COMPONENTS: usize = CUBE_COMPONENTS;
const QUAD_VERTICES: [f32; QUAD_COMPONENTS * QUAD_COUNT] = [-1.0, -1.0, 0.0, 0.0, 0.0, 1.0, -1.0, 0.0, 1.0, 0.0, -1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0];
const INDEX_COUNT: usize = 36;
const INDICES: [u8; INDEX_COUNT] = [
    0, 1, 2, 2, 1, 3, // first quad
    4, 5, 6, 6, 5, 7, // second quad
    8, 9, 10, 10, 9, 11, // third quad
    12, 13, 14, 14, 13, 15, // fourth quad
    16, 17, 18, 18, 17, 19, // fifth quad
    20, 21, 22, 22, 21, 23, // sixth quad
];

pub struct Framebuffer {
    gl: Rc<gl::Gl>,
    framebuffer: gl::types::GLuint,
    texture: gl::types::GLuint,
    rbo: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut texture = 0;
        let mut rbo = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(1, &mut texture);
            gl.GenRenderbuffers(1, &mut rbo);
        }

        Framebuffer { gl, framebuffer, texture, rbo }
    }

    pub fn complete(self) -> Self {
        let gl = &self.gl;

        unsafe {
            gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);

            gl.BindTexture(gl::TEXTURE_2D, self.texture);
            gl.TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGB as i32,
                SCR_WIDTH as i32,
                SCR_HEIGHT as i32,
                0,
                gl::RGB,
                gl::UNSIGNED_BYTE,
                ptr::null(),
            );
            let linear = gl::LINEAR as i32;
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);
            gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, self.texture, 0);

            gl.BindRenderbuffer(gl::RENDERBUFFER, self.rbo);
            gl.RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH24_STENCIL8, SCR_WIDTH as i32, SCR_HEIGHT as i32);
            gl.FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_STENCIL_ATTACHMENT, gl::RENDERBUFFER, self.rbo);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        self
    }

    pub fn bind(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_texture(&self) {
        unsafe {
            self.gl.BindTexture(gl::TEXTURE_2D, self.texture);
        }
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        let gl = &self.gl;

        unsafe {
            gl.DeleteFramebuffers(1, &self.framebuffer);
            gl.DeleteTextures(1, &self.texture);
            gl.DeleteRenderbuffers(1, &self.rbo);
        }
    }
}
