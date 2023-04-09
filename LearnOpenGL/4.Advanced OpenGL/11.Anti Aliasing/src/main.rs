use gfx_maths::{mat4::Mat4, vec2::Vec2, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, fs::File, io::Read, mem::size_of, ptr, rc::Rc};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 1000.0;

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

    let shader = Program::new(Rc::clone(&gl)).link("src/11.2.anti_aliasing.vs", "src/11.2.anti_aliasing.fs", None);
    let screen_shader = Program::new(Rc::clone(&gl)).link("src/11.2.aa_post.vs", "src/11.2.aa_post.fs", None);

    let cube = VertexArray::new_3d(Rc::clone(&gl), Vec::from(&CUBE_VERTICES[..]), Vec::from(&INDICES[..]));
    let quad = VertexArray::new_2d_tex(Rc::clone(&gl), Vec::from(&QUAD_VERTICES[..]), Vec::from(&INDICES[..6]));

    let framebuffer = Framebuffer::new_msaa(Rc::clone(&gl));
    let intermediate = Framebuffer::new_intermediate(Rc::clone(&gl));

    screen_shader.apply();
    screen_shader.set_int("screenTexture", 0);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        framebuffer.bind();
        gl.clear(0.1, 0.1, 0.1, 1.0);
        gl.depth_enable(true);

        shader.apply();
        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        shader.set_mat4("model", Mat4::identity());

        cube.bind();
        cube.draw();

        framebuffer.bind_read();
        intermediate.bind_draw();
        gl.blit_framebuffer();

        gl.unbind_framebuffer();
        gl.clear(1.0, 1.0, 1.0, 1.0);
        gl.depth_enable(false);

        screen_shader.apply();
        // GL_TEXTURE0 is active by default
        intermediate.bind_texture();
        quad.bind();
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

        pub fn blit_framebuffer(&self) {
            use crate::{SCR_HEIGHT, SCR_WIDTH};

            unsafe {
                self.BlitFramebuffer(
                    0,
                    0,
                    SCR_WIDTH as i32,
                    SCR_HEIGHT as i32,
                    0,
                    0,
                    SCR_WIDTH as i32,
                    SCR_HEIGHT as i32,
                    self::COLOR_BUFFER_BIT,
                    self::NEAREST,
                );
            }
        }

        pub fn unbind_framebuffer(&self) {
            unsafe {
                self.BindFramebuffer(self::FRAMEBUFFER, 0);
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
const QUAD_COUNT: usize = 4;
const QUAD_COMPONENTS: usize = 4;
const QUAD_VERTICES: [f32; QUAD_COMPONENTS * QUAD_COUNT] = [-1.0, -1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 0.0, -1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0];
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
    pub fn new_msaa(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut texture = 0;
        let mut rbo = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(1, &mut texture);
            gl.GenRenderbuffers(1, &mut rbo);

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            gl.BindTexture(gl::TEXTURE_2D_MULTISAMPLE, texture);
            gl.TexImage2DMultisample(gl::TEXTURE_2D_MULTISAMPLE, 4, gl::RGB, SCR_WIDTH as i32, SCR_HEIGHT as i32, gl::TRUE);
            gl.BindTexture(gl::TEXTURE_2D_MULTISAMPLE, 0);
            gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D_MULTISAMPLE, texture, 0);

            gl.BindRenderbuffer(gl::RENDERBUFFER, rbo);
            gl.RenderbufferStorageMultisample(gl::RENDERBUFFER, 4, gl::DEPTH24_STENCIL8, SCR_WIDTH as i32, SCR_HEIGHT as i32);
            gl.BindRenderbuffer(gl::RENDERBUFFER, 0);
            gl.FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_STENCIL_ATTACHMENT, gl::RENDERBUFFER, rbo);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { gl, framebuffer, texture, rbo }
    }

    pub fn new_intermediate(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut texture = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(1, &mut texture);

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            gl.BindTexture(gl::TEXTURE_2D, texture);
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
            gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, texture, 0);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Intermediate framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { gl, framebuffer, texture, rbo: 0 }
    }

    pub fn bind(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_read(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::READ_FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_draw(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);
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
