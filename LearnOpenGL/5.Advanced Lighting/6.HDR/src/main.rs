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
use mesh::mesh::{Texture, VertexArray};
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

    let shader = Program::new(Rc::clone(&gl)).link("src/6.lighting.vs", "src/6.lighting.fs");
    let hdr_shader = Program::new(Rc::clone(&gl)).link("src/6.hdr.vs", "src/6.hdr.fs");

    let cube = VertexArray::new_cube(Rc::clone(&gl));
    let quad = VertexArray::new_quad(Rc::clone(&gl));

    let wood_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/wood.png", true);

    let framebuffer = Framebuffer::new(Rc::clone(&gl));

    let light_positions = vec![Vec3::new(0.0, 0.0, 49.5), Vec3::new(-1.4, -1.9, 9.0), Vec3::new(0.0, -1.8, 4.0), Vec3::new(0.8, -1.7, 6.0)];
    let light_colors = vec![Vec3::new(200.0, 200.0, 200.0), Vec3::new(0.1, 0.0, 0.0), Vec3::new(0.0, 0.0, 0.2), Vec3::new(0.0, 0.1, 0.0)];

    let mut hdr = HDR {
        is_on: true,
        pressed: false,
        exposure: 1.0,
    };

    shader.apply();
    shader.set_int("diffuseTexture", 0);
    hdr_shader.apply();
    hdr_shader.set_int("hdrBuffer", 0);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 5.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut hdr, &mut window, delta_time);

        gl.color_clear(0.1, 0.1, 0.1, 1.0);

        framebuffer.bind();
        gl.clear();
        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();
        shader.apply();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        wood_tex.bind_active(gl::TEXTURE0);
        for i in 0..light_positions.len() {
            shader.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Position")), *light_positions.get(i).unwrap());
            shader.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Color")), *light_colors.get(i).unwrap());
        }
        shader.set_vec3("viewPos", camera.position());
        let model = Mat4::translate(Vec3::new(0.0, 0.0, 25.0)) * Mat4::scale(Vec3::new(2.5, 2.5, 27.5));
        shader.set_mat4("model", model);
        shader.set_int("inverse_normals", true as i32);
        cube.bind();
        cube.draw();
        gl.unbind_framebuffer();

        gl.clear();
        hdr_shader.apply();
        framebuffer.bind_texture(0);
        hdr_shader.set_int("hdr", hdr.is_on as i32);
        hdr_shader.set_float("exposure", hdr.exposure);
        quad.bind();
        quad.draw();

        println!("{} {} {} {}", "hdr: ", if hdr.is_on { "on" } else { "off" }, "| exposure: ", hdr.exposure);

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut camera, &mut window, event);
        }
    }
}

fn process_input(camera: &mut Camera, hdr: &mut HDR, window: &mut Window, delta_time: f32) {
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

    if window.get_key(Key::Space) == Action::Press && !hdr.pressed {
        hdr.is_on = !hdr.is_on;
        hdr.pressed = true;
    }
    if window.get_key(Key::Space) == Action::Release {
        hdr.pressed = false;
    }

    if window.get_key(Key::Q) == Action::Press {
        if hdr.exposure > 0.0 {
            hdr.exposure -= 0.001;
        } else {
            hdr.exposure = 0.0;
        }
    }

    if window.get_key(Key::E) == Action::Press {
        hdr.exposure += 0.001;
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
        pub fn color_clear(&self, red: f32, green: f32, blue: f32, alpha: f32) {
            unsafe {
                self.ClearColor(red, green, blue, alpha);
                self.Clear(self::COLOR_BUFFER_BIT | self::DEPTH_BUFFER_BIT);
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

            gl.BindTexture(gl::TEXTURE_2D, texture);
            gl.TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA16F as i32,
                SCR_WIDTH as i32,
                SCR_HEIGHT as i32,
                0,
                gl::RGBA,
                gl::FLOAT,
                ptr::null(),
            );
            let linear = gl::LINEAR as i32;
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);

            gl.BindRenderbuffer(gl::RENDERBUFFER, rbo);
            gl.RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH_COMPONENT, SCR_WIDTH as i32, SCR_HEIGHT as i32);

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);
            gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, texture, 0);
            gl.FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::RENDERBUFFER, rbo);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { gl, framebuffer, texture, rbo }
    }

    pub fn bind(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_texture(&self, unit: u32) {
        unsafe {
            self.gl.ActiveTexture(gl::TEXTURE0 + unit);
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

pub struct HDR {
    pub is_on: bool,
    pub pressed: bool,
    pub exposure: f32,
}
