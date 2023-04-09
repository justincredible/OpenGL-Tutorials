use gfx_maths::{mat4::Mat4, quaternion::Quaternion, vec2::Vec2, vec3::Vec3};
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

    let shader = Program::new(Rc::clone(&gl)).link("src/7.bloom.vs", "src/7.bloom.fs");
    let shader_light = Program::new(Rc::clone(&gl)).link("src/7.bloom.vs", "src/7.light_box.fs");
    let shader_blur = Program::new(Rc::clone(&gl)).link("src/7.blur.vs", "src/7.blur.fs");
    let shader_bloom = Program::new(Rc::clone(&gl)).link("src/7.bloom_final.vs", "src/7.bloom_final.fs");

    let cube = VertexArray::new_cube(Rc::clone(&gl));
    let quad = VertexArray::new_quad(Rc::clone(&gl));

    let wood_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/wood.png", true);
    let container_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/container2.png", true);

    let bloom_fbo = Framebuffer::new_bloom(Rc::clone(&gl));
    let ping_pong = [Framebuffer::new_ping_pong(Rc::clone(&gl)), Framebuffer::new_ping_pong(Rc::clone(&gl))];

    let light_positions = vec![Vec3::new(0.0, 0.5, 1.5), Vec3::new(-4.0, 0.5, -3.0), Vec3::new(3.0, 0.5, 1.0), Vec3::new(-0.8, 2.4, -1.0)];
    let light_colors = vec![Vec3::new(5.0, 5.0, 5.0), Vec3::new(10.0, 0.0, 0.0), Vec3::new(0.0, 0.0, 15.0), Vec3::new(0.0, 5.0, 0.0)];

    let mut bloom = Bloom {
        is_on: true,
        pressed: false,
        exposure: 1.0,
    };

    shader.apply();
    shader.set_int("diffuseTexture", 0);
    shader_blur.apply();
    shader_blur.set_int("image", 0);
    shader_bloom.apply();
    shader_bloom.set_int("scene", 0);
    shader_bloom.set_int("bloomBlur", 1);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 5.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut bloom, &mut window, delta_time);

        gl.clear_color(0.0, 0.0, 0.0, 1.0);

        bloom_fbo.bind();
        gl.clear();
        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();
        shader.apply();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        gl.active_texture(0);
        wood_tex.bind();
        for i in 0..light_positions.len() {
            shader.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Position")), *light_positions.get(i).unwrap());
            shader.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Color")), *light_colors.get(i).unwrap());
        }
        shader.set_vec3("viewPos", camera.position());
        let model = Mat4::translate(Vec3::new(0.0, -1.0, 0.0)) * Mat4::scale(Vec3::new(12.5, 0.5, 12.5));
        shader.set_mat4("model", model);
        cube.bind();
        cube.draw();
        container_tex.bind();
        let model = Mat4::translate(Vec3::new(0.0, 1.5, 0.0)) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
        shader.set_mat4("model", model);
        cube.draw();
        let model = Mat4::translate(Vec3::new(2.0, 0.0, 1.0)) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
        shader.set_mat4("model", model);
        cube.draw();
        let model = Mat4::translate(Vec3::new(-1.0, -1.0, 2.0)) * Mat4::rotate(Quaternion::axis_angle(Vec3::new(1.0, 0.0, 1.0).normalized(), PI / 3.0));
        shader.set_mat4("model", model);
        cube.draw();
        let model = Mat4::translate(Vec3::new(0.0, 2.7, 4.0))
            * Mat4::rotate(Quaternion::axis_angle(Vec3::new(1.0, 0.0, 1.0).normalized(), 23.0 * PI / 180.0))
            * Mat4::scale(Vec3::new(1.25, 1.25, 1.25));
        shader.set_mat4("model", model);
        cube.draw();
        let model = Mat4::translate(Vec3::new(-2.0, 1.0, -3.0)) * Mat4::rotate(Quaternion::axis_angle(Vec3::new(1.0, 0.0, 1.0).normalized(), 31.0 * PI / 45.0));
        shader.set_mat4("model", model);
        cube.draw();
        let model = Mat4::translate(Vec3::new(-3.0, 0.0, 0.0)) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
        shader.set_mat4("model", model);
        cube.draw();

        shader_light.apply();
        shader_light.set_mat4("projection", projection);
        shader_light.set_mat4("view", view);
        for i in 0..light_positions.len() {
            let model = Mat4::translate(*light_positions.get(i).unwrap()) * Mat4::scale(Vec3::new(0.25, 0.25, 0.25));
            shader_light.set_mat4("model", model);
            shader_light.set_vec3("lightColor", *light_colors.get(i).unwrap());
            cube.draw();
        }
        gl.unbind_framebuffer();

        let mut horizontal = false;
        let amount = 10;
        shader_blur.apply();
        ping_pong[1].bind();
        shader_blur.set_int("horizontal", 1);
        bloom_fbo.bind_texture(1);
        quad.bind();
        quad.draw();
        for _ in 1..amount {
            ping_pong[horizontal as usize].bind();
            shader_blur.set_int("horizontal", horizontal as i32);
            ping_pong[!horizontal as usize].bind_texture(0);
            quad.draw();
            horizontal = !horizontal;
        }
        gl.unbind_framebuffer();

        gl.clear();
        shader_bloom.apply();
        bloom_fbo.bind_texture(0);
        gl.active_texture(1);
        ping_pong[!horizontal as usize].bind_texture(0);
        shader_bloom.set_int("bloom", bloom.is_on as i32);
        shader_bloom.set_float("exposure", bloom.exposure);
        quad.draw();

        println!("{} {} {} {}", "Bloom: ", if bloom.is_on { "on" } else { "off" }, "| exposure: ", bloom.exposure);

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut camera, &mut window, event);
        }
    }
}

fn process_input(camera: &mut Camera, bloom: &mut Bloom, window: &mut Window, delta_time: f32) {
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

    if window.get_key(Key::Space) == Action::Press && !bloom.pressed {
        bloom.is_on = !bloom.is_on;
        bloom.pressed = true;
    }
    if window.get_key(Key::Space) == Action::Release {
        bloom.pressed = false;
    }

    if window.get_key(Key::Q) == Action::Press {
        if bloom.exposure > 0.0 {
            bloom.exposure -= 0.001;
        } else {
            bloom.exposure = 0.0;
        }
    }

    if window.get_key(Key::E) == Action::Press {
        bloom.exposure += 0.001;
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

pub struct Framebuffer {
    gl: Rc<gl::Gl>,
    framebuffer: gl::types::GLuint,
    textures: [gl::types::GLuint; 2],
    rbo: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new_bloom(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut textures = [0, 0];
        let mut rbo = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(2, textures.as_mut_ptr());
            gl.GenRenderbuffers(1, &mut rbo);

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            for i in 0..textures.len() {
                gl.BindTexture(gl::TEXTURE_2D, textures[i]);
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
                let clamp_edge = gl::CLAMP_TO_EDGE as i32;
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);

                gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0 + i as u32, gl::TEXTURE_2D, textures[i], 0);
            }

            gl.BindRenderbuffer(gl::RENDERBUFFER, rbo);
            gl.RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH_COMPONENT, SCR_WIDTH as i32, SCR_HEIGHT as i32);
            gl.FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::RENDERBUFFER, rbo);

            let attachments = [gl::COLOR_ATTACHMENT0, gl::COLOR_ATTACHMENT1];
            gl.DrawBuffers(2, attachments.as_ptr());

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { gl, framebuffer, textures, rbo }
    }

    pub fn new_ping_pong(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut textures = [0, 0];

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(1, textures.as_mut_ptr());

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            gl.BindTexture(gl::TEXTURE_2D, textures[0]);
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
            let clamp_edge = gl::CLAMP_TO_EDGE as i32;
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);

            gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, textures[0], 0);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer {
            gl,
            framebuffer,
            textures,
            rbo: 0,
        }
    }

    pub fn bind(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_texture(&self, texture: usize) {
        assert!(texture == 0 || texture == 1);

        unsafe {
            self.gl.BindTexture(gl::TEXTURE_2D, self.textures[texture]);
        }
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        let gl = &self.gl;

        unsafe {
            gl.DeleteFramebuffers(1, &self.framebuffer);
            gl.DeleteTextures(2, self.textures.as_ptr());
            gl.DeleteRenderbuffers(1, &self.rbo);
        }
    }
}

pub struct Bloom {
    pub is_on: bool,
    pub pressed: bool,
    pub exposure: f32,
}
