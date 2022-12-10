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
use mesh::mesh::{stbi_flip_vertical, Mesh, Texture, Vertex, VertexArray};
pub mod model;
use model::model::Model;
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

    stbi_flip_vertical(true);

    let gl = Rc::new(gl::Gl::load_with(|s| window.get_proc_address(s).cast()));

    gl.depth_enable(true);

    let shader_geometry = Program::new(Rc::clone(&gl)).link("src/8.2.g_buffer.vs", "src/8.2.g_buffer.fs");
    let shader_lighting = Program::new(Rc::clone(&gl)).link("src/8.2.deferred_shading.vs", "src/8.2.deferred_shading.fs");
    let shader_light_box = Program::new(Rc::clone(&gl)).link("src/8.2.deferred_light_box.vs", "src/8.2.deferred_light_box.fs");

    let backpack = Model::new(Rc::clone(&gl)).load_model("resources/objects/backpack/backpack.obj");
    let object_positions = vec![
        Vec3::new(-3.0, -0.5, -3.0),
        Vec3::new(0.0, -0.5, -3.0),
        Vec3::new(3.0, -0.5, -3.0),
        Vec3::new(-3.0, -0.5, 0.0),
        Vec3::new(0.0, -0.5, 0.0),
        Vec3::new(3.0, -0.5, 0.0),
        Vec3::new(-3.0, -0.5, 3.0),
        Vec3::new(0.0, -0.5, 3.0),
        Vec3::new(3.0, -0.5, 3.0),
    ];

    let cube = VertexArray::new_cube(Rc::clone(&gl));
    let quad = VertexArray::new_quad(Rc::clone(&gl));

    let g_buffer = Framebuffer::new_g_buffer(Rc::clone(&gl));

    let nr_lights = 32usize;
    let mut light_positions = vec![];
    let mut light_colors = vec![];
    for _ in 0..nr_lights {
        let x = ((fastrand::u32(..) % 100) as f32 / 100.0) * 6.0 - 3.0;
        let y = ((fastrand::u32(..) % 100) as f32 / 100.0) * 6.0 - 4.0;
        let z = ((fastrand::u32(..) % 100) as f32 / 100.0) * 6.0 - 3.0;
        light_positions.push(Vec3::new(x, y, z));
        let r = ((fastrand::u32(..) % 100) as f32 / 200.0) + 0.5;
        let g = ((fastrand::u32(..) % 100) as f32 / 200.0) + 0.5;
        let b = ((fastrand::u32(..) % 100) as f32 / 200.0) + 0.5;
        light_colors.push(Vec3::new(r, g, b));
    }

    shader_lighting.apply();
    shader_lighting.set_int("gPosition", 0);
    shader_lighting.set_int("gNormal", 1);
    shader_lighting.set_int("gAlbedoSpec", 2);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 5.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear_color(0.0, 0.0, 0.0, 1.0);

        g_buffer.bind();
        gl.clear();
        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();
        shader_geometry.apply();
        shader_geometry.set_mat4("projection", projection);
        shader_geometry.set_mat4("view", view);
        for position in &object_positions {
            let model = Mat4::translate(*position) * Mat4::scale(Vec3::new(0.25, 0.25, 0.25));
            shader_geometry.set_mat4("model", model);
            backpack.draw(&shader_geometry);
        }
        gl.unbind_framebuffer();

        gl.clear();
        shader_lighting.apply();
        gl.active_texture(0);
        g_buffer.bind_texture(0);
        gl.active_texture(1);
        g_buffer.bind_texture(1);
        gl.active_texture(2);
        g_buffer.bind_texture(2);
        for i in 0..nr_lights {
            shader_lighting.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Position")), *light_positions.get(i).unwrap());
            shader_lighting.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Color")), *light_colors.get(i).unwrap());

            let linear = 0.7;
            let quadratic = 1.8;
            shader_lighting.set_float(&("lights[".to_string() + &(i.to_string() + "].Linear")), linear);
            shader_lighting.set_float(&("lights[".to_string() + &(i.to_string() + "].Quadratic")), quadratic);

            let light = light_colors.get(i).unwrap();
            let max_brightness = f32::max(f32::max(light.x, light.y), light.z);
            let radius = (-linear + f32::sqrt(linear * linear - 4.0 * quadratic * (1.0 - (256.0 / 5.0) * max_brightness))) / (2.0 * quadratic);
            shader_lighting.set_float(&("lights[".to_string() + &(i.to_string() + "].Radius")), radius);
        }
        shader_lighting.set_vec3("viewPos", camera.position());
        quad.bind();
        quad.draw();

        g_buffer.bind_read();
        gl.unbind_draw_framebuffer();
        gl.blit_framebuffer(SCR_WIDTH as i32, SCR_HEIGHT as i32);
        gl.unbind_framebuffer();

        shader_light_box.apply();
        shader_light_box.set_mat4("projection", projection);
        shader_light_box.set_mat4("view", view);
        cube.bind();
        for i in 0..light_positions.len() {
            let model = Mat4::translate(*light_positions.get(i).unwrap()) * Mat4::scale(Vec3::new(0.125, 0.125, 0.125));
            shader_light_box.set_mat4("model", model);
            shader_light_box.set_vec3("lightColor", *light_colors.get(i).unwrap());
            cube.draw();
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

        pub fn unbind_draw_framebuffer(&self) {
            unsafe {
                self.BindFramebuffer(self::DRAW_FRAMEBUFFER, 0);
            }
        }

        pub fn blit_framebuffer(&self, width: i32, height: i32) {
            unsafe {
                self.BlitFramebuffer(0, 0, width, height, 0, 0, width, height, self::DEPTH_BUFFER_BIT, self::NEAREST);
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

const FB_MAX_TEXAS: usize = 3usize;

pub struct Framebuffer {
    gl: Rc<gl::Gl>,
    framebuffer: gl::types::GLuint,
    textures: [gl::types::GLuint; FB_MAX_TEXAS],
    rbo: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new_g_buffer(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut textures = [0, 0, 0];
        let mut rbo = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(FB_MAX_TEXAS as i32, textures.as_mut_ptr());
            gl.GenRenderbuffers(1, &mut rbo);

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            for i in 0..FB_MAX_TEXAS - 1 {
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
                let nearest = gl::NEAREST as i32;
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, nearest);
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, nearest);

                gl.FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0 + i as u32, gl::TEXTURE_2D, textures[i], 0);
            }

            gl.BindTexture(gl::TEXTURE_2D, textures[FB_MAX_TEXAS - 1]);
            gl.TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA as i32,
                SCR_WIDTH as i32,
                SCR_HEIGHT as i32,
                0,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                ptr::null(),
            );
            let nearest = gl::NEAREST as i32;
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, nearest);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, nearest);

            gl.FramebufferTexture2D(
                gl::FRAMEBUFFER,
                gl::COLOR_ATTACHMENT0 + (FB_MAX_TEXAS - 1) as u32,
                gl::TEXTURE_2D,
                textures[FB_MAX_TEXAS - 1],
                0,
            );

            let attachments = [gl::COLOR_ATTACHMENT0, gl::COLOR_ATTACHMENT1, gl::COLOR_ATTACHMENT2];
            gl.DrawBuffers(FB_MAX_TEXAS as i32, attachments.as_ptr());

            gl.BindRenderbuffer(gl::RENDERBUFFER, rbo);
            gl.RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH_COMPONENT, SCR_WIDTH as i32, SCR_HEIGHT as i32);
            gl.FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::RENDERBUFFER, rbo);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { gl, framebuffer, textures, rbo }
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

    pub fn bind_texture(&self, texture: usize) {
        assert!(texture < FB_MAX_TEXAS);

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
