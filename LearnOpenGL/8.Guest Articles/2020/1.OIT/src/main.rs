use glam::{Mat4, Vec3, Vec4};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::ptr;

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod camera;
use camera::camera::{Camera, Movement};
pub mod mesh;
use mesh::mesh::VertexArray;
pub mod shader;
use shader::shader::Program;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(4));
    glfw.window_hint(WindowHint::ContextVersionMinor(0));
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

    let quad = VertexArray::new_quad();

    let solid_shader = Program::new().link("src/solid.vs", "src/solid.fs");
    let transparent_shader = Program::new().link("src/transparent.vs", "src/transparent.fs");
    let composite_shader = Program::new().link("src/composite.vs", "src/composite.fs");
    let screen_shader = Program::new().link("src/screen.vs", "src/screen.fs");

    let fbo = Framebuffer::new();

    let red_model = calculate_model_matrix(Vec3::new(0.0, 0.0, 1.0), Vec3::ZERO, Vec3::ONE);
    let green_model = calculate_model_matrix(Vec3::ZERO, Vec3::ZERO, Vec3::ONE);
    let blue_model = calculate_model_matrix(Vec3::new(0.0, 0.0, 2.0), Vec3::ZERO, Vec3::ONE);

    let zero_filler = Vec4::ZERO;
    let one_filler = Vec4::ONE;

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 5.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        let projection = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, SCR_NEAR, SCR_FAR);
        let view = camera.view_matrix();
        let vp = projection * view;

        process_input(&mut camera, &mut window, delta_time);

        ogl::depth_enable(true);
        ogl::depth_always(false);
        ogl::depth_mask(true);
        ogl::blend_enable(false);
        ogl::clear_color(0.0, 0.0, 0.0, 0.0);

        fbo.bind_opaque(true);
        ogl::clear();

        solid_shader.apply();
        solid_shader.set_mat4("mvp", vp * red_model);
        solid_shader.set_vec3("color", Vec3::new(1.0, 0.0, 0.0));
        quad.bind();
        quad.draw();

        ogl::depth_mask(false);
        ogl::blend_enable(true);
        ogl::blend_equation();

        fbo.bind_opaque(false);
        ogl::clear_buffer(0, &zero_filler);
        ogl::clear_buffer(1, &one_filler);

        transparent_shader.apply();
        transparent_shader.set_mat4("mvp", vp * green_model);
        transparent_shader.set_vec4("color", Vec4::new(0.0, 1.0, 0.0, 0.5));
        quad.draw();
        transparent_shader.set_mat4("mvp", vp * blue_model);
        transparent_shader.set_vec4("color", Vec4::new(0.0, 0.0, 1.0, 0.5));
        quad.draw();

        ogl::depth_always(true);
        ogl::blend_func();

        fbo.bind_opaque(true);

        composite_shader.apply();
        ogl::active_texture(0);
        fbo.bind_texture(0);
        ogl::active_texture(1);
        fbo.bind_texture(1);
        quad.draw();

        ogl::depth_enable(false);
        ogl::depth_mask(true);
        ogl::blend_enable(false);

        ogl::unbind_framebuffer();
        ogl::clear();

        screen_shader.apply();
        ogl::active_texture(0);
        fbo.bind_texture(2);
        quad.draw();

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&mut camera, &mut window, event);
        }
    }
}

fn calculate_model_matrix(position: Vec3, rotation: Vec3, scale: Vec3) -> Mat4 {
    Mat4::from_translation(position) * Mat4::from_rotation_x(rotation.x) * Mat4::from_rotation_y(rotation.y) * Mat4::from_rotation_z(rotation.z) * Mat4::from_scale(scale)
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
    use crate::Vec4;

    pub fn clear_color(red: f32, green: f32, blue: f32, alpha: f32) {
        unsafe {
            gl::ClearColor(red, green, blue, alpha);
        }
    }

    pub fn clear() {
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT | gl::STENCIL_BUFFER_BIT);
        }
    }

    pub fn unbind_framebuffer() {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
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

    pub fn depth_always(on: bool) {
        unsafe {
            if on {
                gl::DepthFunc(gl::ALWAYS);
            } else {
                gl::DepthFunc(gl::LESS);
            }
        }
    }

    pub fn depth_mask(on: bool) {
        unsafe {
            if on {
                gl::DepthMask(gl::TRUE);
            } else {
                gl::DepthMask(gl::FALSE);
            }
        }
    }

    pub fn blend_enable(on: bool) {
        unsafe {
            if on {
                gl::Enable(gl::BLEND);
            } else {
                gl::Disable(gl::BLEND);
            }
        }
    }

    pub fn blend_equation() {
        unsafe {
            gl::BlendFunci(0, gl::ONE, gl::ONE);
            gl::BlendFunci(1, gl::ZERO, gl::ONE_MINUS_SRC_COLOR);
            gl::BlendEquation(gl::FUNC_ADD);
        }
    }

    pub fn clear_buffer(index: i32, vector: &Vec4) {
        unsafe {
            gl::ClearBufferfv(gl::COLOR, index, vector.to_array().as_ptr());
        }
    }

    pub fn blend_func() {
        unsafe {
            gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        }
    }

    pub fn active_texture(unit: u32) {
        assert!(unit <= gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            gl::ActiveTexture(gl::TEXTURE0 + unit);
        }
    }
}

pub struct Framebuffer {
    opaque_framebuf: gl::types::GLuint,
    transparent_framebuf: gl::types::GLuint,
    opaque_tex: gl::types::GLuint,
    depth_tex: gl::types::GLuint,
    accum_tex: gl::types::GLuint,
    reveal_tex: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new() -> Self {
        let mut opaque_framebuf = 0;
        let mut transparent_framebuf = 0;
        let mut opaque_tex = 0;
        let mut depth_tex = 0;
        let mut accum_tex = 0;
        let mut reveal_tex = 0;

        unsafe {
            gl::GenFramebuffers(1, &mut opaque_framebuf);
            gl::GenFramebuffers(1, &mut transparent_framebuf);
            gl::GenTextures(1, &mut opaque_tex);
            gl::GenTextures(1, &mut depth_tex);
            gl::GenTextures(1, &mut accum_tex);
            gl::GenTextures(1, &mut reveal_tex);

            gl::BindTexture(gl::TEXTURE_2D, opaque_tex);
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA16F as i32,
                SCR_WIDTH as i32,
                SCR_HEIGHT as i32,
                0,
                gl::RGBA,
                gl::HALF_FLOAT,
                ptr::null(),
            );
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::BindTexture(gl::TEXTURE_2D, 0);

            gl::BindTexture(gl::TEXTURE_2D, depth_tex);
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::DEPTH_COMPONENT as i32,
                SCR_WIDTH as i32,
                SCR_HEIGHT as i32,
                0,
                gl::DEPTH_COMPONENT,
                gl::FLOAT,
                ptr::null(),
            );
            gl::BindTexture(gl::TEXTURE_2D, 0);

            gl::BindFramebuffer(gl::FRAMEBUFFER, opaque_framebuf);
            gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, opaque_tex, 0);
            gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::TEXTURE_2D, depth_tex, 0);

            if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Opaque framebuffer is not complete!");
            }

            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);

            gl::BindTexture(gl::TEXTURE_2D, accum_tex);
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA16F as i32,
                SCR_WIDTH as i32,
                SCR_HEIGHT as i32,
                0,
                gl::RGBA,
                gl::HALF_FLOAT,
                ptr::null(),
            );
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::BindTexture(gl::TEXTURE_2D, 0);

            gl::BindTexture(gl::TEXTURE_2D, reveal_tex);
            gl::TexImage2D(gl::TEXTURE_2D, 0, gl::R8 as i32, SCR_WIDTH as i32, SCR_HEIGHT as i32, 0, gl::RED, gl::FLOAT, ptr::null());
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            gl::BindTexture(gl::TEXTURE_2D, 0);

            gl::BindFramebuffer(gl::FRAMEBUFFER, transparent_framebuf);
            gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, accum_tex, 0);
            gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT1, gl::TEXTURE_2D, reveal_tex, 0);
            gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::TEXTURE_2D, depth_tex, 0);

            let transparent_draw_buffers: [u32; 2] = [gl::COLOR_ATTACHMENT0, gl::COLOR_ATTACHMENT1];
            gl::DrawBuffers(2, transparent_draw_buffers.as_ptr());

            if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Transparent framebuffer is not complete!");
            }

            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer {
            opaque_framebuf,
            transparent_framebuf,
            opaque_tex,
            depth_tex,
            accum_tex,
            reveal_tex,
        }
    }

    pub fn bind_opaque(&self, opaque: bool) {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, if opaque { self.opaque_framebuf } else { self.transparent_framebuf });
        }
    }

    pub fn bind_texture(&self, texture: usize) {
        unsafe {
            match texture {
                0 => gl::BindTexture(gl::TEXTURE_2D, self.accum_tex),
                1 => gl::BindTexture(gl::TEXTURE_2D, self.reveal_tex),
                2 => gl::BindTexture(gl::TEXTURE_2D, self.opaque_tex),
                _ => {}
            }
        }
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteFramebuffers(1, &self.opaque_framebuf);
            gl::DeleteFramebuffers(1, &self.transparent_framebuf);
            gl::DeleteTextures(1, &self.depth_tex);
            gl::DeleteTextures(1, &self.accum_tex);
            gl::DeleteTextures(1, &self.reveal_tex);
        }
    }
}
