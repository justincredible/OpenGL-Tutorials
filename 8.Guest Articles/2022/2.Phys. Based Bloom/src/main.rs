use glam::{IVec2, Mat4, Quat, Vec2, Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, ptr};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod camera;
use camera::camera::{Camera, Movement};
pub mod mesh;
use mesh::mesh::{Texture, VertexArray};
pub mod shader;
use shader::shader::Program;

struct BloomMip {
    size: Vec2,
    int_size: IVec2,
    texture: u32,
}

impl Drop for BloomMip {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteTextures(1, &self.texture);
        }
    }
}

struct BloomFBO {
    m_init: bool,
    m_fbo: u32,
    m_mip_chain: Vec<BloomMip>,
}

impl BloomFBO {
    pub fn new() -> Self {
        let mut m_fbo = 0;

        unsafe {
            gl::GenFramebuffers(1, &mut m_fbo);
        }

        BloomFBO {
            m_init: false,
            m_fbo,
            m_mip_chain: Vec::new(),
        }
    }

    pub fn init(&mut self, window_width: i32, window_height: i32, mip_chain_length: u32) -> bool {
        if self.m_init {
            return true;
        }

        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.m_fbo);

            let mut mip_size = Vec2::new(window_width as f32, window_height as f32);
            let mut mip_int_size = IVec2::new(window_width, window_height);

            if window_width > i32::MAX || window_height > i32::MAX {
                println!("Window size conversion overflow - cannot build bloom FBO!");
                return false;
            }

            for _ in 0..mip_chain_length {
                mip_size *= 0.5;
                mip_int_size /= 2;

                let mut texture = 0;

                gl::GenTextures(1, &mut texture);
                gl::BindTexture(gl::TEXTURE_2D, texture);

                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    0,
                    gl::R11F_G11F_B10F as i32,
                    mip_int_size.x,
                    mip_int_size.y,
                    0,
                    gl::RGB,
                    gl::FLOAT,
                    ptr::null(),
                );

                let linear = gl::LINEAR as i32;
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);
                let clamp_edge = gl::CLAMP_TO_EDGE as i32;
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);

                println!("Created bloom mip {}x{}", mip_size.x, mip_size.y);
                self.m_mip_chain.push(BloomMip {
                    size: mip_size,
                    int_size: mip_int_size,
                    texture,
                });
            }

            gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, self.mip_chain()[0].texture, 0);

            let attachments = [gl::COLOR_ATTACHMENT0];
            gl::DrawBuffers(1, attachments.as_ptr());

            let status = gl::CheckFramebufferStatus(gl::FRAMEBUFFER);
            if status != gl::FRAMEBUFFER_COMPLETE {
                println!("gbuffer FBO error, status: {:x}", status);
                gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
                return false;
            }

            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        self.m_init = true;
        true
    }

    pub fn bind_for_writing(&self) {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.m_fbo);
        }
    }

    pub fn mip_chain(&self) -> &Vec<BloomMip> {
        &self.m_mip_chain
    }
}

impl Drop for BloomFBO {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteFramebuffers(1, &self.m_fbo);
        }
    }
}

struct BloomRenderer {
    m_fbo: BloomFBO,
    m_src_viewport_size: IVec2,
    m_src_viewport_size_float: Vec2,
    m_downsample_shader: Program,
    m_upsample_shader: Program,
    m_karis_average_on_downsample: bool,
}

impl BloomRenderer {
    pub fn init(window_width: i32, window_height: i32) -> Self {
        let m_src_viewport_size = IVec2::new(window_width, window_height);
        let m_src_viewport_size_float = Vec2::new(window_width as f32, window_height as f32);

        let num_bloom_mips = 6;
        let mut m_fbo = BloomFBO::new();
        let status = m_fbo.init(window_width, window_height, num_bloom_mips);
        if !status {
            panic!("Failed to initialize bloom FBO - cannot create bloom renderer!\n");
        }

        let m_downsample_shader = Program::new().link("src/6.new_downsample.vs", "src/6.new_downsample.fs");
        let m_upsample_shader = Program::new().link("src/6.new_upsample.vs", "src/6.new_upsample.fs");

        m_downsample_shader.apply();
        m_downsample_shader.set_int("srcTexture", 0);
        ogl::unuse_program();

        m_upsample_shader.apply();
        m_upsample_shader.set_int("srcTexture", 0);
        ogl::unuse_program();

        BloomRenderer {
            m_fbo,
            m_src_viewport_size,
            m_src_viewport_size_float,
            m_downsample_shader,
            m_upsample_shader,
            m_karis_average_on_downsample: true,
        }
    }
}

impl BloomRenderer {
    fn render_downsamples(&self, src_texture: u32, quad: &VertexArray) {
        let mip_chain = self.m_fbo.mip_chain();

        self.m_downsample_shader.apply();
        self.m_downsample_shader.set_vec2("srcResolution", self.m_src_viewport_size_float);
        if self.m_karis_average_on_downsample {
            self.m_downsample_shader.set_int("mipLevel", 0);
        }

        ogl::active_texture(0);
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, src_texture);

            quad.bind();
            for i in 0..mip_chain.len() {
                let mip = &mip_chain[i];
                ogl::viewport(mip.size.x as i32, mip.size.y as i32);
                gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, mip.texture, 0);

                quad.draw();

                self.m_downsample_shader.set_vec2("srcResolution", mip.size);
                gl::BindTexture(gl::TEXTURE_2D, mip.texture);
                if i == 0 {
                    self.m_downsample_shader.set_int("mipLevel", 1);
                }
            }
        }

        ogl::unuse_program();
    }

    fn render_upsamples(&self, filter_radius: f32, quad: &VertexArray) {
        let mip_chain = self.m_fbo.mip_chain();

        self.m_upsample_shader.apply();
        self.m_upsample_shader.set_float("filterRadius", filter_radius);

        unsafe {
            gl::Enable(gl::BLEND);
            gl::BlendFunc(gl::ONE, gl::ONE);
            gl::BlendEquation(gl::FUNC_ADD);

            quad.bind();
            for i in (1..mip_chain.len()).rev() {
                let mip = &mip_chain[i];
                let next_mip = &mip_chain[i - 1];

                ogl::active_texture(0);
                gl::BindTexture(gl::TEXTURE_2D, mip.texture);

                ogl::viewport(next_mip.int_size.x, next_mip.int_size.y);
                gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, next_mip.texture, 0);

                quad.draw();
            }

            gl::Disable(gl::BLEND);
        }

        ogl::unuse_program();
    }

    pub fn render_bloom_texture(&self, src_texture: u32, filter_radius: f32, quad: &VertexArray) {
        self.m_fbo.bind_for_writing();

        self.render_downsamples(src_texture, quad);
        self.render_upsamples(filter_radius, quad);

        ogl::unbind_framebuffer();

        ogl::viewport(self.m_src_viewport_size.x, self.m_src_viewport_size.y);
    }

    pub fn bloom_texture(&self) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, self.m_fbo.mip_chain()[0].texture);
        }
    }

    pub fn bloom_mip_i(&self, index: usize) -> u32 {
        let mip_chain = self.m_fbo.mip_chain();
        let size = mip_chain.len();
        mip_chain[usize::min(index, size - 1)].texture
    }
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw
        .create_window(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_framebuffer_size_polling(true);
    window.set_cursor_pos_polling(true);
    window.set_scroll_polling(true);

    window.set_cursor_mode(CursorMode::Disabled);
    window.focus();

    gl::load_with(|s| window.get_proc_address(s) as *const _);

    let quad = VertexArray::new_quad();
    let cube = VertexArray::new_cube();

    ogl::depth_enable(true);

    let shader = Program::new().link("src/6.bloom.vs", "src/6.bloom.fs");
    let shader_light = Program::new().link("src/6.bloom.vs", "src/6.light_box.fs");
    let shader_blur = Program::new().link("src/6.old_blur.vs", "src/6.old_blur.fs");
    let shader_bloom_final = Program::new().link("src/6.bloom_final.vs", "src/6.bloom_final.fs");

    let wood_texture = Texture::new_hdr("resources/textures/wood.png");
    let container_texture = Texture::new_hdr("resources/textures/container2.png");

    let hdr_fbo = Framebuffer::new_hdr();
    let mut ping_pong = PingPongFBO::new();

    let light_positions = vec![Vec3::new(0.0, 0.5, 1.5), Vec3::new(-4.0, 0.5, -3.0), Vec3::new(3.0, 0.5, 1.0), Vec3::new(-0.8, 2.4, -1.0)];
    let light_colours = vec![Vec3::new(5.0, 5.0, 5.0), Vec3::new(10.0, 0.0, 0.0), Vec3::new(0.0, 0.0, 15.0), Vec3::new(0.0, 5.0, 0.0)];

    shader.apply();
    shader.set_int("diffuseTexture", 0);
    shader_blur.apply();
    shader_blur.set_int("image", 0);
    shader_bloom_final.apply();
    shader_bloom_final.set_int("scene", 0);
    shader_bloom_final.set_int("bloomBlur", 1);

    let bloom_renderer = BloomRenderer::init(SCR_WIDTH as i32, SCR_HEIGHT as i32);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 5.0), x_pos as f32, y_pos as f32);

    let mut input_state = InputState {
        bloom: true,
        exposure: 1.0,
        program_choice: 1,
        bloom_filter_radius: 0.005,
    };

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, &mut input_state, delta_time);

        ogl::clear_color(0.0, 0.0, 0.0, 1.0);
        ogl::clear();

        hdr_fbo.bind();
        ogl::clear();

        let projection = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, SCR_NEAR, SCR_FAR);
        let view = camera.view_matrix();
        shader.apply();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        ogl::active_texture(0);
        wood_texture.bind();
        for i in 0..light_positions.len() {
            shader.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Position")), light_positions[i]);
            shader.set_vec3(&("lights[".to_string() + &(i.to_string() + "].Color")), light_colours[i]);
        }
        shader.set_vec3("viewPos", camera.position());
        let model = Mat4::from_translation(-Vec3::Y) * Mat4::from_scale(Vec3::new(12.5, 0.5, 12.5));
        shader.set_mat4("model", model);
        cube.bind();
        cube.draw();

        container_texture.bind();
        let model = Mat4::from_translation(Vec3::new(0.0, 1.5, 0.0)) * Mat4::from_scale(Vec3::splat(0.5));
        shader.set_mat4("model", model);
        cube.draw();

        let model = Mat4::from_translation(Vec3::new(2.0, 0.0, 1.0)) * Mat4::from_scale(Vec3::splat(0.5));
        shader.set_mat4("model", model);
        cube.draw();

        let model = Mat4::from_translation(Vec3::new(-1.0, -1.0, 2.0)) * Mat4::from_quat(Quat::from_axis_angle(Vec3::new(1.0, 0.0, 1.0).normalize(), PI / 3.0));
        shader.set_mat4("model", model);
        cube.draw();

        let model = Mat4::from_translation(Vec3::new(0.0, 2.7, 4.0))
            * Mat4::from_quat(Quat::from_axis_angle(Vec3::new(1.0, 0.0, 1.0).normalize(), 23.0 * PI / 180.0))
            * Mat4::from_scale(Vec3::splat(1.25));
        shader.set_mat4("model", model);
        cube.draw();

        let model = Mat4::from_translation(Vec3::new(-2.0, 1.0, -3.0)) * Mat4::from_quat(Quat::from_axis_angle(Vec3::new(1.0, 0.0, 1.0).normalize(), 31.0 * PI / 45.0));
        shader.set_mat4("model", model);
        cube.draw();

        let model = Mat4::from_translation(Vec3::new(-3.0, 0.0, 0.0)) * Mat4::from_scale(Vec3::splat(0.5));
        shader.set_mat4("model", model);
        cube.draw();

        shader_light.apply();
        shader_light.set_mat4("projection", projection);
        shader_light.set_mat4("view", view);

        for i in 0..light_positions.len() {
            let model = Mat4::from_translation(light_positions[i]) * Mat4::from_scale(Vec3::splat(0.25));
            shader_light.set_mat4("model", model);
            shader_light.set_vec3("lightColor", light_colours[i]);
            cube.draw()
        }
        ogl::unbind_framebuffer();

        if input_state.program_choice < 1 || input_state.program_choice > 3 {
            input_state.program_choice = 1;
        }
        input_state.bloom = input_state.program_choice != 1;

        if input_state.program_choice == 2 {
            shader_blur.apply();

            // first iteration
            ping_pong.bind();
            shader_blur.set_int("horizontal", ping_pong.horizontal() as i32);
            hdr_fbo.bind_texture(1);
            quad.bind();
            quad.draw();
            ping_pong.bounce();
            // remaining iterations
            for _ in 0..9 {
                ping_pong.bind();
                shader_blur.set_int("horizontal", ping_pong.horizontal() as i32);
                ping_pong.bind_texture();
                quad.draw();
                ping_pong.bounce();
            }
            ogl::unbind_framebuffer();
        } else if input_state.program_choice == 3 {
            bloom_renderer.render_bloom_texture(hdr_fbo.colour_buffers(1), input_state.bloom_filter_radius, &quad);
        }

        ogl::clear();
        shader_bloom_final.apply();
        ogl::active_texture(0);
        hdr_fbo.bind_texture(0);
        ogl::active_texture(1);
        if input_state.program_choice == 1 {
            ogl::bind_texture0();
        } else if input_state.program_choice == 2 {
            ping_pong.bind_texture();
        } else if input_state.program_choice == 3 {
            bloom_renderer.bloom_texture();
        }
        shader_bloom_final.set_int("programChoice", input_state.program_choice);
        shader_bloom_final.set_float("exposure", input_state.exposure);
        quad.bind();
        quad.draw();

        //println!("bloom: {}| exposure: {}", if bloom { "on" } else { "off" }, exposure);

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&mut camera, &mut window, event);
        }
    }
}

struct InputState {
    bloom: bool,
    exposure: f32,
    program_choice: i32,
    bloom_filter_radius: f32,
}

fn process_input(camera: &mut Camera, window: &mut Window, input_state: &mut InputState, delta_time: f32) {
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

    if window.get_key(Key::Q) == Action::Press {
        if input_state.exposure > 0.0 {
            input_state.exposure -= 0.001;
        } else {
            input_state.exposure = 0.0;
        }
    }
    if window.get_key(Key::E) == Action::Press {
        input_state.exposure += 0.001;
    }

    if window.get_key(Key::Num1) == Action::Press {
        input_state.program_choice = 1;
    } else if window.get_key(Key::Num2) == Action::Press {
        input_state.program_choice = 2;
    } else if window.get_key(Key::Num3) == Action::Press {
        input_state.program_choice = 3;
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

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
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

    pub fn active_texture(unit: u32) {
        assert!(unit < gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            gl::ActiveTexture(gl::TEXTURE0 + unit);
        }
    }

    pub fn unbind_framebuffer() {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }
    }

    pub fn unuse_program() {
        unsafe {
            gl::UseProgram(0);
        }
    }

    pub fn bind_texture0() {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, 0);
        }
    }
}

pub struct Framebuffer {
    framebuffer: u32,
    textures: [u32; 2],
    renderbuffer: u32,
}

impl Framebuffer {
    pub fn new_hdr() -> Self {
        let mut framebuffer = 0;
        let mut textures = [0, 0];
        let mut renderbuffer = 0;

        unsafe {
            gl::GenFramebuffers(1, &mut framebuffer);
            gl::GenTextures(2, textures.as_mut_ptr());
            gl::GenRenderbuffers(1, &mut renderbuffer);

            gl::BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            for i in 0..2 {
                gl::BindTexture(gl::TEXTURE_2D, textures[i]);
                gl::TexImage2D(
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
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);

                gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0 + i as u32, gl::TEXTURE_2D, textures[i], 0);
            }

            gl::BindRenderbuffer(gl::RENDERBUFFER, renderbuffer);
            gl::RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH_COMPONENT, SCR_WIDTH as i32, SCR_HEIGHT as i32);
            gl::FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::RENDERBUFFER, renderbuffer);

            let attachments = [gl::COLOR_ATTACHMENT0, gl::COLOR_ATTACHMENT1];
            gl::DrawBuffers(2, attachments.as_ptr());

            if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("Framebuffer is not complete!")
            }
            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer {
            framebuffer,
            textures,
            renderbuffer,
        }
    }

    pub fn bind(&self) {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_texture(&self, texture: usize) {
        assert!(texture < 2);

        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, self.textures[texture]);
        }
    }

    pub fn colour_buffers(&self, index: usize) -> u32 {
        self.textures[index]
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteFramebuffers(1, &self.framebuffer);
            gl::DeleteTextures(2, self.textures.as_ptr());
            gl::DeleteRenderbuffers(1, &self.renderbuffer);
        }
    }
}

pub struct PingPongFBO {
    framebuffers: [u32; 2],
    textures: [u32; 2],
    horizontal: bool,
}

impl PingPongFBO {
    pub fn new() -> Self {
        let mut framebuffers = [0, 0];
        let mut textures = [0, 0];

        unsafe {
            gl::GenFramebuffers(2, framebuffers.as_mut_ptr());
            gl::GenTextures(2, textures.as_mut_ptr());

            for i in 0..2 {
                gl::BindFramebuffer(gl::FRAMEBUFFER, framebuffers[i]);
                gl::BindTexture(gl::TEXTURE_2D, textures[i]);
                gl::TexImage2D(
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
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);

                gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0 as u32, gl::TEXTURE_2D, textures[i], 0);

                if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                    println!("Framebuffer is not complete!")
                }
            }
            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        PingPongFBO {
            framebuffers,
            textures,
            horizontal: true,
        }
    }

    pub fn bind(&self) {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.framebuffers[self.horizontal as usize]);
        }
    }

    pub fn bind_texture(&self) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, self.textures[!self.horizontal as usize]);
        }
    }

    pub fn bounce(&mut self) {
        self.horizontal = !self.horizontal;
    }

    pub fn horizontal(&self) -> bool {
        self.horizontal
    }
}

impl Drop for PingPongFBO {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteFramebuffers(2, self.framebuffers.as_ptr());
            gl::DeleteTextures(2, self.textures.as_ptr());
        }
    }
}
