use gfx_maths::{mat4::Mat4, vec2::Vec2, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, fs::File, io::Read, mem::size_of, ptr, rc::Rc};

const SCR_WIDTH: u32 = 1280;
const SCR_HEIGHT: u32 = 720;
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
    glfw.window_hint(WindowHint::Samples(Some(4)));
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
    gl.depth_lequal();

    let pbr_shader = Program::new(Rc::clone(&gl)).link("src/2.1.2.pbr.vs", "src/2.1.2.pbr.fs");
    let equirectangular_cubemap = Program::new(Rc::clone(&gl)).link("src/2.1.2.cubemap.vs", "src/2.1.2.equirectangular_to_cubemap.fs");
    let irradiance_shader = Program::new(Rc::clone(&gl)).link("src/2.1.2.cubemap.vs", "src/2.1.2.irradiance_convolution.fs");
    let background_shader = Program::new(Rc::clone(&gl)).link("src/2.1.2.background.vs", "src/2.1.2.background.fs");

    pbr_shader.apply();
    pbr_shader.set_int("irradianceMap", 0);
    pbr_shader.set_vec3("albedo", Vec3::new(0.5, 0.0, 0.0));
    pbr_shader.set_float("ao", 1.0);

    background_shader.apply();
    background_shader.set_int("environmentMap", 0);

    let cube = VertexArray::new_cube(Rc::clone(&gl));
    let sphere = VertexArray::new_sphere(Rc::clone(&gl));

    let light_positions = vec![
        Vec3::new(-10.0, 10.0, 10.0),
        Vec3::new(10.0, 10.0, 10.0),
        Vec3::new(-10.0, -10.0, 10.0),
        Vec3::new(10.0, -10.0, 10.0),
    ];
    let light_colors = vec![
        Vec3::new(300.0, 300.0, 300.0),
        Vec3::new(300.0, 300.0, 300.0),
        Vec3::new(300.0, 300.0, 300.0),
        Vec3::new(300.0, 300.0, 300.0),
    ];
    let nr_rows = 7;
    let nr_columns = 7;
    let spacing = 2.5;

    let capture_fbo = Framebuffer::new(Rc::clone(&gl));

    stbi_flip_vertical(true);
    let hdr_tex = Texture::new_hdr(Rc::clone(&gl), "resources/textures/hdr/newport_loft.hdr");

    let cube_map = Texture::new_cube(Rc::clone(&gl), 512);
    let irradiance_map = Texture::new_cube(Rc::clone(&gl), 32);

    let capture_projection = Mat4::perspective_opengl(PI / 2.0, 0.1, 10.0, 1.0);
    let capture_views = [
        look_at(Vec3::new(0.0, 0.0, 0.0), Vec3::new(1.0, 0.0, 0.0), Vec3::new(0.0, -1.0, 0.0)),
        look_at(Vec3::new(0.0, 0.0, 0.0), Vec3::new(-1.0, 0.0, 0.0), Vec3::new(0.0, -1.0, 0.0)),
        look_at(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 1.0, 0.0), Vec3::new(0.0, 0.0, 1.0)),
        look_at(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, -1.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
        look_at(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, 1.0), Vec3::new(0.0, -1.0, 0.0)),
        look_at(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0), Vec3::new(0.0, -1.0, 0.0)),
    ];

    equirectangular_cubemap.apply();
    equirectangular_cubemap.set_int("equirectangularMap", 0);
    equirectangular_cubemap.set_mat4("projection", capture_projection);
    gl.active_texture(0);
    hdr_tex.bind();

    gl.viewport(512, 512);
    capture_fbo.bind();
    cube.bind();
    for i in 0..6 {
        equirectangular_cubemap.set_mat4("view", *capture_views.get(i).unwrap());
        cube_map.attach(i as u32);
        gl.clear();

        cube.draw();
    }
    gl.unbind_framebuffer();

    capture_fbo.render_dimension(32);

    irradiance_shader.apply();
    irradiance_shader.set_int("environmentMap", 0);
    irradiance_shader.set_mat4("projection", capture_projection);
    cube_map.bind();

    gl.viewport(32, 32);
    capture_fbo.bind();
    for i in 0..6 {
        irradiance_shader.set_mat4("view", *capture_views.get(i).unwrap());
        irradiance_map.attach(i as u32);
        gl.clear();

        cube.draw();
    }
    gl.unbind_framebuffer();

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
    pbr_shader.apply();
    pbr_shader.set_mat4("projection", projection);
    background_shader.apply();
    background_shader.set_mat4("projection", projection);

    gl.viewport(SCR_WIDTH as i32, SCR_HEIGHT as i32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear_color(0.2, 0.3, 0.3, 1.0);
        gl.clear();

        pbr_shader.apply();
        let view = camera.view_matrix();
        pbr_shader.set_mat4("view", view);
        pbr_shader.set_vec3("camPos", camera.position());

        irradiance_map.bind();
        sphere.bind();
        for row in 0..nr_rows {
            pbr_shader.set_float("metallic", row as f32 / nr_rows as f32);
            for col in 0..nr_columns {
                pbr_shader.set_float("roughness", f32::min(f32::max(col as f32 / nr_columns as f32, 0.025), 1.0));

                let model = Mat4::translate(Vec3::new((col - nr_columns / 2) as f32 * spacing, (row - nr_rows / 2) as f32 * spacing, -2.0));
                pbr_shader.set_mat4("model", model);
                sphere.draw();
            }
        }

        for i in 0..light_positions.len() {
            let new_pos = *light_positions.get(i).unwrap();
            pbr_shader.set_vec3(&("lightPositions[".to_string() + &(i.to_string() + "]")), new_pos);
            pbr_shader.set_vec3(&("lightColors[".to_string() + &(i.to_string() + "]")), *light_colors.get(i).unwrap());

            let model = Mat4::translate(new_pos) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
            pbr_shader.set_mat4("model", model);
            sphere.draw();
        }

        background_shader.apply();
        background_shader.set_mat4("view", view);
        cube_map.bind();
        cube.bind();
        cube.draw();

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut camera, &mut window, event);
        }
    }
}

fn look_at(eye: Vec3, center: Vec3, up: Vec3) -> Mat4 {
    let mut f = center - eye;
    f.normalize();
    let up = up.normalized();
    let mut s = f.cross(up);
    s.normalize();
    let mut u = s.cross(f);
    u.normalize(); // we're not working with the reals here

    Mat4::from([s.x, u.x, f.x, 0.0, s.y, u.y, f.y, 0.0, s.z, u.z, f.z, 0.0, -s.dot(eye), -u.dot(eye), -f.dot(eye), 1.0])
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

        pub fn depth_enable(&self, on: bool) {
            unsafe {
                if on {
                    self.Enable(self::DEPTH_TEST);
                } else {
                    self.Disable(self::DEPTH_TEST);
                }
            }
        }

        pub fn depth_lequal(&self) {
            unsafe {
                self.DepthFunc(self::LEQUAL);
            }
        }

        pub fn active_texture(&self, unit: u32) {
            assert!(unit <= self::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

            unsafe {
                self.ActiveTexture(self::TEXTURE0 + unit);
            }
        }

        pub fn viewport(&self, width: i32, height: i32) {
            unsafe {
                self.Viewport(0, 0, width, height);
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
    renderbuffer: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut renderbuffer = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenRenderbuffers(1, &mut renderbuffer);

            gl.BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

            gl.BindRenderbuffer(gl::RENDERBUFFER, renderbuffer);
            gl.RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH_COMPONENT24, 512, 512);
            gl.FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, gl::RENDERBUFFER, renderbuffer);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { gl, framebuffer, renderbuffer }
    }

    pub fn bind(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn render_dimension(&self, side: i32) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);

            self.gl.BindRenderbuffer(gl::RENDERBUFFER, self.renderbuffer);
            self.gl.RenderbufferStorage(gl::RENDERBUFFER, gl::DEPTH_COMPONENT24, side, side);

            self.gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteFramebuffers(1, &self.framebuffer);
            self.gl.DeleteRenderbuffers(1, &self.renderbuffer);
        }
    }
}
