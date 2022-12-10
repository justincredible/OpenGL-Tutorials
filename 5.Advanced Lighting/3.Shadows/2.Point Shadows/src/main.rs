use gfx_maths::{mat4::Mat4, quaternion::Quaternion, vec2::Vec2, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, fs::File, io::Read, mem::size_of, ptr, rc::Rc};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;
const SHADOW_WIDTH: u32 = 1024;
const SHADOW_HEIGHT: u32 = 1024;

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
    gl.cull_enable(true);

    let shader = Program::new(Rc::clone(&gl)).link("src/3.2.2.point_shadows.vs", "src/3.2.2.point_shadows.fs", None);
    let depth_shader = Program::new(Rc::clone(&gl)).link(
        "src/3.2.2.point_shadows_depth.vs",
        "src/3.2.2.point_shadows_depth.fs",
        Some("src/3.2.2.point_shadows_depth.gs"),
    );

    let cube = VertexArray::new_shape(Rc::clone(&gl), Vec::from(&CUBE_VERTICES[..]), Vec::from(&INDICES[..]));

    let wood_tex = Texture::new(Rc::clone(&gl), "", "").load("resources/textures/wood.png", false);

    let depth_map = Framebuffer::new(Rc::clone(&gl)).complete();

    shader.apply();
    shader.set_int("diffuseTexture", 0);
    shader.set_int("depthMap", 1);

    let mut light_pos = Vec3::new(-2.0, 4.0, -1.0);

    let mut shadows = Shadows { is_on: true, pressed: false };

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut shadows, &mut window, delta_time);

        light_pos.z = f32::sin(current_frame * 0.5) * 3.0;

        gl.color_clear(0.1, 0.1, 0.1, 1.0);

        let near_plane = 1.0;
        let far_plane = 25.0;
        let shadow_proj = Mat4::perspective_opengl(PI / 2.0, near_plane, far_plane, SHADOW_WIDTH as f32 / SHADOW_HEIGHT as f32);
        let shadow_transforms = vec![
            shadow_proj * look_at(light_pos, light_pos + Vec3::new(1.0, 0.0, 0.0), Vec3::new(0.0, -1.0, 0.0)),
            shadow_proj * look_at(light_pos, light_pos + Vec3::new(-1.0, 0.0, 0.0), Vec3::new(0.0, -1.0, 0.0)),
            shadow_proj * look_at(light_pos, light_pos + Vec3::new(0.0, 1.0, 0.0), Vec3::new(0.0, 0.0, 1.0)),
            shadow_proj * look_at(light_pos, light_pos + Vec3::new(0.0, -1.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
            shadow_proj * look_at(light_pos, light_pos + Vec3::new(0.0, 0.0, 1.0), Vec3::new(0.0, -1.0, 0.0)),
            shadow_proj * look_at(light_pos, light_pos + Vec3::new(0.0, 0.0, -1.0), Vec3::new(0.0, -1.0, 0.0)),
        ];

        gl.viewport(SHADOW_WIDTH, SHADOW_HEIGHT);
        depth_map.bind();
        gl.clear();
        depth_shader.apply();
        let mut i = 0;
        for transform in shadow_transforms {
            depth_shader.set_mat4(&("shadowMatrices[".to_string() + &(i.to_string() + "]")), transform);
            i += 1;
        }
        depth_shader.set_float("far_plane", far_plane);
        depth_shader.set_vec3("lightPos", light_pos);
        render_scene(&depth_shader, &gl, &cube);
        gl.unbind_framebuffer();

        gl.viewport(SCR_WIDTH, SCR_HEIGHT);
        gl.clear();
        shader.apply();
        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        shader.set_vec3("lightPos", light_pos);
        shader.set_vec3("viewPos", camera.position());
        shader.set_int("shadows", shadows.is_on as i32);
        shader.set_float("far_plane", far_plane);
        wood_tex.bind_active(gl::TEXTURE0);
        depth_map.bind_texture(1);
        render_scene(&shader, &gl, &cube);

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut camera, &mut window, event);
        }
    }
}

fn render_scene(shader: &Program, gl: &gl::Gl, cube: &VertexArray) {
    shader.set_mat4("model", Mat4::scale(Vec3::new(5.0, 5.0, 5.0)));
    gl.cull_enable(false); // note that we disable culling here since we render 'inside' the cube instead of the usual 'outside' which throws off the normal culling methods.
    shader.set_int("reverse_normals", 1); // A small little hack to invert normals when drawing cube from the inside so lighting still works.
    cube.bind();
    cube.draw();
    shader.set_int("reverse_normals", 0); // and of course disable it
    gl.cull_enable(true);

    let model = Mat4::translate(Vec3::new(4.0, -3.5, 0.0)) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
    shader.set_mat4("model", model);
    cube.draw();
    let model = Mat4::translate(Vec3::new(2.0, 3.0, 1.0)) * Mat4::scale(Vec3::new(0.75, 0.75, 0.75));
    shader.set_mat4("model", model);
    cube.draw();
    let model = Mat4::translate(Vec3::new(-3.0, -1.0, 0.0)) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
    shader.set_mat4("model", model);
    cube.draw();
    let model = Mat4::translate(Vec3::new(-1.5, 1.0, 1.5)) * Mat4::scale(Vec3::new(0.5, 0.5, 0.5));
    shader.set_mat4("model", model);
    cube.draw();
    let model =
        Mat4::translate(Vec3::new(-1.5, 2.0, -3.0)) * Mat4::rotate(Quaternion::axis_angle(Vec3::new(1.0, 0.0, 1.0), PI / 3.0)) * Mat4::scale(Vec3::new(0.75, 0.75, 0.75));
    shader.set_mat4("model", model);
    cube.draw();
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

fn process_input(camera: &mut Camera, shadows: &mut Shadows, window: &mut Window, delta_time: f32) {
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

    if window.get_key(Key::Space) == Action::Press && !shadows.pressed {
        shadows.is_on = !shadows.is_on;
        shadows.pressed = true;
    }

    if window.get_key(Key::Space) == Action::Release {
        shadows.pressed = false;
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

        pub fn cull_enable(&self, on: bool) {
            unsafe {
                if on {
                    self.Enable(self::CULL_FACE);
                } else {
                    self.Disable(self::CULL_FACE);
                }
            }
        }

        pub fn viewport(&self, width: u32, height: u32) {
            unsafe {
                self.Viewport(0, 0, width as i32, height as i32);
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
const CUBE_COMPONENTS: usize = 8;
const CUBE_VERTICES: [f32; CUBE_COMPONENTS * CUBE_COUNT] = [
    // back face
    1.0, -1.0, -1.0, 0.0, 0.0, -1.0, 1.0, 0.0, // bottom-right
    -1.0, -1.0, -1.0, 0.0, 0.0, -1.0, 0.0, 0.0, // bottom-left
    1.0, 1.0, -1.0, 0.0, 0.0, -1.0, 1.0, 1.0, // top-right
    -1.0, 1.0, -1.0, 0.0, 0.0, -1.0, 0.0, 1.0, // top-left
    // front face
    -1.0, -1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, // bottom-left
    1.0, -1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, // bottom-right
    -1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, // top-left
    1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, // top-right
    // left face
    -1.0, -1.0, -1.0, -1.0, 0.0, 0.0, 0.0, 1.0, // bottom-left
    -1.0, -1.0, 1.0, -1.0, 0.0, 0.0, 0.0, 0.0, // bottom-right
    -1.0, 1.0, -1.0, -1.0, 0.0, 0.0, 1.0, 1.0, // top-left
    -1.0, 1.0, 1.0, -1.0, 0.0, 0.0, 1.0, 0.0, // top-right
    // right face
    1.0, -1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, // bottom-left
    1.0, -1.0, -1.0, 1.0, 0.0, 0.0, 0.0, 1.0, // bottom-right
    1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, // top-left
    1.0, 1.0, -1.0, 1.0, 0.0, 0.0, 1.0, 1.0, // top-right
    // bottom face
    -1.0, -1.0, -1.0, 0.0, -1.0, 0.0, 0.0, 1.0, // top-right
    1.0, -1.0, -1.0, 0.0, -1.0, 0.0, 1.0, 1.0, // top-left
    -1.0, -1.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, // bottom-right
    1.0, -1.0, 1.0, 0.0, -1.0, 0.0, 1.0, 0.0, // bottom-left
    // top face
    -1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, // bottom-left
    1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, // bottom-right
    -1.0, 1.0, -1.0, 0.0, 1.0, 0.0, 0.0, 1.0, // top-left
    1.0, 1.0, -1.0, 0.0, 1.0, 0.0, 1.0, 1.0, // top-right
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

pub struct Framebuffer {
    gl: Rc<gl::Gl>,
    framebuffer: gl::types::GLuint,
    texture: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut framebuffer = 0;
        let mut texture = 0;

        unsafe {
            gl.GenFramebuffers(1, &mut framebuffer);
            gl.GenTextures(1, &mut texture);
        }

        Framebuffer { gl, framebuffer, texture }
    }

    pub fn complete(self) -> Self {
        let gl = &self.gl;

        unsafe {
            gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);

            gl.BindTexture(gl::TEXTURE_CUBE_MAP, self.texture);
            for i in 0..6 {
                gl.TexImage2D(
                    gl::TEXTURE_CUBE_MAP_POSITIVE_X + i as u32,
                    0,
                    gl::DEPTH_COMPONENT as i32,
                    SHADOW_WIDTH as i32,
                    SHADOW_HEIGHT as i32,
                    0,
                    gl::DEPTH_COMPONENT,
                    gl::FLOAT,
                    ptr::null(),
                );
            }
            let nearest = gl::NEAREST as i32;
            let clamp_border = gl::CLAMP_TO_BORDER as i32;
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_MIN_FILTER, nearest);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_MAG_FILTER, nearest);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_WRAP_S, clamp_border);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_WRAP_T, clamp_border);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_WRAP_R, clamp_border);
            gl.FramebufferTexture(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, self.texture, 0);

            if gl.CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }

            gl.DrawBuffer(gl::NONE);
            gl.ReadBuffer(gl::NONE);
            gl.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        self
    }

    pub fn bind(&self) {
        unsafe {
            self.gl.BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_texture(&self, unit: u32) {
        assert!(gl::TEXTURE0 + unit <= gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            self.gl.ActiveTexture(gl::TEXTURE0 + unit);
            self.gl.BindTexture(gl::TEXTURE_CUBE_MAP, self.texture);
        }
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        let gl = &self.gl;

        unsafe {
            gl.DeleteFramebuffers(1, &self.framebuffer);
            gl.DeleteTextures(1, &self.texture);
        }
    }
}

pub struct Shadows {
    pub is_on: bool,
    pub pressed: bool,
}
