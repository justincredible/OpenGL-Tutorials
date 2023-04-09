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
use mesh::mesh::{Image, VertexArray};
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

    let shader = Program::new(Rc::clone(&gl)).link("src/6.2.cubemaps.vs", "src/6.2.cubemaps.fs");
    let skybox_shader = Program::new(Rc::clone(&gl)).link("src/6.2.skybox.vs", "src/6.2.skybox.fs");

    let cube = VertexArray::new_lit(Rc::clone(&gl), Vec::from(&CUBE_VERTICES[..]), Vec::from(&INDICES[..]));
    let skybox = VertexArray::new_pos(Rc::clone(&gl), Vec::from(&SKYBOX_VERTICES[..]), Vec::from(&INDICES[..]));

    let faces = vec![
        "resources/textures/skybox/right.jpg",
        "resources/textures/skybox/left.jpg",
        "resources/textures/skybox/top.jpg",
        "resources/textures/skybox/bottom.jpg",
        "resources/textures/skybox/front.jpg",
        "resources/textures/skybox/back.jpg",
    ];

    let cube_map = CubeMap::new(Rc::clone(&gl)).load(faces);

    cube_map.bind_active(gl::TEXTURE0);

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    shader.apply();
    shader.set_int("skybox", 0);

    skybox_shader.apply();
    skybox_shader.set_int("skybox", 0);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear(0.1, 0.1, 0.1, 1.0);

        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let mut view = camera.view_matrix();

        shader.apply();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        shader.set_vec3("cameraPos", camera.position());

        cube.bind();
        shader.set_mat4("model", Mat4::identity());
        cube.draw();

        gl.depth_func(gl::LEQUAL);
        skybox_shader.apply();
        view[(3, 0)] = 0.0;
        view[(3, 1)] = 0.0;
        view[(3, 2)] = 0.0;
        skybox_shader.set_mat4("view", view);
        skybox_shader.set_mat4("projection", projection);

        skybox.bind();
        skybox.draw();

        gl.unbind_vao();

        gl.depth_func(gl::LESS);

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

        pub fn depth_func(&self, func: self::types::GLenum) {
            unsafe {
                self.DepthFunc(func);
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
const CUBE_COMPONENTS: usize = 6;
const CUBE_VERTICES: [f32; CUBE_COMPONENTS * CUBE_COUNT] = [
    -0.5, -0.5, -0.5, 0.0, 0.0, -1.0, -0.5, 0.5, -0.5, 0.0, 0.0, -1.0, 0.5, -0.5, -0.5, 0.0, 0.0, -1.0, 0.5, 0.5, -0.5, 0.0, 0.0, -1.0, // back
    -0.5, -0.5, 0.5, 0.0, 0.0, 1.0, 0.5, -0.5, 0.5, 0.0, 0.0, 1.0, -0.5, 0.5, 0.5, 0.0, 0.0, 1.0, 0.5, 0.5, 0.5, 0.0, 0.0, 1.0, // front
    -0.5, 0.5, 0.5, -1.0, 0.0, 0.0, -0.5, 0.5, -0.5, -1.0, 0.0, 0.0, -0.5, -0.5, 0.5, -1.0, 0.0, 0.0, -0.5, -0.5, -0.5, -1.0, 0.0, 0.0, // left
    0.5, 0.5, 0.5, 1.0, 0.0, 0.0, 0.5, -0.5, 0.5, 1.0, 0.0, 0.0, 0.5, 0.5, -0.5, 1.0, 0.0, 0.0, 0.5, -0.5, -0.5, 1.0, 0.0, 0.0, // right
    -0.5, -0.5, -0.5, 0.0, -1.0, 0.0, 0.5, -0.5, -0.5, 0.0, -1.0, 0.0, -0.5, -0.5, 0.5, 0.0, -1.0, 0.0, 0.5, -0.5, 0.5, 0.0, -1.0, 0.0, // bottom
    -0.5, 0.5, -0.5, 0.0, 1.0, 0.0, -0.5, 0.5, 0.5, 0.0, 1.0, 0.0, 0.5, 0.5, -0.5, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.0, 1.0, 0.0, // top
];
const SKYBOX_COUNT: usize = 24;
const SKYBOX_COMPONENTS: usize = 3;
const SKYBOX_VERTICES: [f32; SKYBOX_COMPONENTS * SKYBOX_COUNT] = [
    -1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, -1.0, 1.0, -1.0, -1.0, // back
    -1.0, -1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, -1.0, 1.0, -1.0, // left
    1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, 1.0, -1.0, 1.0, 1.0, 1.0, // right
    -1.0, -1.0, 1.0, 1.0, -1.0, 1.0, -1.0, 1.0, 1.0, 1.0, 1.0, 1.0, // front
    -1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0, -1.0, 1.0, 1.0, 1.0, // top
    -1.0, -1.0, -1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, -1.0, 1.0, // bottom
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

pub struct CubeMap {
    gl: Rc<gl::Gl>,
    cube_map: gl::types::GLuint,
}

impl CubeMap {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut cube_map = 0;

        unsafe {
            gl.GenTextures(1, &mut cube_map);
        }

        CubeMap { gl, cube_map }
    }

    pub fn load(self, faces: Vec<&str>) -> Self {
        let gl = &self.gl;

        let linear = gl::LINEAR as i32;
        let clamp_edge = gl::CLAMP_TO_EDGE as i32;
        unsafe {
            gl.BindTexture(gl::TEXTURE_CUBE_MAP, self.cube_map);

            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_MIN_FILTER, linear);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_MAG_FILTER, linear);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_WRAP_T, clamp_edge);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_WRAP_S, clamp_edge);
            gl.TexParameteri(gl::TEXTURE_CUBE_MAP, gl::TEXTURE_WRAP_R, clamp_edge);
        }

        let mut i = 0;
        for face in faces {
            let image = Image::new(face);

            let format = match image.components {
                1 => gl::RED,
                3 => gl::RGB,
                4 => gl::RGBA,
                _ => panic!("Unexpected image format"),
            };

            if image.data == ptr::null_mut() {
                println!("Cubemap texture failed to load at path: {}", face);
            } else {
                unsafe {
                    gl.TexImage2D(
                        gl::TEXTURE_CUBE_MAP_POSITIVE_X + i,
                        0,
                        format as i32,
                        image.width,
                        image.height,
                        0,
                        format,
                        gl::UNSIGNED_BYTE,
                        image.data.cast(),
                    );
                }
            }

            i += 1;
        }

        self
    }

    pub fn bind_active(&self, active: gl::types::GLenum) {
        assert!((gl::TEXTURE0..gl::TEXTURE0 + gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS).contains(&active));

        let gl = &self.gl;

        unsafe {
            gl.ActiveTexture(active);
            gl.BindTexture(gl::TEXTURE_CUBE_MAP, self.cube_map);
        }
    }
}

impl Drop for CubeMap {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteTextures(1, &self.cube_map);
        }
    }
}
