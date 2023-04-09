use gfx_maths::{mat4::Mat4, quaternion::Quaternion, vec2::Vec2, vec3::Vec3};
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
use mesh::mesh::{Mesh, Texture, Vertex};
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

    let gl = Rc::new(gl::Gl::load_with(|s| window.get_proc_address(s).cast()));

    gl.depth_enable(true);

    let asteroid_shader = Program::new(Rc::clone(&gl)).link("src/10.3.asteroids.vs", "src/10.3.asteroids.fs", None);
    let planet_shader = Program::new(Rc::clone(&gl)).link("src/10.3.planet.vs", "src/10.3.planet.fs", None);

    let rock = Model::new(Rc::clone(&gl)).load_model("resources/objects/rock/rock.obj");
    let planet = Model::new(Rc::clone(&gl)).load_model("resources/objects/planet/planet.obj");

    let amount = 100_000;
    let mut model_matrices = Vec::new();
    let radius = 150.0;
    let offset = 25.0;
    for i in 0..amount {
        let angle = i as f32 / amount as f32 * 360.0;
        let mut displacement = (fastrand::i32(..) % (2 * offset as i32 * 100)) as f32 / 100.0 - offset;
        let x = f32::sin(angle) * radius + displacement;
        displacement = (fastrand::i32(..) % (2 * offset as i32 * 100)) as f32 / 100.0 - offset;
        let y = displacement * 0.4;
        displacement = (fastrand::i32(..) % (2 * offset as i32 * 100)) as f32 / 100.0 - offset;
        let z = f32::cos(angle) * radius + displacement;

        let scale = (fastrand::i32(..) % 20) as f32 / 100.0 + 0.05;

        let rot_angle = (fastrand::i32(..) % 360) as f32;

        model_matrices.push(
            Mat4::translate(Vec3::new(x, y, z)) * Mat4::scale(Vec3::new(scale, scale, scale)) * Mat4::rotate(Quaternion::axis_angle(Vec3::new(0.4, 0.6, 0.8), rot_angle)),
        );
    }

    let _buffer = InstanceBuffer::new(Rc::clone(&gl), amount, model_matrices);

    rock.instance_meshes();

    glfw.poll_events();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 155.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear(0.1, 0.1, 0.1, 1.0);

        let projection = Mat4::perspective_opengl(PI / 4.0, SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        let view = camera.view_matrix();
        asteroid_shader.apply();
        asteroid_shader.set_mat4("projection", projection);
        asteroid_shader.set_mat4("view", view);
        planet_shader.apply();
        planet_shader.set_mat4("projection", projection);
        planet_shader.set_mat4("view", view);
        planet_shader.set_mat4("model", Mat4::translate(Vec3::new(0.0, -3.0, 0.0)) * Mat4::scale(Vec3::new(4.0, 4.0, 4.0)));
        planet.draw(&planet_shader);

        asteroid_shader.apply();
        asteroid_shader.set_int("texture_diffuse1", 0);
        rock.bind_texture();
        for mesh in &rock.meshes {
            mesh.draw_instanced(amount as i32);
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

pub struct InstanceBuffer {
    gl: Rc<gl::Gl>,
    buffer: gl::types::GLuint,
}

impl InstanceBuffer {
    pub fn new(gl: Rc<gl::Gl>, amount: usize, matrices: Vec<Mat4>) -> Self {
        let mut buffer = 0;

        unsafe {
            gl.GenBuffers(1, &mut buffer);
            gl.BindBuffer(gl::ARRAY_BUFFER, buffer);
            gl.BufferData(gl::ARRAY_BUFFER, (amount * size_of::<Mat4>()) as isize, matrices.as_ptr().cast(), gl::STATIC_DRAW);
        }

        InstanceBuffer { gl, buffer }
    }
}

impl Drop for InstanceBuffer {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteBuffers(1, &self.buffer);
        }
    }
}
