use gfx_maths::{mat4::Mat4, vec3::Vec3};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, mem::size_of, ptr, rc::Rc};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const SCR_NEAR: f32 = 0.1;
const SCR_FAR: f32 = 100.0;

pub mod shader;
use shader::shader::Program;
pub mod camera;
use camera::camera::Camera;
use camera::camera::Movement;

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

    gl.depth_enable();

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);
    let light_position = Vec3::new(1.2, 1.0, 2.0);
    let lighting_shader = Program::new(Rc::clone(&gl)).link("src/2.2.basic_lighting.vs", "src/2.2.basic_lighting.fs");
    let light_cube_shader = Program::new(Rc::clone(&gl)).link("src/2.2.light_cube.vs", "src/2.2.light_cube.fs");
    let cube = VertexArray::new(Rc::clone(&gl));
    let light_cube = VertexArray::new(Rc::clone(&gl));

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, delta_time);

        gl.clear(0.1, 0.1, 0.1, 1.0);

        lighting_shader.apply();
        lighting_shader.set_vec3("objectColor", Vec3::new(1.0, 0.5, 0.31));
        lighting_shader.set_vec3("lightColor", Vec3::new(1.0, 1.0, 1.0));
        lighting_shader.set_vec3("lightPos", light_position);
        lighting_shader.set_vec3("viewPos", camera.position());

        let projection = Mat4::perspective_opengl(camera.zoom(), SCR_NEAR, SCR_FAR, SCR_WIDTH as f32 / SCR_HEIGHT as f32);
        lighting_shader.set_mat4("projection", projection);

        let view = camera.view_matrix();
        lighting_shader.set_mat4("view", view);

        let model = Mat4::identity();
        lighting_shader.set_mat4("model", model);

        cube.draw();

        light_cube_shader.apply();
        light_cube_shader.set_mat4("projection", projection);
        light_cube_shader.set_mat4("view", view);

        let model = Mat4::translate(light_position) * Mat4::scale(Vec3::new(0.2, 0.2, 0.2));
        light_cube_shader.set_mat4("model", model);

        light_cube.draw();

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

        pub fn depth_enable(&self) {
            unsafe {
                self.Enable(self::DEPTH_TEST);
            }
        }
    }
}

const FLOAT_SIZE: usize = size_of::<f32>();
const VERTEX_COUNT: usize = 24;
const VERTEX_COMPONENTS: usize = 6;
const VERTICES: [f32; VERTEX_COMPONENTS * VERTEX_COUNT] = [
    -0.5, -0.5, -0.5, 0.0, 0.0, -1.0, 0.5, -0.5, -0.5, 0.0, 0.0, -1.0, -0.5, 0.5, -0.5, 0.0, 0.0, -1.0, 0.5, 0.5, -0.5, 0.0, 0.0, -1.0, // front face
    -0.5, -0.5, 0.5, 0.0, 0.0, 1.0, -0.5, 0.5, 0.5, 0.0, 0.0, 1.0, 0.5, -0.5, 0.5, 0.0, 0.0, 1.0, 0.5, 0.5, 0.5, 0.0, 0.0, 1.0, // back face
    -0.5, 0.5, 0.5, -1.0, 0.0, 0.0, -0.5, -0.5, 0.5, -1.0, 0.0, 0.0, -0.5, 0.5, -0.5, -1.0, 0.0, 0.0, -0.5, -0.5, -0.5, -1.0, 0.0, 0.0, // left face
    0.5, 0.5, 0.5, 1.0, 0.0, 0.0, 0.5, 0.5, -0.5, 1.0, 0.0, 0.0, 0.5, -0.5, 0.5, 1.0, 0.0, 0.0, 0.5, -0.5, -0.5, 1.0, 0.0, 0.0, // right face
    -0.5, -0.5, -0.5, 0.0, -1.0, 0.0, -0.5, -0.5, 0.5, 0.0, -1.0, 0.0, 0.5, -0.5, -0.5, 0.0, -1.0, 0.0, 0.5, -0.5, 0.5, 0.0, -1.0, 0.0, // bottom face
    -0.5, 0.5, -0.5, 0.0, 1.0, 0.0, 0.5, 0.5, -0.5, 0.0, 1.0, 0.0, -0.5, 0.5, 0.5, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.0, 1.0, 0.0, // top face
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

pub struct VertexArray {
    gl: Rc<gl::Gl>,
    vertex_array: gl::types::GLuint,
    vertex_buffer: gl::types::GLuint,
    index_buffer: gl::types::GLuint,
}

impl VertexArray {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut vertex_array = 0;
        let mut vertex_buffer = 0;
        let mut index_buffer = 0;

        unsafe {
            gl.GenVertexArrays(1, &mut vertex_array);
            gl.GenBuffers(1, &mut vertex_buffer);
            gl.GenBuffers(1, &mut index_buffer);

            gl.BindVertexArray(vertex_array);

            gl.BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
            gl.BufferData(
                gl::ARRAY_BUFFER,
                (FLOAT_SIZE * VERTEX_COMPONENTS * VERTEX_COUNT) as isize,
                VERTICES.as_ptr().cast(),
                gl::STATIC_DRAW,
            );

            gl.VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (VERTEX_COMPONENTS * FLOAT_SIZE) as i32, ptr::null());
            gl.EnableVertexAttribArray(0);

            gl.VertexAttribPointer(
                1,
                3,
                gl::FLOAT,
                gl::FALSE,
                (VERTEX_COMPONENTS * FLOAT_SIZE) as i32,
                ((3 * FLOAT_SIZE) as *const usize).cast(),
            );
            gl.EnableVertexAttribArray(1);

            gl.BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
            gl.BufferData(gl::ELEMENT_ARRAY_BUFFER, INDEX_COUNT as isize, INDICES.as_ptr().cast(), gl::STATIC_DRAW);
        }

        VertexArray {
            gl,
            vertex_array,
            vertex_buffer,
            index_buffer,
        }
    }

    pub fn draw(&self) {
        unsafe {
            self.gl.BindVertexArray(self.vertex_array);

            self.gl.DrawElements(gl::TRIANGLES, INDEX_COUNT as i32, gl::UNSIGNED_BYTE, ptr::null());
        }
    }
}

impl Drop for VertexArray {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteVertexArrays(1, &self.vertex_array);
            self.gl.DeleteBuffers(1, &self.vertex_buffer);
            self.gl.DeleteBuffers(1, &self.index_buffer);
        }
    }
}
