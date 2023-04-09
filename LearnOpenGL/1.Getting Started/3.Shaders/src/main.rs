use glfw::{Action, Context, Key, Window, WindowHint};
use std::rc::Rc;

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;

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
    window.set_framebuffer_size_polling(true);
    window.focus();

    let gl = Rc::new(gl::Gl::load_with(|s| window.get_proc_address(s) as *const _));

    let program = Program::new(Rc::clone(&gl)).link("src/3.3.shader.vs", "src/3.3.shader.fs");
    let vao = gl::VertexArray::new(Rc::clone(&gl));

    while !window.should_close() {
        process_input(&mut window);

        gl.clear(0.2, 0.3, 0.3, 1.0);

        program.apply();
        vao.draw();

        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut window, event);
        }
    }
}

fn process_input(window: &mut Window) {
    if window.get_key(Key::Escape) == Action::Press {
        window.set_should_close(true);
    }
}

fn handle_window_event(gl: &gl::Gl, _window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        glfw::WindowEvent::FramebufferSize(width, height) => unsafe {
            gl.Viewport(0, 0, width, height);
        },
        _ => {}
    }
}

mod gl {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

    impl self::Gl {
        pub fn clear(&self, red: f32, green: f32, blue: f32, alpha: f32) {
            unsafe {
                self.ClearColor(red, green, blue, alpha);
                self.Clear(self::COLOR_BUFFER_BIT);
            }
        }
    }

    use crate::Rc;

    const NUM_VERTICES: usize = 3;
    const VERTICES: [f32; 6 * NUM_VERTICES] = [
        // positions         // colors
        0.5, -0.5, 0.0, 1.0, 0.0, 0.0, // bottom right
        -0.5, -0.5, 0.0, 0.0, 1.0, 0.0, // bottom left
        0.0, 0.5, 0.0, 0.0, 0.0, 1.0, // top
    ];

    pub struct VertexArray {
        gl: Rc<self::Gl>,
        vertex_array: self::types::GLuint,
        vertex_buffer: self::types::GLuint,
    }

    impl VertexArray {
        pub fn new(gl: Rc<self::Gl>) -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;

            unsafe {
                gl.GenVertexArrays(1, &mut vertex_array);
                gl.GenBuffers(1, &mut vertex_buffer);

                gl.BindVertexArray(vertex_array);

                gl.BindBuffer(self::ARRAY_BUFFER, vertex_buffer);
                gl.BufferData(
                    self::ARRAY_BUFFER,
                    (std::mem::size_of::<f32>() * 6 * NUM_VERTICES).try_into().unwrap(),
                    VERTICES.as_ptr().cast(),
                    self::STATIC_DRAW,
                );

                gl.VertexAttribPointer(0, 3, self::FLOAT, self::FALSE, (6 * std::mem::size_of::<f32>()).try_into().unwrap(), std::ptr::null());
                gl.EnableVertexAttribArray(0);

                let offset = 3 * std::mem::size_of::<f32>();
                gl.VertexAttribPointer(
                    1,
                    3,
                    self::FLOAT,
                    self::FALSE,
                    (6 * std::mem::size_of::<f32>()).try_into().unwrap(),
                    (offset as *const usize).cast(),
                );
                gl.EnableVertexAttribArray(1);
            }

            VertexArray { gl, vertex_array, vertex_buffer }
        }

        pub fn draw(&self) {
            unsafe {
                self.gl.BindVertexArray(self.vertex_array);

                self.gl.DrawArrays(self::TRIANGLES, 0, NUM_VERTICES.try_into().unwrap());
            }
        }
    }

    impl Drop for VertexArray {
        fn drop(&mut self) {
            unsafe {
                self.gl.DeleteVertexArrays(1, &self.vertex_array);
                self.gl.DeleteBuffers(1, &self.vertex_buffer);
            }
        }
    }
}
