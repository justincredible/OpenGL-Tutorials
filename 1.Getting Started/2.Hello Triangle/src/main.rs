use glfw::{Action, Context, Key, Window, WindowHint};
use std::rc::Rc;

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;

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

    let program = gl::Program::new(Rc::clone(&gl));
    program.link();

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

    const LOG_SIZE: usize = 512;

    struct Shader {
        gl: Rc<self::Gl>,
        shader: self::types::GLuint,
    }

    impl Shader {
        pub fn new(gl: Rc<self::Gl>, shader_type: self::types::GLenum) -> Self {
            let shader = unsafe { gl.CreateShader(shader_type) };
            Shader { gl, shader }
        }

        pub fn compile(&self, source: &[u8]) {
            let gl = &self.gl;
            let shader = self.shader;

            let mut success: i32 = 0;
            let mut info_log = [0; LOG_SIZE];

            unsafe {
                gl.ShaderSource(shader, 1, [source.as_ptr().cast()].as_ptr(), std::ptr::null());
                gl.CompileShader(shader);

                gl.GetShaderiv(shader, self::COMPILE_STATUS, &mut success);
            }

            if success == 0 {
                unsafe {
                    gl.GetShaderInfoLog(shader, LOG_SIZE.try_into().unwrap(), std::ptr::null_mut(), info_log.as_mut_ptr());
                }
                println!(
                    "{}",
                    std::str::from_utf8(&info_log.iter().take_while(|&i| *i > 0).map(|i| *i as u8).collect::<Vec<u8>>()).unwrap()
                );
            }
        }
    }

    impl Drop for Shader {
        fn drop(&mut self) {
            unsafe {
                self.gl.DeleteShader(self.shader);
            }
        }
    }

    pub struct Program {
        gl: Rc<self::Gl>,
        program: self::types::GLuint,
    }

    impl Program {
        pub fn new(gl: Rc<self::Gl>) -> Self {
            let program = unsafe { gl.CreateProgram() };
            Program { gl, program }
        }

        pub fn link(&self) {
            let gl = &self.gl;
            let program = self.program;

            let mut success: i32 = 0;
            let mut info_log = [0; LOG_SIZE];

            let vertex_shader = Shader::new(Rc::clone(gl), self::VERTEX_SHADER);
            vertex_shader.compile(crate::VERTEX_SHADER_SOURCE);

            let fragment_shader = Shader::new(Rc::clone(gl), self::FRAGMENT_SHADER);
            fragment_shader.compile(crate::FRAGMENT_SHADER_SOURCE);

            unsafe {
                gl.AttachShader(program, vertex_shader.shader);
                gl.AttachShader(program, fragment_shader.shader);
                gl.LinkProgram(program);
                gl.DetachShader(program, vertex_shader.shader);
                gl.DetachShader(program, fragment_shader.shader);

                gl.GetProgramiv(program, self::LINK_STATUS, &mut success);
            }

            if success == 0 {
                // GL_FALSE
                unsafe {
                    gl.GetProgramInfoLog(program, LOG_SIZE.try_into().unwrap(), std::ptr::null_mut(), info_log.as_mut_ptr());
                }
                println!(
                    "{}",
                    std::str::from_utf8(&info_log.iter().take_while(|&i| *i > 0).map(|i| *i as u8).collect::<Vec<u8>>()).unwrap()
                );
            }
        }

        pub fn apply(&self) {
            unsafe {
                self.gl.UseProgram(self.program);
            }
        }
    }

    impl Drop for Program {
        fn drop(&mut self) {
            unsafe {
                self.gl.DeleteProgram(self.program);
            }
        }
    }

    const NUM_VERTICES: usize = 4;
    const VERTICES: [f32; 3 * NUM_VERTICES] = [0.5, 0.5, 0.0, 0.5, -0.5, 0.0, -0.5, -0.5, 0.0, -0.5, 0.5, 0.0];

    const NUM_INDICES: usize = 6;
    const INDICES: [u8; NUM_INDICES] = [0, 1, 3, 1, 2, 3];

    pub struct VertexArray {
        gl: Rc<self::Gl>,
        vertex_array: self::types::GLuint,
        vertex_buffer: self::types::GLuint,
        index_buffer: self::types::GLuint,
    }

    impl VertexArray {
        pub fn new(gl: Rc<self::Gl>) -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            unsafe {
                gl.GenVertexArrays(1, &mut vertex_array);
                gl.GenBuffers(1, &mut vertex_buffer);
                gl.GenBuffers(1, &mut index_buffer);

                gl.BindVertexArray(vertex_array);

                gl.BindBuffer(self::ARRAY_BUFFER, vertex_buffer);
                gl.BufferData(
                    self::ARRAY_BUFFER,
                    (std::mem::size_of::<f32>() * 3 * NUM_VERTICES).try_into().unwrap(),
                    VERTICES.as_ptr().cast(),
                    self::STATIC_DRAW,
                );

                gl.BindBuffer(self::ELEMENT_ARRAY_BUFFER, index_buffer);
                gl.BufferData(
                    self::ELEMENT_ARRAY_BUFFER,
                    NUM_INDICES.try_into().unwrap(),
                    INDICES.as_ptr().cast(),
                    self::STATIC_DRAW,
                );

                gl.VertexAttribPointer(
                    0,
                    3,
                    self::FLOAT,
                    self::FALSE,
                    (3 * std::mem::size_of::<f32>()).try_into().unwrap(),
                    std::ptr::null(),
                );
                gl.EnableVertexAttribArray(0);

                gl.BindBuffer(self::ARRAY_BUFFER, 0);

                gl.BindVertexArray(0);
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

                self.gl
                    .DrawElements(self::TRIANGLES, NUM_INDICES.try_into().unwrap(), self::UNSIGNED_BYTE, std::ptr::null());
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
}

const VERTEX_SHADER_SOURCE: &[u8] = b"
#version 330 core

layout (location = 0) in vec3 aPos;
void main()
{
	gl_Position = vec4(aPos, 1.0);
}
\0";

const FRAGMENT_SHADER_SOURCE: &[u8] = b"
#version 330 core

out vec4 FragColor;

void main()
{
	FragColor = vec4(1.0, 0.5, 0.2, 1.0);
}
\0";
