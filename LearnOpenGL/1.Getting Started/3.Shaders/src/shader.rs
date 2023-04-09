pub mod shader {
    use crate::gl;
    use std::fs::File;
    use std::io::prelude::*;
    use std::rc::Rc;

    const LOG_SIZE: usize = 1024;

    struct Shader {
        gl: Rc<gl::Gl>,
        shader: gl::types::GLuint,
    }

    impl Shader {
        pub fn new(gl: Rc<gl::Gl>, shader_type: gl::types::GLenum) -> Self {
            let shader = unsafe { gl.CreateShader(shader_type) };
            Shader { gl, shader }
        }

        pub fn compile(self, source: &[u8]) -> Self {
            let gl = &self.gl;
            let shader = self.shader;

            let mut success: i32 = 0;
            let mut info_log = [0; LOG_SIZE];

            unsafe {
                gl.ShaderSource(shader, 1, [source.as_ptr().cast()].as_ptr(), std::ptr::null());
                gl.CompileShader(shader);

                gl.GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);
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

            self
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
        gl: Rc<gl::Gl>,
        program: gl::types::GLuint,
    }

    impl Program {
        pub fn new(gl: Rc<gl::Gl>) -> Self {
            let program = unsafe { gl.CreateProgram() };
            Program { gl, program }
        }

        pub fn link(self, vertex_file: &str, fragment_file: &str) -> Self {
            let gl = &self.gl;
            let program = self.program;

            let mut vertex_source = Vec::new();
            File::open(vertex_file).unwrap().read_to_end(&mut vertex_source).unwrap();
            vertex_source.push(0);

            let mut fragment_source = Vec::new();
            File::open(fragment_file).unwrap().read_to_end(&mut fragment_source).unwrap();
            fragment_source.push(0);

            let mut success: i32 = 0;
            let mut info_log = [0; LOG_SIZE];

            let vertex_shader = Shader::new(Rc::clone(&gl), gl::VERTEX_SHADER).compile(&vertex_source);
            let fragment_shader = Shader::new(Rc::clone(&gl), gl::FRAGMENT_SHADER).compile(&fragment_source);

            unsafe {
                gl.AttachShader(program, vertex_shader.shader);
                gl.AttachShader(program, fragment_shader.shader);
                gl.LinkProgram(program);
                gl.DetachShader(program, vertex_shader.shader);
                gl.DetachShader(program, fragment_shader.shader);

                gl.GetProgramiv(program, gl::LINK_STATUS, &mut success);
            }

            if success == 0 {
                unsafe {
                    gl.GetProgramInfoLog(program, LOG_SIZE.try_into().unwrap(), std::ptr::null_mut(), info_log.as_mut_ptr());
                }
                println!(
                    "{}",
                    std::str::from_utf8(&info_log.iter().take_while(|&i| *i > 0).map(|i| *i as u8).collect::<Vec<u8>>()).unwrap()
                );
            }

            self
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
}
