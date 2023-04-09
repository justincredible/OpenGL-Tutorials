pub mod shader {
    use glam::{Mat4, Vec2, Vec3, Vec4};
    use glfw::{string_from_c_str, with_c_str};
    use std::ptr;

    pub struct Shader {
        pub id: u32,
    }

    impl Drop for Shader {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteProgram(self.id);
            }
        }
    }

    impl Shader {
        pub fn r#use(&self) {
            unsafe {
                gl::UseProgram(self.id);
            }
        }

        pub fn compile(vertex_source: Vec<u8>, fragment_source: Vec<u8>) -> Self {
            unsafe {
                let s_vertex = gl::CreateShader(gl::VERTEX_SHADER);
                gl::ShaderSource(s_vertex, 1, [vertex_source.as_ptr().cast()].as_ptr(), ptr::null());
                gl::CompileShader(s_vertex);
                check_compile_errors(s_vertex, "VERTEX");
                let s_fragment = gl::CreateShader(gl::FRAGMENT_SHADER);
                gl::ShaderSource(s_fragment, 1, [fragment_source.as_ptr().cast()].as_ptr(), ptr::null());
                gl::CompileShader(s_fragment);
                check_compile_errors(s_fragment, "FRAGMENT");
                let id = gl::CreateProgram();
                gl::AttachShader(id, s_vertex);
                gl::AttachShader(id, s_fragment);
                gl::LinkProgram(id);
                check_compile_errors(id, "PROGRAM");
                gl::DeleteShader(s_vertex);
                gl::DetachShader(id, s_vertex);
                gl::DeleteShader(s_fragment);
                gl::DetachShader(id, s_fragment);

                Shader { id }
            }
        }

        pub fn set_float(&self, name: &str, value: f32) {
            unsafe {
                with_c_str(name, |locn| gl::Uniform1f(gl::GetUniformLocation(self.id, locn), value));
            }
        }

        pub fn set_int(&self, name: &str, value: i32) {
            unsafe {
                with_c_str(name, |locn| gl::Uniform1i(gl::GetUniformLocation(self.id, locn), value));
            }
        }

        pub fn set_vec2(&self, name: &str, value: Vec2) {
            unsafe {
                with_c_str(name, |locn| gl::Uniform2f(gl::GetUniformLocation(self.id, locn), value.x, value.y));
            }
        }

        pub fn set_vec3(&self, name: &str, value: Vec3) {
            unsafe {
                with_c_str(name, |locn| gl::Uniform3f(gl::GetUniformLocation(self.id, locn), value.x, value.y, value.z));
            }
        }

        pub fn set_vec4(&self, name: &str, value: Vec4) {
            unsafe {
                with_c_str(name, |locn| gl::Uniform4f(gl::GetUniformLocation(self.id, locn), value.x, value.y, value.z, value.w));
            }
        }

        pub fn set_mat4(&self, name: &str, value: Mat4) {
            let mut matrix = [0.0; 16];
            value.write_cols_to_slice(&mut matrix);

            unsafe {
                with_c_str(name, |locn| gl::UniformMatrix4fv(gl::GetUniformLocation(self.id, locn), 1, gl::FALSE, matrix.as_ptr()));
            }
        }
    }

    fn check_compile_errors(object: u32, r#type: &str) {
        let mut success = 0;
        let mut info_log: [i8; 1024] = [0; 1024];
        unsafe {
            if r#type != "PROGRAM" {
                gl::GetShaderiv(object, gl::COMPILE_STATUS, &mut success);
                if success == 0 {
                    gl::GetShaderInfoLog(object, 1024, ptr::null_mut(), info_log.as_mut_ptr());
                    println!("| ERROR::SHADER: Compile-time error: Type: {}", r#type);
                    println!("{}", string_from_c_str(info_log.as_ptr()));
                    println!("\n -- --------------------------------------------------- -- ");
                }
            } else {
                gl::GetProgramiv(object, gl::LINK_STATUS, &mut success);
                if success == 0 {
                    gl::GetProgramInfoLog(object, 1024, ptr::null_mut(), info_log.as_mut_ptr());
                    println!("| ERROR::Shader: Link-time error: Type: {}", r#type);
                    println!("{}", string_from_c_str(info_log.as_ptr()));
                    println!("\n -- --------------------------------------------------- -- ");
                }
            }
        }
    }
}
