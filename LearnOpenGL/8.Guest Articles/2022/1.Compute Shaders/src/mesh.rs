pub mod mesh {
    use std::{mem::size_of, ptr};

    const FLOAT_SIZE: usize = size_of::<f32>();
    const U32_SIZE: usize = size_of::<u32>();

    pub struct VertexArray {
        vertex_array: gl::types::GLuint,
        vertex_buffer: gl::types::GLuint,
        index_buffer: gl::types::GLuint,
        index_count: usize,
    }

    impl VertexArray {
        pub fn new() -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            unsafe {
                gl::GenVertexArrays(1, &mut vertex_array);
                gl::GenBuffers(1, &mut vertex_buffer);
                gl::GenBuffers(1, &mut index_buffer);
            }

            VertexArray {
                vertex_array,
                vertex_buffer,
                index_buffer,
                index_count: 0,
            }
        }

        pub fn new_quad() -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            let vertices: Vec<f32> = vec![
                -1.0, -1.0, 0.0, 0.0, 0.0, // 1
                1.0, -1.0, 0.0, 1.0, 0.0, // 2
                -1.0, 1.0, 0.0, 0.0, 1.0, // 3
                1.0, 1.0, 0.0, 1.0, 1.0, // 4
            ];
            let indices = vec![0, 1, 2, 3];

            unsafe {
                gl::GenVertexArrays(1, &mut vertex_array);
                gl::GenBuffers(1, &mut vertex_buffer);
                gl::GenBuffers(1, &mut index_buffer);

                gl::BindVertexArray(vertex_array);

                gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
                gl::BufferData(gl::ARRAY_BUFFER, (FLOAT_SIZE * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 5) as i32, ptr::null());
                gl::EnableVertexAttribArray(0);

                gl::VertexAttribPointer(1, 2, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 5) as i32, ((3 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(1);

                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
                gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (indices.len() * U32_SIZE) as isize, indices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::BindVertexArray(0);
            }

            VertexArray {
                vertex_array,
                vertex_buffer,
                index_buffer,
                index_count: indices.len(),
            }
        }

        pub fn bind(&self) {
            unsafe {
                gl::BindVertexArray(self.vertex_array);
            }
        }

        pub fn draw(&self) {
            unsafe {
                gl::DrawElements(gl::TRIANGLE_STRIP, self.index_count as i32, gl::UNSIGNED_INT, ptr::null());
            }
        }
    }

    impl Drop for VertexArray {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteVertexArrays(1, &self.vertex_array);
                gl::DeleteBuffers(1, &self.vertex_buffer);
                gl::DeleteBuffers(1, &self.index_buffer);
            }
        }
    }

    pub struct Texture {
        texture: gl::types::GLuint,
    }

    impl Texture {
        pub fn new() -> Self {
            let mut texture = 0;

            unsafe {
                gl::GenTextures(1, &mut texture);

                gl::ActiveTexture(gl::TEXTURE0);
                gl::BindTexture(gl::TEXTURE_2D, texture);

                let clamp_edge = gl::CLAMP_TO_EDGE as i32;
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);

                let linear = gl::LINEAR as i32;
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);

                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    0,
                    gl::RGBA32F as i32,
                    crate::TEX_WIDTH,
                    crate::TEX_HEIGHT,
                    0,
                    gl::RGBA,
                    gl::FLOAT,
                    ptr::null(),
                );

                gl::BindImageTexture(0, texture, 0, gl::FALSE, 0, gl::READ_WRITE, gl::RGBA32F);
            }

            Texture { texture }
        }

        pub fn bind(&self) {
            unsafe {
                gl::BindTexture(gl::TEXTURE_2D, self.texture);
            }
        }
    }

    impl Drop for Texture {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteTextures(1, &self.texture);
            }
        }
    }
}
