pub mod mesh {
    use stb_image::stb_image::bindgen;
    use std::{fs::File, io::Read, mem::size_of, ptr};

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

        pub fn new_with(vertices: Vec<f32>, indices: Vec<u32>) -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            unsafe {
                gl::GenVertexArrays(1, &mut vertex_array);
                gl::GenBuffers(1, &mut vertex_buffer);
                gl::GenBuffers(1, &mut index_buffer);

                gl::BindVertexArray(vertex_array);

                gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
                gl::BufferData(gl::ARRAY_BUFFER, (FLOAT_SIZE * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 3) as i32, ptr::null());
                gl::EnableVertexAttribArray(0);

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
                gl::DrawElements(gl::TRIANGLES, self.index_count as i32, gl::UNSIGNED_INT, ptr::null());
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

    pub fn stbi_flip_vertical(flip: bool) {
        unsafe {
            bindgen::stbi_set_flip_vertically_on_load(flip as i32);
        }
    }

    #[derive(Debug)]
    pub struct Image {
        dataf32: *mut f32,
        datau8: *mut u8,
        pub width: i32,
        pub height: i32,
        pub components: i32,
    }

    impl Image {
        pub fn new(path: &str, hdr: bool) -> Self {
            let mut file = File::open(path).unwrap();
            let mut contents = Vec::new();

            file.read_to_end(&mut contents).unwrap();

            let mut width = 0;
            let mut height = 0;
            let mut components = 0;

            if hdr {
                let dataf32 = unsafe { bindgen::stbi_loadf_from_memory(contents.as_mut_ptr(), contents.len() as i32, &mut width, &mut height, &mut components, 0) };

                Image {
                    dataf32,
                    datau8: ptr::null_mut(),
                    width,
                    height,
                    components,
                }
            } else {
                let datau8 = unsafe { bindgen::stbi_load_from_memory(contents.as_mut_ptr(), contents.len() as i32, &mut width, &mut height, &mut components, 0) };

                Image {
                    dataf32: ptr::null_mut(),
                    datau8,
                    width,
                    height,
                    components,
                }
            }
        }

        pub unsafe fn data(&self) -> &*mut u8 {
            if self.datau8 != ptr::null_mut() {
                &self.datau8
            } else {
                panic!("unexpected null")
            }
        }
    }

    impl Drop for Image {
        fn drop(&mut self) {
            unsafe {
                if self.dataf32 != ptr::null_mut() {
                    bindgen::stbi_image_free(self.dataf32.cast());
                }
                if self.datau8 != ptr::null_mut() {
                    bindgen::stbi_image_free(self.datau8.cast());
                }
            }
        }
    }
}
