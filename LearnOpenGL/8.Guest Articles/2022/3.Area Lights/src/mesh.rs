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
        render_primitive: u32,
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
                render_primitive: gl::TRIANGLES,
            }
        }

        pub fn new_plane(vertices: &Vec<f32>) -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            let indices = vec![0, 1, 2, 3];

            unsafe {
                gl::GenVertexArrays(1, &mut vertex_array);
                gl::GenBuffers(1, &mut vertex_buffer);
                gl::GenBuffers(1, &mut index_buffer);

                gl::BindVertexArray(vertex_array);

                gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
                gl::BufferData(gl::ARRAY_BUFFER, (FLOAT_SIZE * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 8) as i32, ptr::null());
                gl::EnableVertexAttribArray(0);

                gl::VertexAttribPointer(1, 3, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 8) as i32, ((3 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(1);

                gl::VertexAttribPointer(2, 2, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 8) as i32, ((6 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(2);

                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
                gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (indices.len() * U32_SIZE) as isize, indices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::BindVertexArray(0);
            }

            VertexArray {
                vertex_array,
                vertex_buffer,
                index_buffer,
                index_count: indices.len(),
                render_primitive: gl::TRIANGLE_STRIP,
            }
        }

        pub fn new_area_light(vertices: &Vec<f32>) -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            let indices = vec![0, 1, 2, 3];

            unsafe {
                gl::GenVertexArrays(1, &mut vertex_array);
                gl::GenBuffers(1, &mut vertex_buffer);
                gl::GenBuffers(1, &mut index_buffer);

                gl::BindVertexArray(vertex_array);

                gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
                gl::BufferData(gl::ARRAY_BUFFER, (FLOAT_SIZE * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 8) as i32, ptr::null());
                gl::EnableVertexAttribArray(0);

                gl::VertexAttribPointer(1, 3, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 8) as i32, ((3 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(1);

                gl::VertexAttribPointer(2, 2, gl::FLOAT, gl::FALSE, (FLOAT_SIZE * 8) as i32, ((6 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(2);

                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
                gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (indices.len() * U32_SIZE) as isize, indices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::BindVertexArray(0);
            }

            VertexArray {
                vertex_array,
                vertex_buffer,
                index_buffer,
                index_count: indices.len(),
                render_primitive: gl::TRIANGLE_STRIP,
            }
        }

        pub fn bind(&self) {
            unsafe {
                gl::BindVertexArray(self.vertex_array);
            }
        }

        pub fn draw(&self) {
            unsafe {
                gl::DrawElements(self.render_primitive, self.index_count as i32, gl::UNSIGNED_INT, ptr::null());
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
    struct Image {
        dataf32: *mut f32,
        datau8: *mut u8,
        width: i32,
        height: i32,
        components: i32,
    }

    impl Image {
        pub fn new(path: &str, hdr: bool) -> Self {
            let mut file = File::open(path).unwrap();
            let mut contents = vec![];

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

    pub struct Texture {
        texture: gl::types::GLuint,
    }

    impl Texture {
        pub fn new_hdr(path: &str) -> Self {
            let mut texture = 0;

            let image = Image::new(path, false);

            if image.datau8 == ptr::null_mut() {
                panic!("Failed to load HDR image");
            } else {
                unsafe {
                    gl::GenTextures(1, &mut texture);

                    let (internal_format, data_format) = match image.components {
                        1 => (gl::RED, gl::RED),
                        3 => (gl::SRGB, gl::RGB),
                        4 => (gl::SRGB_ALPHA, gl::RGBA),
                        _ => panic!("unexpected number of channels"),
                    };

                    gl::BindTexture(gl::TEXTURE_2D, texture);
                    gl::TexImage2D(
                        gl::TEXTURE_2D,
                        0,
                        internal_format as i32,
                        image.width,
                        image.height,
                        0,
                        data_format,
                        gl::UNSIGNED_BYTE,
                        image.dataf32.cast(),
                    );
                    gl::GenerateMipmap(gl::TEXTURE_2D);

                    let repeat = gl::REPEAT as i32;
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, repeat);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, repeat);

                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR_MIPMAP_LINEAR as i32);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
                }
            }

            Texture { texture }
        }

        pub fn bind(&self) {
            unsafe {
                gl::BindTexture(gl::TEXTURE_2D, self.texture);
            }
        }

        pub fn texture(&self) -> u32 {
            self.texture
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
