pub mod mesh {
    use crate::{bindgen, Program, Vertex};
    use glfw::with_c_str;
    use std::{fs::File, io::Read, mem::size_of, ptr, rc::Rc};

    const FLOAT_SIZE: usize = size_of::<f32>();
    const U32_SIZE: usize = size_of::<u32>();

    pub struct Mesh {
        indices: Vec<u32>,
        textures: Vec<Rc<Texture>>,
        pub vao: VertexArray,
    }

    impl Mesh {
        pub fn new(vertices: Vec<Vertex>, indices: Vec<u32>, textures: Vec<Rc<Texture>>) -> Self {
            let vao = VertexArray::new();

            unsafe {
                gl::BindVertexArray(vao.vertex_array);

                gl::BindBuffer(gl::ARRAY_BUFFER, vao.vertex_buffer);
                gl::BufferData(gl::ARRAY_BUFFER, (size_of::<Vertex>() * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, vao.index_buffer);
                gl::BufferData(
                    gl::ELEMENT_ARRAY_BUFFER,
                    (size_of::<u32>() * indices.len()) as isize,
                    indices.as_ptr().cast(),
                    gl::STATIC_DRAW,
                );

                gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ptr::null());
                gl::EnableVertexAttribArray(0);

                gl::VertexAttribPointer(1, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((3 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(1);

                gl::VertexAttribPointer(2, 2, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((6 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(2);

                gl::VertexAttribPointer(3, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((8 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(3);

                gl::VertexAttribPointer(4, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((11 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(4);

                gl::VertexAttribIPointer(5, 4, gl::INT, size_of::<Vertex>() as i32, ((14 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(5);

                gl::VertexAttribPointer(6, 4, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((18 * FLOAT_SIZE) as *const usize).cast());
                gl::EnableVertexAttribArray(6);

                gl::BindVertexArray(0);
            }

            Mesh { indices, textures, vao }
        }

        pub fn draw(&self, shader: &Program) {
            let mut diffuse = 1u32;
            let mut specular = 1u32;
            let mut normal = 1u32;
            let mut height = 1u32;

            unsafe {
                for i in 0..self.textures.len() {
                    gl::ActiveTexture(gl::TEXTURE0 + i as u32);

                    let name = self.textures[i].r#type.as_ref();
                    let number = (match name {
                        "texture_diffuse" => {
                            diffuse += 1;
                            diffuse - 1
                        }
                        "texture_specular" => {
                            specular += 1;
                            specular - 1
                        }
                        "texture_normal" => {
                            normal += 1;
                            normal - 1
                        }
                        "texture_height" => {
                            height += 1;
                            height - 1
                        }
                        _ => {
                            panic!("Unexpected texture type: {name}");
                        }
                    })
                    .to_string();

                    with_c_str(&(name.to_string() + &number), |locn| {
                        gl::Uniform1i(gl::GetUniformLocation(shader.program(), locn), i as i32)
                    });
                    gl::BindTexture(gl::TEXTURE_2D, self.textures[i].texture);
                }

                gl::BindVertexArray(self.vao.vertex_array);
                gl::DrawElements(gl::TRIANGLES, self.indices.len() as i32, gl::UNSIGNED_INT, ptr::null());

                gl::ActiveTexture(gl::TEXTURE0);
            }
        }

        pub fn draw_instanced(&self, amount: i32) {
            unsafe {
                gl::BindVertexArray(self.vao.vertex_array);
                gl::DrawElementsInstanced(gl::TRIANGLES, self.indices.len() as i32, gl::UNSIGNED_INT, ptr::null(), amount);
                gl::BindVertexArray(0);
            }
        }
    }

    const INDEX_COUNT: usize = 36;
    const INDICES: [u32; INDEX_COUNT] = [
        0, 1, 2, 2, 1, 3, // first quad
        4, 5, 6, 6, 5, 7, // second quad
        8, 9, 10, 10, 9, 11, // third quad
        12, 13, 14, 14, 13, 15, // fourth quad
        16, 17, 18, 18, 17, 19, // fifth quad
        20, 21, 22, 22, 21, 23, // sixth quad
    ];

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

            let vertices: Vec<f32> = vec![-1.0, -1.0, 0.0, 0.0, 0.0, 1.0, -1.0, 0.0, 1.0, 0.0, -1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0];
            let indices = Vec::from(&INDICES[..6]);

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

        pub fn new_cube() -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            let vertices: Vec<f32> = vec![
                // back face
                1.0, -1.0, -1.0, 1.0, 0.0, // bottom-right
                -1.0, -1.0, -1.0, 0.0, 0.0, // bottom-left
                1.0, 1.0, -1.0, 1.0, 1.0, // top-right
                -1.0, 1.0, -1.0, 0.0, 1.0, // top-left
                // front face
                -1.0, -1.0, 1.0, 0.0, 0.0, // bottom-left
                1.0, -1.0, 1.0, 1.0, 0.0, // bottom-right
                -1.0, 1.0, 1.0, 0.0, 1.0, // top-left
                1.0, 1.0, 1.0, 1.0, 1.0, // top-right
                // left face
                -1.0, -1.0, -1.0, 0.0, 1.0, // bottom-left
                -1.0, -1.0, 1.0, 0.0, 0.0, // bottom-right
                -1.0, 1.0, -1.0, 1.0, 1.0, // top-left
                -1.0, 1.0, 1.0, 1.0, 0.0, // top-right
                // right face
                1.0, -1.0, 1.0, 0.0, 0.0, // bottom-left
                1.0, -1.0, -1.0, 0.0, 1.0, // bottom-right
                1.0, 1.0, 1.0, 1.0, 0.0, // top-left
                1.0, 1.0, -1.0, 1.0, 1.0, // top-right
                // bottom face
                1.0, -1.0, 1.0, 1.0, 0.0, // bottom-left
                -1.0, -1.0, 1.0, 0.0, 0.0, // bottom-right
                1.0, -1.0, -1.0, 1.0, 1.0, // top-left
                -1.0, -1.0, -1.0, 0.0, 1.0, // top-right
                // top face
                -1.0, 1.0, 1.0, 0.0, 0.0, // bottom-left
                1.0, 1.0, 1.0, 1.0, 0.0, // bottom-right
                -1.0, 1.0, -1.0, 0.0, 1.0, // top-left
                1.0, 1.0, -1.0, 1.0, 1.0, // top-right
            ];
            let indices = Vec::from(&INDICES[..]);

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
                let datau8 = unsafe { bindgen::stbi_load_from_memory(contents.as_mut_ptr(), contents.len() as i32, &mut width, &mut height, &mut components, 4) };

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
        r#type: String,
        pub path: String,
    }

    impl Texture {
        pub fn new(r#type: String, path: String) -> Self {
            let mut texture = 0;

            unsafe {
                gl::GenTextures(1, &mut texture);
            }

            Texture { texture, r#type, path }
        }

        pub fn load(self, path: &str) -> Self {
            let texture = self.texture;

            let image = Image::new(path, false);

            let format = match image.components {
                1 => gl::RED,
                3 => gl::RGB,
                4 => gl::RGBA,
                _ => panic!("Unexpected image format {}", image.components),
            };

            if image.datau8 == ptr::null_mut() {
                println!("Failed to load texture");
            } else {
                unsafe {
                    gl::BindTexture(gl::TEXTURE_2D, texture);
                    gl::TexImage2D(
                        gl::TEXTURE_2D,
                        0,
                        format as i32,
                        image.width,
                        image.height,
                        0,
                        format,
                        gl::UNSIGNED_BYTE,
                        image.datau8.cast(),
                    );
                    gl::GenerateMipmap(gl::TEXTURE_2D);

                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32);

                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR_MIPMAP_LINEAR as i32);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
                }
            }

            self
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
