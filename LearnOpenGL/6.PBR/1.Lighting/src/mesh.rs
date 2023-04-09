pub mod mesh {
    use crate::{gl, ptr, size_of, File, Program, Rc, Read, Vec2, Vec3, PI};

    const MAX_BONE_INFLUENCE: usize = 4;
    const FLOAT_SIZE: usize = size_of::<f32>();
    const U32_SIZE: usize = size_of::<u32>();

    #[repr(C)]
    pub struct Vertex {
        pub position: Vec3,
        pub normal: Vec3,
        pub tex_coords: Vec2,
        pub tangent: Vec3,
        pub bitangent: Vec3,
        pub bone_ids: [i32; MAX_BONE_INFLUENCE],
        pub weights: [f32; MAX_BONE_INFLUENCE],
    }

    pub struct Mesh {
        gl: Rc<gl::Gl>,
        indices: Vec<u32>,
        textures: Vec<Rc<Texture>>,
        pub vao: VertexArray,
    }

    impl Mesh {
        pub fn new(gl: Rc<gl::Gl>, vertices: Vec<Vertex>, indices: Vec<u32>, textures: Vec<Rc<Texture>>) -> Self {
            let vao = VertexArray::new(Rc::clone(&gl));

            unsafe {
                gl.BindVertexArray(vao.vertex_array);

                gl.BindBuffer(gl::ARRAY_BUFFER, vao.vertex_buffer);
                gl.BufferData(gl::ARRAY_BUFFER, (size_of::<Vertex>() * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl.BindBuffer(gl::ELEMENT_ARRAY_BUFFER, vao.index_buffer);
                gl.BufferData(
                    gl::ELEMENT_ARRAY_BUFFER,
                    (size_of::<u32>() * indices.len()) as isize,
                    indices.as_ptr().cast(),
                    gl::STATIC_DRAW,
                );

                gl.VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ptr::null());
                gl.EnableVertexAttribArray(0);

                gl.VertexAttribPointer(1, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((3 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(1);

                gl.VertexAttribPointer(2, 2, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((6 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(2);

                gl.VertexAttribPointer(3, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((8 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(3);

                gl.VertexAttribPointer(4, 3, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((11 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(4);

                gl.VertexAttribIPointer(5, 4, gl::INT, size_of::<Vertex>() as i32, ((14 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(5);

                gl.VertexAttribPointer(6, 4, gl::FLOAT, gl::FALSE, size_of::<Vertex>() as i32, ((18 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(6);

                gl.BindVertexArray(0);
            }

            Mesh { gl, indices, textures, vao }
        }

        pub fn draw(&self, shader: &Program) {
            let gl = &self.gl;

            let mut diffuse = 1u32;
            let mut specular = 1u32;
            let mut normal = 1u32;
            let mut height = 1u32;

            unsafe {
                for i in 0..self.textures.len() {
                    gl.ActiveTexture(gl::TEXTURE0 + i as u32);

                    let name = self.textures[i].tex_type.as_ref();
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

                    gl.Uniform1i(gl.GetUniformLocation(shader.program(), gl::c_named(name, &number).as_ptr()), i as i32);
                    gl.BindTexture(gl::TEXTURE_2D, self.textures[i].texture);
                }

                gl.BindVertexArray(self.vao.vertex_array);
                gl.DrawElements(gl::TRIANGLES, self.indices.len() as i32, gl::UNSIGNED_INT, ptr::null());

                gl.ActiveTexture(gl::TEXTURE0);
            }
        }

        pub fn draw_instanced(&self, amount: i32) {
            unsafe {
                self.gl.BindVertexArray(self.vao.vertex_array);
                self.gl
                    .DrawElementsInstanced(gl::TRIANGLES, self.indices.len() as i32, gl::UNSIGNED_INT, ptr::null(), amount);
                self.gl.BindVertexArray(0);
            }
        }
    }

    pub struct VertexArray {
        gl: Rc<gl::Gl>,
        vertex_array: gl::types::GLuint,
        vertex_buffer: gl::types::GLuint,
        index_buffer: gl::types::GLuint,
        index_count: usize,
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
            }

            VertexArray {
                gl,
                vertex_array,
                vertex_buffer,
                index_buffer,
                index_count: 0,
            }
        }

        pub fn new_sphere(gl: Rc<gl::Gl>) -> Self {
            let mut vertex_array = 0;
            let mut vertex_buffer = 0;
            let mut index_buffer = 0;

            let mut positions: Vec<Vec3> = Vec::new();
            let mut uvs: Vec<Vec2> = Vec::new();
            let mut normals: Vec<Vec3> = Vec::new();
            let mut indices: Vec<u32> = Vec::new();

            const X_SEGMENTS: usize = 64usize;
            const Y_SEGMENTS: usize = 64usize;

            for x in 0..=X_SEGMENTS {
                for y in 0..=Y_SEGMENTS {
                    let x_segment = x as f32 / X_SEGMENTS as f32;
                    let y_segment = y as f32 / Y_SEGMENTS as f32;
                    let x_pos = f32::cos(x_segment * 2.0 * PI) * f32::sin(y_segment * PI);
                    let y_pos = f32::cos(y_segment * PI);
                    let z_pos = f32::sin(x_segment * 2.0 * PI) * f32::sin(y_segment * PI);

                    positions.push(Vec3::new(x_pos, y_pos, z_pos));
                    uvs.push(Vec2::new(x_segment, y_segment));
                    normals.push(Vec3::new(x_pos, y_pos, z_pos));
                }
            }
            let mut odd_row = false;
            for y in 0..Y_SEGMENTS {
                if !odd_row {
                    for x in 0..=X_SEGMENTS {
                        indices.push((y * (X_SEGMENTS + 1) + x) as u32);
                        indices.push(((y + 1) * (X_SEGMENTS + 1) + x) as u32);
                    }
                } else {
                    for x in (0..=X_SEGMENTS).rev() {
                        indices.push(((y + 1) * (X_SEGMENTS + 1) + x) as u32);
                        indices.push((y * (X_SEGMENTS + 1) + x) as u32);
                    }
                }
                odd_row = !odd_row;
            }

            let mut vertices = Vec::new();
            for i in 0..positions.len() {
                let position = positions.get(i).unwrap();
                let normal = normals.get(i).unwrap();
                let uv = uvs.get(i).unwrap();

                vertices.push(position.x);
                vertices.push(position.y);
                vertices.push(position.z);
                vertices.push(normal.x);
                vertices.push(normal.y);
                vertices.push(normal.z);
                vertices.push(uv.x);
                vertices.push(uv.y);
            }

            unsafe {
                gl.GenVertexArrays(1, &mut vertex_array);
                gl.GenBuffers(1, &mut vertex_buffer);
                gl.GenBuffers(1, &mut index_buffer);

                gl.BindVertexArray(vertex_array);

                gl.BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
                gl.BufferData(gl::ARRAY_BUFFER, (FLOAT_SIZE * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                let stride = (FLOAT_SIZE * 8) as i32;
                gl.VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, stride, ptr::null());
                gl.EnableVertexAttribArray(0);

                gl.VertexAttribPointer(1, 3, gl::FLOAT, gl::FALSE, stride, ((3 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(1);

                gl.VertexAttribPointer(2, 2, gl::FLOAT, gl::FALSE, stride, ((6 * FLOAT_SIZE) as *const usize).cast());
                gl.EnableVertexAttribArray(2);

                gl.BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
                gl.BufferData(gl::ELEMENT_ARRAY_BUFFER, (indices.len() * U32_SIZE) as isize, indices.as_ptr().cast(), gl::STATIC_DRAW);

                gl.BindVertexArray(0);
            }

            VertexArray {
                gl,
                vertex_array,
                vertex_buffer,
                index_buffer,
                index_count: indices.len(),
            }
        }

        pub fn bind(&self) {
            unsafe {
                self.gl.BindVertexArray(self.vertex_array);
            }
        }

        pub fn draw(&self) {
            unsafe {
                self.gl.DrawElements(gl::TRIANGLE_STRIP, self.index_count as i32, gl::UNSIGNED_INT, ptr::null());
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

    use stb_image::stb_image::bindgen;

    pub fn stbi_flip_vertical(flip: bool) {
        unsafe {
            bindgen::stbi_set_flip_vertically_on_load(flip as i32);
        }
    }

    struct Image {
        data: *mut u8,
        width: i32,
        height: i32,
        components: i32,
    }

    impl Image {
        pub fn new(path: &str) -> Self {
            let mut file = File::open(path).unwrap();
            let mut contents = vec![];

            file.read_to_end(&mut contents).unwrap();

            let mut width = 0;
            let mut height = 0;
            let mut components = 0;

            let data = unsafe { bindgen::stbi_load_from_memory(contents.as_mut_ptr(), contents.len() as i32, &mut width, &mut height, &mut components, 0) };

            Image { data, width, height, components }
        }
    }

    impl Drop for Image {
        fn drop(&mut self) {
            unsafe {
                bindgen::stbi_image_free(self.data.cast());
            }
        }
    }

    pub struct Texture {
        gl: Rc<gl::Gl>,
        pub tex_type: String,
        pub tex_path: String,
        pub texture: gl::types::GLuint,
    }

    impl Texture {
        pub fn new(gl: Rc<gl::Gl>, tex_type: &str, tex_path: &str) -> Self {
            let mut texture = 0;

            unsafe {
                gl.GenTextures(1, &mut texture);
            }

            Texture {
                gl,
                tex_type: tex_type.to_string(),
                tex_path: tex_path.to_string(),
                texture,
            }
        }

        pub fn load(self, path: &str, gamma_correction: bool) -> Self {
            let gl = &self.gl;
            let texture = self.texture;

            let image = Image::new(path);

            let (internal_format, data_format) = match image.components {
                1 => (gl::RED, gl::RED),
                3 => (if gamma_correction { gl::SRGB } else { gl::RGB }, gl::RGB),
                4 => (if gamma_correction { gl::SRGB_ALPHA } else { gl::RGBA }, gl::RGBA),
                _ => panic!("Unexpected image format"),
            };

            if image.data == ptr::null_mut() {
                println!("Failed to load texture");
            } else {
                unsafe {
                    gl.BindTexture(gl::TEXTURE_2D, texture);
                    gl.TexImage2D(
                        gl::TEXTURE_2D,
                        0,
                        internal_format as i32,
                        image.width,
                        image.height,
                        0,
                        data_format,
                        gl::UNSIGNED_BYTE,
                        image.data.cast(),
                    );
                    gl.GenerateMipmap(gl::TEXTURE_2D);

                    gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32);
                    gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32);

                    gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR_MIPMAP_LINEAR as i32);
                    gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
                }
            }

            self
        }

        pub fn load_noise(self, noise: Vec<f32>) -> Self {
            let gl = &self.gl;

            unsafe {
                gl.BindTexture(gl::TEXTURE_2D, self.texture);
                gl.TexImage2D(gl::TEXTURE_2D, 0, gl::RGBA32F as i32, 4, 4, 0, gl::RGB, gl::FLOAT, noise.as_ptr().cast());
                gl.GenerateMipmap(gl::TEXTURE_2D);

                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32);
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32);

                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::NEAREST as i32);
                gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::NEAREST as i32);
            }

            self
        }

        pub fn bind(&self) {
            unsafe {
                self.gl.BindTexture(gl::TEXTURE_2D, self.texture);
            }
        }
    }

    impl Drop for Texture {
        fn drop(&mut self) {
            unsafe {
                self.gl.DeleteTextures(1, &self.texture);
            }
        }
    }
}