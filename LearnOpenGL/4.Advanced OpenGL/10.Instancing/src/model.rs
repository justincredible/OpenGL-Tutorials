pub mod model {
    use crate::{gl, ptr, size_of, Mat4, Mesh, Program, Rc, Texture, Vec2, Vec3, Vertex};
    use russimp::{
        material::Material,
        mesh,
        node::Node,
        scene::{PostProcess, Scene},
        texture::TextureType,
    };

    pub struct Model {
        gl: Rc<gl::Gl>,
        textures_loaded: Vec<Rc<Texture>>,
        pub meshes: Vec<Mesh>,
        directory: String,
    }

    impl Model {
        pub fn new(gl: Rc<gl::Gl>) -> Self {
            Model {
                gl,
                textures_loaded: vec![],
                meshes: vec![],
                directory: "".to_string(),
            }
        }

        pub fn load_model(mut self, path: &str) -> Self {
            let scene = Scene::from_file(
                path,
                vec![
                    PostProcess::Triangulate,
                    PostProcess::GenerateSmoothNormals,
                    PostProcess::FlipUVs,
                    PostProcess::CalculateTangentSpace,
                ],
            )
            .unwrap();

            if scene.flags > 0 || scene.root.is_none() {
                panic!("Failed to load scene.");
            }

            self.directory = path.rsplit_once("/").unwrap().0.to_string();

            self.process_node(&*scene.root.as_ref().unwrap().borrow(), &scene);

            self
        }

        fn process_node(&mut self, node: &Node, scene: &Scene) {
            for mesh in &node.meshes {
                let mesh = self.process_mesh(scene.meshes.get(*mesh as usize).unwrap(), scene);
                self.meshes.push(mesh);
            }

            for node in &node.children {
                self.process_node(&*node.borrow(), scene);
            }
        }

        fn process_mesh(&mut self, mesh: &mesh::Mesh, scene: &Scene) -> Mesh {
            let mut vertices: Vec<Vertex> = vec![];
            let mut indices: Vec<u32> = vec![];
            let mut textures: Vec<Rc<Texture>> = vec![];

            for i in 0..mesh.vertices.len() {
                let vector = mesh.vertices.get(i).unwrap();
                let position = Vec3::new(vector.x, vector.y, vector.z);

                let normal = if i < mesh.normals.len() {
                    let vector = mesh.normals.get(i).unwrap();
                    Vec3::new(vector.x, vector.y, vector.z)
                } else {
                    Vec3::zero()
                };

                let (tex_coords, tangent, bitangent) = if mesh.texture_coords.len() > 0 {
                    let vector = mesh.texture_coords.get(0).unwrap().as_ref().unwrap().get(i).unwrap();
                    let tex_coords = Vec2::new(vector.x, vector.y);
                    let vector = mesh.tangents.get(i).unwrap();
                    let tangent = Vec3::new(vector.x, vector.y, vector.z);
                    let vector = mesh.bitangents.get(i).unwrap();
                    let bitangent = Vec3::new(vector.x, vector.y, vector.z);

                    (tex_coords, tangent, bitangent)
                } else {
                    (Vec2::zero(), Vec3::zero(), Vec3::zero())
                };

                vertices.push(Vertex {
                    position,
                    normal,
                    tex_coords,
                    tangent,
                    bitangent,
                    bone_ids: [0; 4],
                    weights: [0.0; 4],
                });
            }

            for face in &mesh.faces {
                for index in &face.0 {
                    indices.push(*index);
                }
            }

            let material = scene.materials.get(mesh.material_index as usize).unwrap();

            let mut diffuse_maps = self.load_material_textures(material, TextureType::Diffuse, "texture_diffuse");
            textures.append(&mut diffuse_maps);
            let mut specular_maps = self.load_material_textures(material, TextureType::Specular, "texture_specular");
            textures.append(&mut specular_maps);
            let mut normal_maps = self.load_material_textures(material, TextureType::Height, "texture_normal");
            textures.append(&mut normal_maps);
            let mut height_maps = self.load_material_textures(material, TextureType::Ambient, "texture_height");
            textures.append(&mut height_maps);

            Mesh::new(Rc::clone(&self.gl), vertices, indices, textures)
        }

        fn load_material_textures(&mut self, material: &Material, tex_type: TextureType, type_name: &str) -> Vec<Rc<Texture>> {
            let mut textures = vec![];

            for vector in material.textures.get(&tex_type) {
                for texture in vector {
                    let matches = self.textures_loaded.iter().filter(|t| texture.path == t.tex_path).collect::<Vec<_>>();

                    if matches.len() > 0 {
                        textures.push(Rc::clone(*matches.get(0).unwrap()));
                    } else {
                        let tex_path = self.directory.clone() + &("/".to_string() + &texture.path);
                        let mesh_tex = Rc::new(Texture::new(Rc::clone(&self.gl), type_name, &texture.path).load(&tex_path));

                        textures.push(Rc::clone(&mesh_tex));
                        self.textures_loaded.push(mesh_tex);
                    }
                }
            }

            textures
        }

        pub fn draw(&self, shader: &Program) {
            for i in 0..self.meshes.len() {
                self.meshes[i].draw(shader)
            }
        }

        pub fn bind_texture(&self) {
            unsafe {
                self.gl.ActiveTexture(gl::TEXTURE0);
                self.gl.BindTexture(gl::TEXTURE_2D, self.textures_loaded.get(0).unwrap().texture);
            }
        }

        pub fn instance_meshes(&self) {
            for mesh in &self.meshes {
                let gl = &self.gl;
                let float_size = size_of::<f32>();
                let matrix_size = size_of::<Mat4>() as i32;

                unsafe {
                    gl.BindVertexArray(mesh.vao.vertex_array);
                    gl.EnableVertexAttribArray(3);
                    gl.VertexAttribPointer(3, 4, gl::FLOAT, gl::FALSE, matrix_size, ptr::null());
                    gl.EnableVertexAttribArray(4);
                    gl.VertexAttribPointer(4, 4, gl::FLOAT, gl::FALSE, matrix_size, ((4 * float_size) as *const usize).cast());
                    gl.EnableVertexAttribArray(5);
                    gl.VertexAttribPointer(5, 4, gl::FLOAT, gl::FALSE, matrix_size, ((8 * float_size) as *const usize).cast());
                    gl.EnableVertexAttribArray(6);
                    gl.VertexAttribPointer(6, 4, gl::FLOAT, gl::FALSE, matrix_size, ((12 * float_size) as *const usize).cast());

                    gl.VertexAttribDivisor(3, 1);
                    gl.VertexAttribDivisor(4, 1);
                    gl.VertexAttribDivisor(5, 1);
                    gl.VertexAttribDivisor(6, 1);

                    gl.BindVertexArray(0);
                }
            }
        }
    }
}
