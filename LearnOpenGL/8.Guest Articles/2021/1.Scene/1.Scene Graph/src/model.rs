pub mod model {
    use crate::{Mesh, Program, Texture};
    use glam::{Vec2, Vec3};
    use russimp::{
        material::Material,
        mesh,
        node::Node,
        scene::{PostProcess, Scene},
        texture::TextureType,
    };
    use std::rc::Rc;

    const MAX_BONE_INFLUENCE: usize = 4usize;

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

    pub struct Model {
        textures_loaded: Vec<Rc<Texture>>,
        pub meshes: Vec<Mesh>,
        directory: String,
    }

    impl Model {
        pub fn new() -> Self {
            Model {
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
            let mut vertices: Vec<Vertex> = Vec::new();
            let mut indices: Vec<u32> = Vec::new();
            let mut textures: Vec<Rc<Texture>> = Vec::new();

            for i in 0..mesh.vertices.len() {
                let vector = mesh.vertices[i];
                let position = Vec3::new(vector.x, vector.y, vector.z);

                let normal = if i < mesh.normals.len() {
                    let vector = mesh.normals[i];
                    Vec3::new(vector.x, vector.y, vector.z)
                } else {
                    Vec3::ZERO
                };

                let (tex_coords, tangent, bitangent) = if mesh.texture_coords.len() > 0 {
                    let vector = mesh.texture_coords[0].as_ref().unwrap()[i];
                    let tex_coords = Vec2::new(vector.x, vector.y);
                    let vector = mesh.tangents[i];
                    let tangent = Vec3::new(vector.x, vector.y, vector.z);
                    let vector = mesh.bitangents[i];
                    let bitangent = Vec3::new(vector.x, vector.y, vector.z);

                    (tex_coords, tangent, bitangent)
                } else {
                    (Vec2::ZERO, Vec3::ZERO, Vec3::ZERO)
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

            Mesh::new(vertices, indices, textures)
        }

        fn load_material_textures(&mut self, material: &Material, tex_type: TextureType, type_name: &str) -> Vec<Rc<Texture>> {
            let mut textures = Vec::new();

            if let Some(vector) = material.textures.get(&tex_type) {
                for texture in vector {
                    let matches = self.textures_loaded.iter().filter(|t| texture.path == t.path).collect::<Vec<_>>();

                    if matches.len() > 0 {
                        textures.push(Rc::clone(matches[0]));
                    } else {
                        let tex_path = self.directory.clone() + &("/".to_string() + &texture.path);
                        let mesh_tex = Rc::new(Texture::new(type_name.to_string(), texture.path.clone()).load(&tex_path));

                        textures.push(Rc::clone(&mesh_tex));
                        self.textures_loaded.push(mesh_tex);
                    }
                }
            }

            textures
        }

        pub fn draw(&self, shader: &Program) {
            for mesh in &self.meshes {
                mesh.draw(shader)
            }
        }

        pub fn bind_texture(&self) {
            unsafe {
                gl::ActiveTexture(gl::TEXTURE0);
                gl::BindTexture(gl::TEXTURE_2D, self.textures_loaded[0].texture());
            }
        }
    }
}
