pub mod model {
    use crate::{Mesh, Program, Texture};
    use glam::{Mat4, Vec2, Vec3};
    use russimp::{
        material::Material,
        mesh,
        node::Node,
        scene::{PostProcess, Scene},
        texture::TextureType,
        Matrix4x4,
    };
    use std::{collections::HashMap, rc::Rc};

    const MAX_BONE_INFLUENCE: usize = 4usize;

    #[repr(C)]
    #[derive(Clone, Debug, PartialEq)]
    pub struct Vertex {
        pub position: Vec3,
        pub normal: Vec3,
        pub tex_coords: Vec2,
        pub tangent: Vec3,
        pub bitangent: Vec3,
        pub bone_ids: [i32; MAX_BONE_INFLUENCE],
        pub weights: [f32; MAX_BONE_INFLUENCE],
    }

    #[derive(Clone, Debug)]
    pub struct BoneInfo {
        pub id: i32,
        pub offset: Mat4,
    }

    pub struct Model {
        pub textures_loaded: Vec<Rc<Texture>>,
        pub meshes: Vec<Mesh>,
        pub directory: String,
        pub gamma_correction: bool,
        pub bone_info_map: HashMap<String, BoneInfo>,
        pub bone_counter: i32,
    }

    impl Model {
        pub fn new(gamma_correction: bool) -> Self {
            Model {
                textures_loaded: Vec::new(),
                meshes: Vec::new(),
                directory: "".to_string(),
                gamma_correction,
                bone_info_map: HashMap::new(),
                bone_counter: 0,
            }
        }

        pub fn load_model(mut self, path: &str) -> Self {
            let scene = Scene::from_file(path, vec![PostProcess::Triangulate, PostProcess::GenerateSmoothNormals, PostProcess::CalculateTangentSpace]).unwrap();

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
                    bone_ids: [-1; 4],
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

            self.extract_bone_weight_for_vertices(&mut vertices, mesh);

            Mesh::new(vertices, indices, textures)
        }

        fn set_vertex_bone_data(vertex: &mut Vertex, bone_id: i32, weight: f32) {
            for i in 0..MAX_BONE_INFLUENCE {
                if vertex.bone_ids[i] < 0 {
                    vertex.weights[i] = weight;
                    vertex.bone_ids[i] = bone_id;
                    break;
                }
            }
        }

        fn extract_bone_weight_for_vertices(&mut self, vertices: &mut Vec<Vertex>, mesh: &mesh::Mesh) {
            for bone in &mesh.bones {
                let bone_id: i32;
                let bone_name = &bone.name;
                if !self.bone_info_map.contains_key(bone_name) {
                    bone_id = self.bone_counter;
                    let offset = convert_matrix(bone.offset_matrix);
                    self.bone_info_map.insert(bone_name.to_string(), BoneInfo { id: bone_id, offset });
                    self.bone_counter += 1;
                } else {
                    bone_id = self.bone_info_map[bone_name].id;
                }
                let weights = &bone.weights;

                for weight in weights {
                    let vertex_id = weight.vertex_id as usize;
                    assert!(vertex_id < vertices.len());
                    Model::set_vertex_bone_data(&mut vertices[vertex_id], bone_id, weight.weight);
                }
            }
        }

        fn load_material_textures(&mut self, material: &Material, tex_type: TextureType, type_name: &str) -> Vec<Rc<Texture>> {
            let mut textures = Vec::new();

            if let Some(vector) = material.textures.get(&tex_type) {
                for texture in vector {
                    let matches = self.textures_loaded.iter().filter(|t| texture.path == t.path).collect::<Vec<_>>();

                    if matches.len() > 0 {
                        textures.push(Rc::clone(*matches.get(0).unwrap()));
                    } else {
                        let tex_path = self.directory.clone() + &("/".to_string() + &texture.path);
                        let mesh_tex = Rc::new(Texture::new(type_name.to_string(), (&texture.path).to_string()).load(&tex_path));

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

    pub fn convert_matrix(mx: Matrix4x4) -> Mat4 {
        Mat4::from_cols_slice(&[
            mx.a1, mx.b1, mx.c1, mx.d1, mx.a2, mx.b2, mx.c2, mx.d2, mx.a3, mx.b3, mx.c3, mx.d3, mx.a4, mx.b4, mx.c4, mx.d4,
        ])
    }
}
