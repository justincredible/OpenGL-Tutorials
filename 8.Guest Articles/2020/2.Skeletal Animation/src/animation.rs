pub mod animation {
    use crate::{convert_matrix, Bone, BoneInfo, Model};
    use glam::Mat4;
    use russimp::{
        animation,
        node::Node,
        scene::{PostProcess, Scene},
    };
    use std::{collections::HashMap, mem};

    pub struct Animator {
        final_bone_matrices: Vec<Mat4>,
        current_animation: Animation,
        pub current_time: f32,
        delta_time: f32,
    }

    macro_rules! fmod {
        ($x:expr, $y:expr) => {
            f32::max(0.0, $x - f32::round($x / $y) * $y)
        };
    }

    impl Animator {
        pub fn new(current_animation: Animation) -> Self {
            let mut final_bone_matrices = Vec::new();
            for _ in 0..100 {
                final_bone_matrices.push(Mat4::IDENTITY);
            }

            Animator {
                final_bone_matrices,
                current_animation,
                current_time: 0.0,
                delta_time: 0.0,
            }
        }

        pub fn update_animation(&mut self, dt: f32) {
            self.delta_time = dt;

            self.current_time += self.current_animation.ticks_per_second as f32 * dt;
            self.current_time = fmod!(self.current_time, self.current_animation.duration as f32);
            let root_node = mem::take(&mut self.current_animation.root_node).unwrap();
            self.calculate_bone_transform(&root_node, Mat4::IDENTITY);
            self.current_animation.root_node = Some(root_node);
        }

        pub fn calculate_bone_transform(&mut self, node: &RussimpNodeData, parent_transform: Mat4) {
            let node_name = &node.name;
            let mut node_transform = node.transformation;

            let mut bone = self.current_animation.find_bone(node_name);

            if let Some(bone) = &mut bone {
                bone.update(self.current_time);
                node_transform = bone.local_transform();
            }

            let global_transformation = parent_transform * node_transform;

            let bone_info = self.current_animation.bone_id_map();
            if bone_info.contains_key(node_name) {
                let index = bone_info[node_name].id;
                let offset = bone_info[node_name].offset;
                self.final_bone_matrices[index as usize] = global_transformation * offset;
            }

            for node in &node.children {
                self.calculate_bone_transform(node, global_transformation);
            }
        }

        pub fn final_bone_matrices(&self) -> &Vec<Mat4> {
            &self.final_bone_matrices
        }
    }

    #[derive(Debug)]
    pub struct RussimpNodeData {
        transformation: Mat4,
        name: String,
        children_count: i32,
        children: Vec<RussimpNodeData>,
    }

    pub struct Animation {
        duration: f32,
        ticks_per_second: i32,
        bones: Vec<Bone>,
        root_node: Option<RussimpNodeData>,
        bone_info_map: HashMap<String, BoneInfo>,
    }

    impl Animation {
        pub fn new(path: &str, model: &Model) -> Self {
            let scene = Scene::from_file(path, vec![PostProcess::Triangulate, PostProcess::FixOrRemoveInvalidData]).unwrap();

            assert!(scene.root.is_some());
            let animation = &scene.animations[0];
            // issue with Blender timeline to AssImp duration
            let duration = 2.0 * animation.duration as f32;
            let ticks_per_second = animation.ticks_per_second as i32;
            let _global_transformation = convert_matrix(scene.root.as_ref().unwrap().borrow().transformation).inverse();

            let root_node = None;
            let mut anim = Animation {
                duration,
                ticks_per_second,
                bones: Vec::new(),
                root_node,
                bone_info_map: HashMap::new(),
            };

            let mut root_node = RussimpNodeData {
                transformation: Mat4::ZERO,
                name: "".to_string(),
                children_count: 0,
                children: Vec::new(),
            };
            anim.read_heirarchy_data(&mut root_node, &*scene.root.as_ref().unwrap().borrow());
            anim.root_node = Some(root_node);
            anim.read_missing_bones(&animation, model);

            anim
        }

        pub fn find_bone(&mut self, name: &str) -> Option<&mut Bone> {
            for bone in self.bones.iter_mut() {
                if bone.bone_name() == name {
                    return Some(bone);
                }
            }
            None
        }

        pub fn ticks_per_second(&self) -> i32 {
            self.ticks_per_second
        }

        pub fn duration(&self) -> f32 {
            self.duration
        }

        pub fn root_node(&self) -> &RussimpNodeData {
            self.root_node.as_ref().unwrap()
        }

        pub fn bone_id_map(&self) -> &HashMap<String, BoneInfo> {
            &self.bone_info_map
        }

        fn read_missing_bones(&mut self, animation: &animation::Animation, model: &Model) {
            let mut bone_info_map = model.bone_info_map.clone();
            let mut bone_count = model.bone_counter;

            for channel in &animation.channels {
                let bone_name = &channel.name;

                if !bone_info_map.contains_key(bone_name) {
                    bone_info_map.insert(
                        bone_name.to_string(),
                        BoneInfo {
                            id: bone_count,
                            offset: Mat4::IDENTITY,
                        },
                    );
                    bone_count += 1;
                }
                self.bones.push(Bone::new(bone_name.to_string(), bone_info_map[bone_name].id, &channel));
            }
            self.bone_info_map = bone_info_map;
        }

        fn read_heirarchy_data(&mut self, dest: &mut RussimpNodeData, src: &Node) {
            dest.name = src.name.clone();
            dest.transformation = convert_matrix(src.transformation);
            dest.children_count = src.children.len() as i32;

            for node in &src.children {
                let mut new_data = RussimpNodeData {
                    transformation: Mat4::ZERO,
                    name: "".to_string(),
                    children_count: 0,
                    children: Vec::new(),
                };
                self.read_heirarchy_data(&mut new_data, &*node.borrow());
                dest.children.push(new_data);
            }
        }
    }
}
