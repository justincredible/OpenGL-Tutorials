pub mod bone {
    use glam::{Mat4, Quat, Vec3};
    use russimp::animation::NodeAnim;

    struct KeyPosition {
        position: Vec3,
        time_stamp: f32,
    }

    struct KeyRotation {
        orientation: Quat,
        time_stamp: f32,
    }

    struct KeyScale {
        scale: Vec3,
        time_stamp: f32,
    }

    pub struct Bone {
        positions: Vec<KeyPosition>,
        rotations: Vec<KeyRotation>,
        scales: Vec<KeyScale>,
        local_transform: Mat4,
        name: String,
        id: i32,
    }

    impl Bone {
        pub fn new(name: String, id: i32, channel: &NodeAnim) -> Self {
            let local_transform = Mat4::IDENTITY;

            let mut positions = Vec::new();
            for position in &channel.position_keys {
                let pos = position.value;
                positions.push(KeyPosition {
                    position: Vec3::new(pos.x, pos.y, pos.z),
                    time_stamp: position.time as f32,
                });
            }

            let mut rotations = Vec::new();
            for rotation in &channel.rotation_keys {
                let quat = Quat::from_xyzw(rotation.value.x, rotation.value.y, rotation.value.z, rotation.value.w);
                let (axis, angle) = quat.to_axis_angle();
                rotations.push(KeyRotation {
                    orientation: Quat::from_axis_angle(Vec3::new(axis.x, axis.y, axis.z), angle),
                    time_stamp: rotation.time as f32,
                });
            }

            let mut scales = Vec::new();
            for scaling in &channel.scaling_keys {
                let scl = scaling.value;
                scales.push(KeyScale {
                    scale: Vec3::new(scl.x, scl.y, scl.z),
                    time_stamp: scaling.time as f32,
                });
            }

            Bone {
                positions,
                rotations,
                scales,
                local_transform,
                name,
                id,
            }
        }

        pub fn update(&mut self, animation_time: f32) {
            let translation = self.interpolate_position(animation_time);
            let rotation = self.interpolate_rotation(animation_time);
            let scale = self.interpolate_scaling(animation_time);
            self.local_transform = translation * rotation * scale;
        }

        pub fn local_transform(&self) -> Mat4 {
            self.local_transform
        }

        pub fn bone_name(&self) -> &String {
            &self.name
        }

        pub fn bone_id(&self) -> i32 {
            self.id
        }

        pub fn position_index(&self, animation_time: f32) -> i32 {
            for index in 0..self.positions.len() - 1 {
                if animation_time < self.positions[index + 1].time_stamp {
                    return index as i32;
                }
            }
            panic!("Incomplete animation.");
        }

        pub fn rotation_index(&self, animation_time: f32) -> i32 {
            for index in 0..self.rotations.len() - 1 {
                if animation_time < self.rotations[index + 1].time_stamp {
                    return index as i32;
                }
            }
            panic!("Incomplete animation.");
        }

        pub fn scale_index(&self, animation_time: f32) -> i32 {
            for index in 0..self.scales.len() - 1 {
                if animation_time < self.scales[index + 1].time_stamp {
                    return index as i32;
                }
            }
            panic!("Incomplete animation.");
        }

        fn scale_factor(last_time_stamp: f32, next_time_stamp: f32, animation_time: f32) -> f32 {
            let midway_length = animation_time - last_time_stamp;
            let frames_diff = next_time_stamp - last_time_stamp;

            midway_length / frames_diff
        }

        fn interpolate_position(&self, animation_time: f32) -> Mat4 {
            if self.positions.len() == 1 {
                return Mat4::from_translation(self.positions[0].position);
            }

            let p0_index = self.position_index(animation_time);
            let p1_index = p0_index + 1;
            let scale_factor = Bone::scale_factor(self.positions[p0_index as usize].time_stamp, self.positions[p1_index as usize].time_stamp, animation_time);
            let final_position = self.positions[p0_index as usize].position.lerp(self.positions[p1_index as usize].position, scale_factor);
            Mat4::from_translation(final_position)
        }

        fn interpolate_rotation(&self, animation_time: f32) -> Mat4 {
            if self.rotations.len() == 1 {
                return Mat4::from_quat(self.rotations[0].orientation);
            }

            let p0_index = self.rotation_index(animation_time);
            let p1_index = p0_index + 1;
            let scale_factor = Bone::scale_factor(self.rotations[p0_index as usize].time_stamp, self.rotations[p1_index as usize].time_stamp, animation_time);
            let final_rotation = self.rotations[p0_index as usize]
                .orientation
                .slerp(self.rotations[p1_index as usize].orientation, scale_factor)
                .normalize();
            Mat4::from_quat(final_rotation)
        }

        fn interpolate_scaling(&self, animation_time: f32) -> Mat4 {
            if self.scales.len() == 1 {
                return Mat4::from_scale(self.scales[0].scale);
            }

            let p0_index = self.scale_index(animation_time);
            let p1_index = p0_index + 1;
            let scale_factor = Bone::scale_factor(self.scales[p0_index as usize].time_stamp, self.scales[p1_index as usize].time_stamp, animation_time);
            let final_scale = self.scales[p0_index as usize].scale.lerp(self.scales[p1_index as usize].scale, scale_factor);
            Mat4::from_scale(final_scale)
        }
    }
}
