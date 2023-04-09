pub mod camera {
    use crate::{Mat4, Vec3, PI};

    pub enum Movement {
        Forward,
        Backward,
        Left,
        Right,
    }

    const YAW: f32 = -PI / 2.0;
    const PITCH: f32 = 0.0;
    const SPEED: f32 = 2.5;
    const SENSITIVITY: f32 = 0.01;
    const ZOOM: f32 = PI / 4.0;
    const BIAS: f32 = 0.1;

    pub struct Camera {
        position: Vec3,
        front: Vec3,
        up: Vec3,
        right: Vec3,
        world_up: Vec3,
        yaw: f32,
        pitch: f32,
        speed: f32,
        sensitivity: f32,
        zoom: f32,
        last_x: f32,
        last_y: f32,
    }

    impl Camera {
        // not equivalent to the first Camera constructor, but matches its usage
        pub fn new(position: Vec3, last_x: f32, last_y: f32) -> Self {
            let world_up = Vec3::new(0.0, 1.0, 0.0);

            let mut camera = Camera {
                position,
                front: Vec3::zero(),
                up: Vec3::zero(),
                right: Vec3::zero(),
                world_up,
                yaw: YAW,
                pitch: PITCH,
                speed: SPEED,
                sensitivity: SENSITIVITY,
                zoom: ZOOM,
                last_x,
                last_y,
            };
            camera.calculate_vectors();

            camera
        }

        fn _new_spec(pos_x: f32, pos_y: f32, pos_z: f32, up_x: f32, up_y: f32, up_z: f32, yaw: f32, pitch: f32) -> Self {
            let mut world_up = Vec3::new(up_x, up_y, up_z);
            world_up.normalize();

            let mut camera = Camera {
                position: Vec3::new(pos_x, pos_y, pos_z),
                front: Vec3::zero(),
                up: Vec3::zero(),
                right: Vec3::zero(),
                world_up,
                yaw: yaw,
                pitch: pitch,
                speed: SPEED,
                sensitivity: SENSITIVITY,
                zoom: ZOOM,
                last_x: 0.0,
                last_y: 0.0,
            };
            camera.calculate_vectors();

            camera
        }

        fn calculate_vectors(&mut self) {
            self.front = Vec3::new(self.yaw.cos() * self.pitch.cos(), self.pitch.sin(), self.yaw.sin() * self.pitch.cos());
            self.front.normalize();

            let up_cos = self.front.dot(self.world_up);
            self.right = if 1.0 - up_cos.abs() < 0.001 {
                self.front.cross(Vec3::new(0.0, 0.0, -up_cos.signum() * 1.0))
            } else {
                self.front.cross(self.world_up)
            };
            self.right.normalize();

            self.up = self.right.cross(self.front);
            self.up.normalize();
        }

        pub fn view_matrix(&self) -> Mat4 {
            Mat4::from([
                self.right.x,
                self.up.x,
                self.front.x,
                0.0,
                self.right.y,
                self.up.y,
                self.front.y,
                0.0,
                self.right.z,
                self.up.z,
                self.front.z,
                0.0,
                -self.position.dot(self.right),
                -self.position.dot(self.up),
                -self.position.dot(self.front),
                1.0,
            ])
        }

        pub fn process_keyboard(&mut self, movement: Movement, delta_time: f32) {
            let velocity = self.speed * delta_time;

            match movement {
                Movement::Forward => self.position += self.front * velocity,
                Movement::Backward => self.position -= self.front * velocity,
                Movement::Left => self.position -= self.right * velocity,
                Movement::Right => self.position += self.right * velocity,
            }
        }

        pub fn process_mouse(&mut self, x_pos: f32, y_pos: f32, constrain_pitch: bool) {
            let x_offset = x_pos - self.last_x;
            let y_offset = self.last_y - y_pos;

            self.last_x = x_pos;
            self.last_y = y_pos;

            self.yaw += x_offset * self.sensitivity;
            self.pitch += y_offset * self.sensitivity;

            if constrain_pitch {
                self.pitch = self.pitch.min(PI / 2.0 - BIAS).max(-PI / 2.0 + BIAS);
            }

            self.calculate_vectors();
        }

        pub fn process_scroll(&mut self, y_offset: f32) {
            self.zoom -= y_offset * BIAS;

            self.zoom = self.zoom.max(PI / 180.0).min(PI / 4.0);
        }

        pub fn position(&self) -> Vec3 {
            self.position
        }

        pub fn zoom(&self) -> f32 {
            self.zoom
        }
    }
}
