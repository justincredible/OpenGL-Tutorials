pub mod entity {
    use crate::{Camera, Model, Program};
    use glam::{EulerRot, Mat4, Quat, Vec3};
    use std::{f32::consts::PI, rc::Rc};

    #[derive(Clone, Copy)]
    pub struct Transform {
        position: Vec3,
        rotation: Vec3,
        scale: Vec3,
        model_matrix: Mat4,
        is_dirty: bool,
    }

    impl Transform {
        pub fn new() -> Self {
            Transform {
                position: Vec3::ZERO,
                rotation: Vec3::ZERO,
                scale: Vec3::ONE,
                model_matrix: Mat4::IDENTITY,
                is_dirty: true,
            }
        }

        fn get_local_model_matrix(&self) -> Mat4 {
            let rotation = Quat::from_euler(EulerRot::XYZ, self.rotation.x * PI / 180.0, self.rotation.y * PI / 180.0, self.rotation.z * PI / 180.0);
            Mat4::from_scale_rotation_translation(self.scale, rotation, self.position)
        }

        pub fn compute_model_matrix(&mut self) {
            self.model_matrix = self.get_local_model_matrix();
        }

        pub fn compute_relative_model_matrix(&mut self, parent: Mat4) {
            self.model_matrix = parent * self.get_local_model_matrix();
        }

        pub fn set_local_position(&mut self, position: Vec3) {
            self.position = position;
            self.is_dirty = true;
        }

        pub fn set_local_rotation(&mut self, rotation: Vec3) {
            self.rotation = rotation;
            self.is_dirty = true;
        }

        pub fn set_local_scale(&mut self, scale: Vec3) {
            self.scale = scale;
            self.is_dirty = true;
        }

        pub fn get_global_position(&self) -> Vec3 {
            self.model_matrix.w_axis.truncate()
        }

        pub fn get_local_position(&self) -> Vec3 {
            self.position
        }

        pub fn get_local_rotation(&self) -> Vec3 {
            self.rotation
        }

        pub fn get_local_scale(&self) -> Vec3 {
            self.scale
        }

        pub fn get_model_matrix(&self) -> Mat4 {
            self.model_matrix
        }

        pub fn get_right(&self) -> Vec3 {
            self.model_matrix.x_axis.truncate()
        }

        pub fn get_up(&self) -> Vec3 {
            self.model_matrix.y_axis.truncate()
        }

        pub fn get_backward(&self) -> Vec3 {
            self.model_matrix.z_axis.truncate()
        }

        pub fn get_forward(&self) -> Vec3 {
            -self.model_matrix.z_axis.truncate()
        }

        pub fn get_global_scale(&self) -> Vec3 {
            Vec3::new(self.get_right().length(), self.get_up().length(), self.get_backward().length())
        }

        pub fn is_dirty(&self) -> bool {
            self.is_dirty
        }
    }

    pub struct Plan {
        normal: Vec3,
        distance: f32,
    }

    impl Default for Plan {
        fn default() -> Self {
            Plan {
                normal: Vec3::new(0.0, 1.0, 0.0),
                distance: 0.0,
            }
        }
    }

    impl Plan {
        pub fn new(p1: Vec3, norm: Vec3) -> Self {
            let normal = norm.normalize();
            Plan { normal, distance: normal.dot(p1) }
        }

        pub fn get_signed_distance_to_plan(&self, point: Vec3) -> f32 {
            self.normal.dot(point) - self.distance
        }
    }

    pub struct Frustum {
        pub top_face: Plan,
        pub bottom_face: Plan,
        pub right_face: Plan,
        pub left_face: Plan,
        pub far_face: Plan,
        pub near_face: Plan,
    }

    pub trait BoundingVolume {
        fn is_on_or_forward_plan(&self, plan: &Plan) -> bool;

        fn is_on_frustum_transform(&self, cam_frustum: &Frustum, transform: &Transform) -> bool;

        fn is_on_frustum(&self, cam_frustum: &Frustum) -> bool {
            self.is_on_or_forward_plan(&cam_frustum.left_face)
                && self.is_on_or_forward_plan(&cam_frustum.right_face)
                && self.is_on_or_forward_plan(&cam_frustum.top_face)
                && self.is_on_or_forward_plan(&cam_frustum.bottom_face)
                && self.is_on_or_forward_plan(&cam_frustum.near_face)
                && self.is_on_or_forward_plan(&cam_frustum.far_face)
        }
    }

    #[derive(Default)]
    pub struct Sphere {
        center: Vec3,
        radius: f32,
    }

    impl BoundingVolume for Sphere {
        fn is_on_or_forward_plan(&self, plan: &Plan) -> bool {
            plan.get_signed_distance_to_plan(self.center) > -self.radius
        }

        fn is_on_frustum_transform(&self, cam_frustum: &Frustum, transform: &Transform) -> bool {
            let global_scale = transform.get_global_scale();
            let global_center = (transform.get_model_matrix() * self.center.extend(1.0)).truncate();
            let max_scale = f32::max(f32::max(global_scale.x, global_scale.y), global_scale.z);
            let global_sphere = Sphere::new(global_center, self.radius * max_scale * 0.5);

            global_sphere.is_on_frustum(cam_frustum)
        }
    }

    impl Sphere {
        pub fn new(center: Vec3, radius: f32) -> Self {
            Sphere { center, radius }
        }
    }

    #[derive(Default)]
    pub struct SquareAABB {
        center: Vec3,
        extent: f32,
    }

    impl BoundingVolume for SquareAABB {
        fn is_on_or_forward_plan(&self, plan: &Plan) -> bool {
            let r = self.extent * (f32::abs(plan.normal.x) + f32::abs(plan.normal.y) + f32::abs(plan.normal.z));

            -r <= plan.get_signed_distance_to_plan(self.center)
        }

        fn is_on_frustum_transform(&self, cam_frustum: &Frustum, transform: &Transform) -> bool {
            let global_center = (transform.get_model_matrix() * self.center.extend(1.0)).truncate();

            let right = transform.get_right() * self.extent;
            let up = transform.get_up() * self.extent;
            let forward = transform.get_forward() * self.extent;

            let new_ii = f32::abs(Vec3::X.dot(right)) + f32::abs(Vec3::X.dot(up)) + f32::abs(Vec3::X.dot(forward));

            let new_ij = f32::abs(Vec3::Y.dot(right)) + f32::abs(Vec3::Y.dot(up)) + f32::abs(Vec3::Y.dot(forward));

            let new_ik = f32::abs(Vec3::Z.dot(right)) + f32::abs(Vec3::Z.dot(up)) + f32::abs(Vec3::Z.dot(forward));

            let max_scale = f32::max(f32::max(new_ii, new_ij), new_ik);
            let global_aabb = SquareAABB::new(global_center, max_scale);

            global_aabb.is_on_frustum(cam_frustum)
        }
    }

    impl SquareAABB {
        pub fn new(center: Vec3, extent: f32) -> Self {
            SquareAABB { center, extent }
        }
    }

    #[derive(Default)]
    pub struct AABB {
        center: Vec3,
        extents: Vec3,
    }

    impl BoundingVolume for AABB {
        fn is_on_or_forward_plan(&self, plan: &Plan) -> bool {
            let r = self.extents.x * f32::abs(plan.normal.x) + self.extents.y * f32::abs(plan.normal.y) + self.extents.z * f32::abs(plan.normal.z);

            -r <= plan.get_signed_distance_to_plan(self.center)
        }

        fn is_on_frustum_transform(&self, cam_frustum: &Frustum, transform: &Transform) -> bool {
            let global_center = (transform.get_model_matrix() * self.center.extend(1.0)).truncate();

            let right = transform.get_right() * self.extents.x;
            let up = transform.get_up() * self.extents.y;
            let forward = transform.get_forward() * self.extents.z;

            let new_ii = f32::abs(Vec3::X.dot(right)) + f32::abs(Vec3::X.dot(up)) + f32::abs(Vec3::X.dot(forward));

            let new_ij = f32::abs(Vec3::Y.dot(right)) + f32::abs(Vec3::Y.dot(up)) + f32::abs(Vec3::Y.dot(forward));

            let new_ik = f32::abs(Vec3::Z.dot(right)) + f32::abs(Vec3::Z.dot(up)) + f32::abs(Vec3::Z.dot(forward));

            let global_aabb = AABB::new(global_center, Vec3::new(new_ii, new_ij, new_ik));

            global_aabb.is_on_frustum(cam_frustum)
        }
    }

    impl AABB {
        pub fn new_minmax(min: Vec3, max: Vec3) -> Self {
            let center = (max + min) * 0.5;
            let extents = Vec3::new(max.x - center.x, max.y - center.y, max.z - center.z);

            AABB { center, extents }
        }

        pub fn new(center: Vec3, extents: Vec3) -> Self {
            AABB { center, extents }
        }

        pub fn get_vertice(&self) -> Vec<Vec3> {
            vec![
                Vec3::new(self.center.x - self.extents.x, self.center.y - self.extents.y, self.center.z - self.extents.z),
                Vec3::new(self.center.x + self.extents.x, self.center.y - self.extents.y, self.center.z - self.extents.z),
                Vec3::new(self.center.x - self.extents.x, self.center.y + self.extents.y, self.center.z - self.extents.z),
                Vec3::new(self.center.x + self.extents.x, self.center.y + self.extents.y, self.center.z - self.extents.z),
                Vec3::new(self.center.x - self.extents.x, self.center.y - self.extents.y, self.center.z + self.extents.z),
                Vec3::new(self.center.x + self.extents.x, self.center.y - self.extents.y, self.center.z + self.extents.z),
                Vec3::new(self.center.x - self.extents.x, self.center.y + self.extents.y, self.center.z + self.extents.z),
                Vec3::new(self.center.x + self.extents.x, self.center.y + self.extents.y, self.center.z + self.extents.z),
            ]
        }
    }

    fn create_frustum_from_camera(cam: Camera, aspect: f32, fov_y: f32, z_near: f32, z_far: f32) -> Frustum {
        let half_v_side = z_far * f32::tan(fov_y * 0.5);
        let half_h_side = half_v_side * aspect;
        let front_mult_far = z_far * cam.front();

        Frustum {
            near_face: Plan::new(cam.position() + z_near * cam.front(), cam.front()),
            far_face: Plan::new(cam.position() + front_mult_far, -cam.front()),
            right_face: Plan::new(cam.position(), cam.up().cross(front_mult_far + cam.right() * half_h_side)),
            left_face: Plan::new(cam.position(), (front_mult_far - cam.right() * half_h_side).cross(cam.up())),
            top_face: Plan::new(cam.position(), cam.right().cross(front_mult_far - cam.up() * half_v_side)),
            bottom_face: Plan::new(cam.position(), (front_mult_far + cam.up() * half_v_side).cross(cam.right())),
        }
    }

    fn generate_aabb(model: &Model) -> AABB {
        let mut min_aabb = Vec3::splat(f32::MAX);
        let mut max_aabb = Vec3::splat(f32::MIN);

        for mesh in &model.meshes {
            for vertex in &mesh.vertices {
                min_aabb.x = f32::min(min_aabb.x, vertex.position.x);
                min_aabb.y = f32::min(min_aabb.y, vertex.position.y);
                min_aabb.z = f32::min(min_aabb.z, vertex.position.z);

                max_aabb.x = f32::max(max_aabb.x, vertex.position.x);
                max_aabb.y = f32::max(max_aabb.y, vertex.position.y);
                max_aabb.z = f32::max(max_aabb.z, vertex.position.z);
            }
        }

        AABB::new_minmax(min_aabb, max_aabb)
    }

    fn generate_sphere(model: &Model) -> Sphere {
        let mut min_aabb = Vec3::splat(f32::MAX);
        let mut max_aabb = Vec3::splat(f32::MIN);

        for mesh in &model.meshes {
            for vertex in &mesh.vertices {
                min_aabb.x = f32::min(min_aabb.x, vertex.position.x);
                min_aabb.y = f32::min(min_aabb.y, vertex.position.y);
                min_aabb.z = f32::min(min_aabb.z, vertex.position.z);

                max_aabb.x = f32::max(max_aabb.x, vertex.position.x);
                max_aabb.y = f32::max(max_aabb.y, vertex.position.y);
                max_aabb.z = f32::max(max_aabb.z, vertex.position.z);
            }
        }

        Sphere::new((max_aabb + min_aabb) * 0.5, (min_aabb - max_aabb).length())
    }

    pub struct Entity {
        pub children: Vec<Entity>,
        parent: Option<Transform>,
        pub transform: Transform,
        pub p_model: Rc<Model>,
        bounding_volume: AABB,
    }

    impl From<&Rc<Model>> for Entity {
        fn from(value: &Rc<Model>) -> Self {
            let bounding_volume = generate_aabb(&value);

            Entity {
                children: Vec::new(),
                parent: None,
                transform: Transform::new(),
                p_model: Rc::clone(value),
                bounding_volume,
            }
        }
    }

    impl Entity {
        pub fn get_global_aabb(&self) -> AABB {
            let global_center = (self.transform.get_model_matrix() * self.bounding_volume.center.extend(1.0)).truncate();

            let right = self.transform.get_right() * self.bounding_volume.extents.x;
            let up = self.transform.get_up() * self.bounding_volume.extents.y;
            let forward = self.transform.get_forward() * self.bounding_volume.extents.z;

            let new_ii = f32::abs(Vec3::X.dot(right)) + f32::abs(Vec3::X.dot(up)) + f32::abs(Vec3::X.dot(forward));

            let new_ij = f32::abs(Vec3::Y.dot(right)) + f32::abs(Vec3::Y.dot(up)) + f32::abs(Vec3::Y.dot(forward));

            let new_ik = f32::abs(Vec3::Z.dot(right)) + f32::abs(Vec3::Z.dot(up)) + f32::abs(Vec3::Z.dot(forward));

            AABB::new(global_center, Vec3::new(new_ii, new_ij, new_ik))
        }

        pub fn add_child(&mut self, mut child: Entity) {
            child.parent = Some(self.transform);
            self.children.push(child);
        }

        pub fn update_self_and_child(&mut self) {
            if self.transform.is_dirty() {
                self.force_update_self_and_child();
            }
        }

        fn force_update_self_and_child(&mut self) {
            if let Some(transform) = self.parent {
                self.transform.compute_relative_model_matrix(transform.get_model_matrix());
            } else {
                self.transform.compute_model_matrix();
            }

            for child in &mut self.children {
                child.force_update_self_and_child();
            }
        }

        pub fn draw_self_and_child(&self, frustum: &Frustum, shader: &Program, display: &mut u32, total: &mut u32) {
            if self.bounding_volume.is_on_frustum_transform(frustum, &self.transform) {
                shader.set_mat4("model", self.transform.get_model_matrix());
                self.p_model.draw(shader);
                *display += 1;
            }
            *total += 1;

            for child in &self.children {
                child.draw_self_and_child(frustum, shader, display, total);
            }
        }
    }
}
