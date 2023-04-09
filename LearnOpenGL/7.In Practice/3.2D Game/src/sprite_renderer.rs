pub mod sprite_renderer {
    use crate::{Shader, Texture};
    use glam::{Mat4, Vec2, Vec3};
    use std::{mem::size_of, ptr, rc::Rc};

    pub struct SpriteRenderer {
        shader: Shader,
        quad_vao: u32,
        vbo: u32,
    }

    impl Drop for SpriteRenderer {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteBuffers(1, &self.vbo);
                gl::DeleteVertexArrays(1, &self.quad_vao);
            }
        }
    }

    impl SpriteRenderer {
        pub fn new(shader: Shader) -> Self {
            let vertices: [f32; 24] = [
                0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
            ];
            let mut quad_vao = 0;
            let mut vbo = 0;
            unsafe {
                gl::GenVertexArrays(1, &mut quad_vao);
                gl::GenBuffers(1, &mut vbo);

                gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
                gl::BufferData(gl::ARRAY_BUFFER, (size_of::<f32>() * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);

                gl::BindVertexArray(quad_vao);
                gl::EnableVertexAttribArray(0);
                gl::VertexAttribPointer(0, 4, gl::FLOAT, gl::FALSE, (4 * size_of::<f32>()) as i32, ptr::null());
                gl::BindBuffer(gl::ARRAY_BUFFER, 0);
                gl::BindVertexArray(0);
            }

            SpriteRenderer { shader, quad_vao, vbo }
        }

        pub fn draw_sprite(&self, texture: &Rc<Texture>, position: Vec2, size: Vec2, rotate: f32, color: Vec3) {
            self.shader.r#use();
            let model = Mat4::from_translation(position.extend(0.0))
                * Mat4::from_translation(Vec3::new(0.5 * size.x, 0.5 * size.y, 0.0))
                * Mat4::from_rotation_z(rotate)
                * Mat4::from_translation(Vec3::new(-0.5 * size.x, -0.5 * size.y, 0.0))
                * Mat4::from_scale(size.extend(1.0));

            self.shader.set_mat4("model", model);

            self.shader.set_vec3("spriteColor", color);

            unsafe {
                gl::ActiveTexture(gl::TEXTURE0);
                texture.bind();

                gl::BindVertexArray(self.quad_vao);
                gl::DrawArrays(gl::TRIANGLES, 0, 6);
                gl::BindVertexArray(0);
            }
        }
    }
}
