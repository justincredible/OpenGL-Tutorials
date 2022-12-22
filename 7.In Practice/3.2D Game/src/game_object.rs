pub mod game_object {
    use crate::{SpriteRenderer, Texture};
    use glam::{Vec2, Vec3};
    use std::rc::Rc;

    pub struct GameObject {
        pub position: Vec2,
        pub size: Vec2,
        pub velocity: Vec2,
        pub color: Vec3,
        rotation: f32,
        pub is_solid: bool,
        pub destroyed: bool,
        sprite: Rc<Texture>,
    }

    macro_rules! sprite {
        () => {
            Rc::new(Texture::new())
        };
    }

    impl GameObject {
        pub fn new() -> Self {
            GameObject {
                position: Vec2::ZERO,
                size: Vec2::ONE,
                velocity: Vec2::ZERO,
                color: Vec3::ONE,
                rotation: 0.0,
                is_solid: false,
                destroyed: false,
                sprite: sprite!(),
            }
        }

        pub fn new_with(position: Vec2, size: Vec2, sprite: Rc<Texture>, color: Vec3, velocity: Vec2) -> Self {
            GameObject {
                position,
                size,
                velocity,
                color,
                rotation: 0.0,
                is_solid: false,
                destroyed: false,
                sprite,
            }
        }

        pub fn draw(&self, renderer: &SpriteRenderer) {
            renderer.draw_sprite(&self.sprite, self.position, self.size, self.rotation, self.color);
        }
    }
}
