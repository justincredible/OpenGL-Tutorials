pub mod power_up {
    use crate::{GameObject, SpriteRenderer, Texture};
    use glam::{Vec2, Vec3};
    use std::rc::Rc;

    const POWERUP_SIZE: Vec2 = Vec2::new(60.0, 20.0);
    const VELOCITY: Vec2 = Vec2::new(0.0, 150.0);

    pub struct PowerUp {
        go: GameObject,
        pub r#type: String,
        pub duration: f32,
        pub activated: bool,
    }

    impl PowerUp {
        pub fn new(r#type: String, color: Vec3, duration: f32, position: Vec2, texture: Rc<Texture>) -> Self {
            let go = GameObject::new_with(position, POWERUP_SIZE, texture, color, VELOCITY);

            PowerUp {
                go,
                r#type,
                duration,
                activated: false,
            }
        }

        pub fn draw(&self, renderer: &SpriteRenderer) {
            self.go.draw(renderer);
        }

        pub fn position(&mut self) -> &mut Vec2 {
            &mut self.go.position
        }

        pub fn velocity(&mut self) -> &mut Vec2 {
            &mut self.go.velocity
        }

        pub fn is_destroyed(&self) -> bool {
            self.go.destroyed
        }

        pub fn destroyed(&mut self) -> &mut bool {
            &mut self.go.destroyed
        }

        pub fn as_go(&self) -> &GameObject {
            &self.go
        }
    }
}
