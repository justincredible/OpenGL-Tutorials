pub mod ball_object {
    use crate::{GameObject, SpriteRenderer, Texture};
    use glam::{Vec2, Vec3};
    use std::rc::Rc;

    pub struct BallObject {
        go: GameObject,
        pub radius: f32,
        pub stuck: bool,
        pub sticky: bool,
        pub pass_through: bool,
    }

    impl BallObject {
        pub fn new() -> Self {
            BallObject {
                go: GameObject::new(),
                radius: 12.5,
                stuck: true,
                sticky: false,
                pass_through: false,
            }
        }

        pub fn new_with(pos: Vec2, radius: f32, velocity: Vec2, sprite: Rc<Texture>) -> Self {
            BallObject {
                go: GameObject::new_with(pos, Vec2::splat(radius * 2.0), sprite, Vec3::ONE, velocity),
                radius,
                stuck: true,
                sticky: false,
                pass_through: false,
            }
        }

        pub fn r#move(&mut self, dt: f32, window_width: u32) -> Vec2 {
            if !self.stuck {
                self.go.position += self.go.velocity * dt;
                if self.go.position.x <= 0.0 {
                    self.go.velocity.x = -self.go.velocity.x;
                    self.go.position.x = 0.0;
                } else if self.go.position.x + self.go.size.x >= window_width as f32 {
                    self.go.velocity.x = -self.go.velocity.x;
                    self.go.position.x = window_width as f32 - self.go.size.x;
                }
                if self.go.position.y <= 0.0 {
                    self.go.velocity.y = -self.go.velocity.y;
                    self.go.position.y = 0.0;
                }
            }

            self.go.position
        }

        pub fn reset(&mut self, position: Vec2, velocity: Vec2) {
            self.go.position = position;
            self.go.velocity = velocity;
            self.stuck = true;
            self.sticky = false;
            self.pass_through = false;
        }

        pub fn draw(&self, renderer: &SpriteRenderer) {
            self.go.draw(renderer);
        }

        pub fn position(&self) -> &Vec2 {
            &self.go.position
        }

        pub fn position_mut(&mut self) -> &mut Vec2 {
            &mut self.go.position
        }

        pub fn velocity(&mut self) -> &mut Vec2 {
            &mut self.go.velocity
        }

        pub fn color(&mut self) -> &mut Vec3 {
            &mut self.go.color
        }

        pub fn as_go(&self) -> &GameObject {
            &self.go
        }
    }
}
