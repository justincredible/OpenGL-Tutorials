pub mod particle_generator {
    use crate::{GameObject, Shader, Texture};
    use glam::{Vec2, Vec4};
    use std::{mem::size_of, ptr, rc::Rc};

    pub struct Particle {
        position: Vec2,
        velocity: Vec2,
        color: Vec4,
        life: f32,
    }

    impl Particle {
        pub fn new() -> Self {
            Particle {
                position: Vec2::ZERO,
                velocity: Vec2::ZERO,
                color: Vec4::ONE,
                life: 0.0,
            }
        }
    }

    pub struct ParticleGenerator {
        particles: Vec<Particle>,
        amount: u32,
        shader: Shader,
        texture: Rc<Texture>,
        last_used: u32,
        vao: u32,
        vbo: u32,
    }

    impl Drop for ParticleGenerator {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteVertexArrays(1, &self.vao);
                gl::DeleteBuffers(1, &self.vbo);
            }
        }
    }

    impl ParticleGenerator {
        pub fn new(shader: Shader, texture: Rc<Texture>, amount: u32) -> Self {
            ParticleGenerator {
                particles: Vec::new(),
                amount,
                shader,
                texture,
                last_used: 0,
                vao: 0,
                vbo: 0,
            }
            .init()
        }

        fn init(mut self) -> Self {
            let particle_quad = [
                0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
            ];
            unsafe {
                gl::GenVertexArrays(1, &mut self.vao);
                gl::GenBuffers(1, &mut self.vbo);
                gl::BindVertexArray(self.vao);
                gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
                gl::BufferData(
                    gl::ARRAY_BUFFER,
                    (size_of::<f32>() * particle_quad.len()) as isize,
                    particle_quad.as_ptr().cast(),
                    gl::STATIC_DRAW,
                );
                gl::EnableVertexAttribArray(0);
                gl::VertexAttribPointer(0, 4, gl::FLOAT, gl::FALSE, (4 * size_of::<f32>()) as i32, ptr::null());
                gl::BindVertexArray(0);
            }
            for _ in 0..self.amount {
                self.particles.push(Particle::new());
            }

            self
        }

        pub fn update(&mut self, dt: f32, object: &GameObject, new_particles: u32, offset: Vec2) {
            for _ in 0..new_particles {
                let unused_particle = self.first_unused_particle();
                respawn_particle(&mut self.particles[unused_particle as usize], object, offset);
            }
            for i in 0..self.amount {
                let mut p = &mut self.particles[i as usize];
                p.life -= dt;
                if p.life > 0.0 {
                    p.position -= p.velocity * dt;
                    p.color.w -= dt * 2.5;
                }
            }
        }

        pub fn draw(&self) {
            unsafe {
                gl::BlendFunc(gl::SRC_ALPHA, gl::ONE);
                self.shader.r#use();
                gl::BindVertexArray(self.vao);
                self.texture.bind();
                for particle in &self.particles {
                    if particle.life > 0.0 {
                        self.shader.set_vec2("offset", particle.position);
                        self.shader.set_vec4("color", particle.color);
                        gl::DrawArrays(gl::TRIANGLES, 0, 6);
                    }
                }
                gl::BindVertexArray(0);
                gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
            }
        }

        pub fn first_unused_particle(&mut self) -> u32 {
            for i in self.last_used..self.amount {
                if self.particles[i as usize].life <= 0.0 {
                    self.last_used = i;
                    return i;
                }
            }
            for i in 0..self.last_used {
                if self.particles[i as usize].life <= 0.0 {
                    self.last_used = i;
                    return i;
                }
            }
            self.last_used = 0;
            0
        }
    }

    pub fn respawn_particle(particle: &mut Particle, object: &GameObject, offset: Vec2) {
        let random = (fastrand::i32(..) % 100 - 50) as f32 / 10.0;
        let r_color = 0.5 + (fastrand::i32(..) % 100) as f32 / 100.0;
        particle.position = object.position + random + offset;
        particle.color = Vec4::new(r_color, r_color, r_color, 1.0);
        particle.life = 1.0;
        particle.velocity = object.velocity * 0.1;
    }
}
