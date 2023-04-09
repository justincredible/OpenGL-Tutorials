pub mod post_processor {
    use crate::{Shader, Texture};
    use std::{mem::size_of, ptr};

    pub struct PostProcessor {
        shader: Shader,
        texture: Texture,
        screen_size: (u32, u32),
        pub confuse: bool,
        pub chaos: bool,
        pub shake: bool,
        msfbo: u32,
        fbo: u32,
        rbo: u32,
        vao: u32,
        vbo: u32,
    }

    impl Drop for PostProcessor {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteFramebuffers(1, &self.msfbo);
                gl::DeleteFramebuffers(1, &self.fbo);
                gl::DeleteRenderbuffers(1, &self.rbo);
                gl::DeleteVertexArrays(1, &self.vao);
                gl::DeleteBuffers(1, &self.vbo);
            }
        }
    }

    impl PostProcessor {
        pub fn new(shader: Shader, width: u32, height: u32) -> Self {
            let screen_size = (width, height);
            let confuse = false;
            let chaos = false;
            let shake = false;
            let mut texture = Texture::new();
            let mut msfbo = 0;
            let mut fbo = 0;
            let mut rbo = 0;
            let mut vao = 0;
            let mut vbo = 0;

            unsafe {
                gl::GenFramebuffers(1, &mut msfbo);
                gl::GenFramebuffers(1, &mut fbo);
                gl::GenRenderbuffers(1, &mut rbo);
                gl::BindFramebuffer(gl::FRAMEBUFFER, msfbo);
                gl::BindRenderbuffer(gl::RENDERBUFFER, rbo);
                gl::RenderbufferStorageMultisample(gl::RENDERBUFFER, 4, gl::RGB, width as i32, height as i32);
                gl::FramebufferRenderbuffer(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::RENDERBUFFER, rbo);
                if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                    println!("ERROR::POSTPROCESSOR: Failed to initialize MSFBO");
                }
                gl::BindFramebuffer(gl::FRAMEBUFFER, fbo);
                texture.generate(width as i32, height as i32, ptr::null());
                gl::FramebufferTexture2D(gl::FRAMEBUFFER, gl::COLOR_ATTACHMENT0, gl::TEXTURE_2D, texture.id, 0);
                if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                    println!("ERROR::POSTPROCESSOR: Failed to initialize FBO");
                }
                gl::BindFramebuffer(gl::FRAMEBUFFER, 0);

                let vertices: [f32; 24] = [
                    -1.0, -1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, -1.0, 1.0, 0.0, 1.0, -1.0, -1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
                ];
                gl::GenVertexArrays(1, &mut vao);
                gl::GenBuffers(1, &mut vbo);
                gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
                gl::BufferData(gl::ARRAY_BUFFER, (size_of::<f32>() * vertices.len()) as isize, vertices.as_ptr().cast(), gl::STATIC_DRAW);
                gl::BindVertexArray(vao);
                gl::EnableVertexAttribArray(0);
                gl::VertexAttribPointer(0, 4, gl::FLOAT, gl::FALSE, (4 * size_of::<f32>()) as i32, ptr::null());
                gl::BindBuffer(gl::ARRAY_BUFFER, 0);
                gl::BindVertexArray(0);

                shader.r#use();
                shader.set_int("scene", 0);
                let offset = 1.0 / 300.0;
                let offsets: [f32; 18] = [
                    -offset, offset, // top-left
                    0.0, offset, // top-center
                    offset, offset, // top-right
                    -offset, 0.0, // center-left
                    0.0, 0.0, // center-center
                    offset, 0.0, // center-right
                    -offset, -offset, // bottom-left
                    0.0, -offset, // bottom-center
                    offset, -offset, // bottom-right
                ];
                glfw::with_c_str("offsets", |locn| gl::Uniform2fv(gl::GetUniformLocation(shader.id, locn), 9, offsets.as_ptr()));
                let edge_kernel: [i32; 9] = [-1, -1, -1, -1, 8, -1, -1, -1, -1];
                glfw::with_c_str("edge_kernel", |locn| gl::Uniform1iv(gl::GetUniformLocation(shader.id, locn), 9, edge_kernel.as_ptr()));
                let blur_kernel: [f32; 9] = [1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0, 2.0 / 16.0, 4.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0];
                glfw::with_c_str("blur_kernel", |locn| gl::Uniform1fv(gl::GetUniformLocation(shader.id, locn), 9, blur_kernel.as_ptr()));
            }

            PostProcessor {
                shader,
                texture,
                screen_size,
                confuse,
                chaos,
                shake,
                msfbo,
                fbo,
                rbo,
                vao,
                vbo,
            }
        }

        pub fn begin_render(&self) {
            unsafe {
                gl::BindFramebuffer(gl::FRAMEBUFFER, self.msfbo);
                gl::ClearColor(0.0, 0.0, 0.0, 1.0);
                gl::Clear(gl::COLOR_BUFFER_BIT);
            }
        }

        pub fn end_render(&self) {
            let (width, height) = self.screen_size;
            unsafe {
                gl::BindFramebuffer(gl::READ_FRAMEBUFFER, self.msfbo);
                gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.fbo);
                gl::BlitFramebuffer(0, 0, width as i32, height as i32, 0, 0, width as i32, height as i32, gl::COLOR_BUFFER_BIT, gl::NEAREST);
                gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
            }
        }

        pub fn render(&self, time: f32) {
            self.shader.r#use();
            self.shader.set_float("time", time);
            self.shader.set_int("confuse", self.confuse as i32);
            self.shader.set_int("chaos", self.chaos as i32);
            self.shader.set_int("shake", self.shake as i32);
            unsafe {
                gl::ActiveTexture(gl::TEXTURE0);
                self.texture.bind();
                gl::BindVertexArray(self.vao);
                gl::DrawArrays(gl::TRIANGLES, 0, 6);
                gl::BindVertexArray(0);
            }
        }
    }
}
