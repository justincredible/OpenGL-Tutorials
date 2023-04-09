pub mod text_renderer {
    use crate::{ResourceManager, Shader};
    use freetype::freetype::{FT_Done_Face, FT_Done_FreeType, FT_Face, FT_Init_FreeType, FT_Library, FT_Load_Char, FT_New_Face, FT_Set_Pixel_Sizes, FT_LOAD_RENDER};
    use glam::{Mat4, Vec2, Vec3};
    use std::{collections::HashMap, mem::size_of, ptr};

    struct Character {
        texture_id: u32,
        size: Vec2,
        bearing: Vec2,
        advance: u32,
    }

    impl Drop for Character {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteTextures(1, &self.texture_id);
            }
        }
    }

    pub struct TextRenderer {
        characters: HashMap<char, Character>,
        text_shader: Shader,
        vao: u32,
        vbo: u32,
    }

    impl Drop for TextRenderer {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteBuffers(1, &self.vbo);
                gl::DeleteVertexArrays(1, &self.vao);
            }
        }
    }

    impl TextRenderer {
        pub fn new(width: i32, height: i32) -> Self {
            let text_shader = ResourceManager::load_shader("src/text_2d.vs", "src/text_2d.fs");
            text_shader.r#use();
            text_shader.set_mat4("projection", Mat4::orthographic_lh(0.0, width as f32, 0.0, height as f32, 0.0, 1.0));
            text_shader.set_int("text", 0);
            let mut vao = 0;
            let mut vbo = 0;

            unsafe {
                gl::GenVertexArrays(1, &mut vao);
                gl::GenBuffers(1, &mut vbo);
                gl::BindVertexArray(vao);
                gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
                gl::BufferData(gl::ARRAY_BUFFER, size_of::<f32>() as isize * 6 * 4, ptr::null(), gl::DYNAMIC_DRAW);
                gl::EnableVertexAttribArray(0);
                gl::VertexAttribPointer(0, 4, gl::FLOAT, gl::FALSE, 4 * size_of::<f32>() as i32, ptr::null());
                gl::BindBuffer(gl::ARRAY_BUFFER, 0);
                gl::BindVertexArray(0);
            }

            TextRenderer {
                characters: HashMap::new(),
                text_shader,
                vao,
                vbo,
            }
        }

        pub fn load(&mut self, font: &str, font_size: u32) {
            self.characters.clear();

            unsafe {
                let mut ft: FT_Library = ptr::null_mut();
                if FT_Init_FreeType(&mut ft as *mut FT_Library) != 0 {
                    println!("ERROR::FREETYPE: Could not init FreeType Library");
                }
                let mut face: FT_Face = ptr::null_mut();
                glfw::with_c_str(font, |c_str| {
                    if FT_New_Face(ft, c_str, 0, &mut face as *mut FT_Face) != 0 {
                        println!("ERROR::FREETYPE: Failed to load font");
                    }
                });
                FT_Set_Pixel_Sizes(face, 0, font_size);
                gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
                for c in '\0'..'\u{80}' {
                    if FT_Load_Char(face, c as u32, FT_LOAD_RENDER as i32) != 0 {
                        println!("ERROR::FREETYTPE: Failed to load Glyph");
                        continue;
                    }
                    let mut texture_id = 0;
                    gl::GenTextures(1, &mut texture_id);
                    gl::BindTexture(gl::TEXTURE_2D, texture_id);
                    gl::TexImage2D(
                        gl::TEXTURE_2D,
                        0,
                        gl::RED as i32,
                        (*(*face).glyph).bitmap.width as i32,
                        (*(*face).glyph).bitmap.rows as i32,
                        0,
                        gl::RED,
                        gl::UNSIGNED_BYTE,
                        (*(*face).glyph).bitmap.buffer.cast(),
                    );
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
                    gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);

                    let character = Character {
                        texture_id,
                        size: Vec2::new((*(*face).glyph).bitmap.width as f32, (*(*face).glyph).bitmap.rows as f32),
                        bearing: Vec2::new((*(*face).glyph).bitmap_left as f32, (*(*face).glyph).bitmap_top as f32),
                        advance: (*(*face).glyph).advance.x as u32,
                    };
                    self.characters.insert(c, character);
                }
                gl::BindTexture(gl::TEXTURE_2D, 0);
                FT_Done_Face(face);
                FT_Done_FreeType(ft);
            }
        }

        pub fn render_text(&self, text: &str, mut x: f32, y: f32, scale: f32, color: Vec3) {
            self.text_shader.r#use();
            self.text_shader.set_vec3("textColor", color);

            unsafe {
                gl::ActiveTexture(gl::TEXTURE0);
                gl::BindVertexArray(self.vao);

                for c in text.chars() {
                    let ch = self.characters.get(&c).unwrap();

                    let xpos = x + ch.bearing.x as f32 * scale;
                    let ypos = y - (ch.size.y - ch.bearing.y) as f32 * scale;

                    let w = ch.size.x as f32 * scale;
                    let h = ch.size.y as f32 * scale;

                    let vertices: [f32; 24] = [
                        xpos,
                        ypos + h,
                        0.0,
                        0.0,
                        xpos,
                        ypos,
                        0.0,
                        1.0,
                        xpos + w,
                        ypos,
                        1.0,
                        1.0,
                        xpos,
                        ypos + h,
                        0.0,
                        0.0,
                        xpos + w,
                        ypos,
                        1.0,
                        1.0,
                        xpos + w,
                        ypos + h,
                        1.0,
                        0.0,
                    ];
                    gl::BindTexture(gl::TEXTURE_2D, ch.texture_id);
                    gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
                    gl::BufferSubData(gl::ARRAY_BUFFER, 0, (size_of::<f32>() * vertices.len()) as isize, vertices.as_ptr().cast());
                    gl::BindBuffer(gl::ARRAY_BUFFER, 0);
                    gl::DrawArrays(gl::TRIANGLES, 0, 6);
                    x += (ch.advance >> 6) as f32 * scale;
                }
                gl::BindVertexArray(0);
                gl::BindTexture(gl::TEXTURE_2D, 0);
            }
        }
    }
}
