use freetype::freetype::*;
use gfx_maths::{mat4::Mat4, vec3::Vec3};
use glam::IVec2;
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{collections::HashMap, fs::File, io::Read, mem::size_of, path::Path, ptr, str};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;

pub mod shader;
use shader::shader::Program;

struct Character {
    texture_id: u32,
    size: IVec2,
    bearing: IVec2,
    advance: u32,
}

impl Drop for Character {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteTextures(1, &self.texture_id);
        }
    }
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw
        .create_window(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_cursor_mode(CursorMode::Disabled);
    window.set_framebuffer_size_polling(true);
    window.set_cursor_pos_polling(true);
    window.set_scroll_polling(true);
    window.focus();

    gl::load_with(|s| window.get_proc_address(s) as *const _);

    ogl::cull_enable();
    ogl::blend_enable();

    let shader = Program::new().link("src/text.vs", "src/text.fs");

    let projection = Mat4::orthographic_opengl(0.0, SCR_WIDTH as f32, 0.0, SCR_HEIGHT as f32, 0.0, 0.1);
    shader.apply();
    shader.set_mat4("projection", projection /*look_at(Vec3::new(0.0, 0.0, 3.0), Vec3::zero(), Vec3::new(0.0, 1.0, 0.0))*/);

    let mut characters = HashMap::<char, Character>::new();

    let mut ft: FT_Library = ptr::null_mut();
    unsafe {
        if FT_Init_FreeType(&mut ft as *mut FT_Library) != 0 {
            panic!("ERROR::FREETYPE: Could not init FreeType Library");
        }
    }

    let font_name = Path::new("resources/fonts/Antonio-Bold.ttf");
    if !Path::exists(font_name) {
        panic!("ERROR::FREETYPE: Failed to load font_name");
    }

    let mut face: FT_Face = ptr::null_mut();
    unsafe {
        if FT_New_Face(ft, c_name(font_name.to_str().unwrap()).as_ptr(), 0, &mut face as *mut FT_Face) != 0 {
            panic!("ERROR::FREETYPE: Failed to load font");
        } else {
            FT_Set_Pixel_Sizes(face, 0, 48);

            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);

            for c in '\0'..'\u{80}' {
                if FT_Load_Char(face, c as u32, FT_LOAD_RENDER as i32) != 0 {
                    println!("ERROR::FREETYTPE: Failed to load Glyph");
                    continue;
                }
                let glyph = (*face).glyph;
                let bitmap = (*glyph).bitmap;
                let bitmap_left = (*glyph).bitmap_left;
                let bitmap_top = (*glyph).bitmap_top;
                let advance = (*glyph).advance;

                let mut texture_id = 0;
                gl::GenTextures(1, &mut texture_id);

                gl::BindTexture(gl::TEXTURE_2D, texture_id);
                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    0,
                    gl::RED as i32,
                    bitmap.width as i32,
                    bitmap.rows as i32,
                    0,
                    gl::RED,
                    gl::UNSIGNED_BYTE,
                    bitmap.buffer.cast(),
                );

                let clamp_edge = gl::CLAMP_TO_EDGE as i32;
                let linear = gl::LINEAR as i32;
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, clamp_edge);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, clamp_edge);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, linear);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, linear);

                let character = Character {
                    texture_id,
                    size: IVec2::new(bitmap.width as i32, bitmap.rows as i32),
                    bearing: IVec2::new(bitmap_left, bitmap_top),
                    advance: advance.x as u32,
                };
                characters.insert(c, character);
            }

            gl::BindTexture(gl::TEXTURE_2D, 0);
        }

        FT_Done_Face(face);
        FT_Done_FreeType(ft);
    }

    let quad = Text2D::new();

    glfw.poll_events();

    while !window.should_close() {
        process_input(&mut window);

        ogl::clear_color(0.2, 0.3, 0.3, 1.0);
        ogl::clear();

        render_text(&shader, &quad, &characters, "This is sample text", 25.0, 25.0, 1.0, Vec3::new(0.5, 0.8, 0.2));
        render_text(&shader, &quad, &characters, "(C) LearnOpenGL.com", 540.0, 570.0, 0.5, Vec3::new(0.3, 0.7, 0.9));

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(event);
        }
    }
}

fn process_input(window: &mut Window) {
    if window.get_key(Key::Escape) == Action::Press {
        window.set_should_close(true);
    }
}

fn handle_window_event(event: glfw::WindowEvent) {
    match event {
        WindowEvent::FramebufferSize(width, height) => ogl::viewport(width, height),
        _ => {}
    }
}

mod ogl {
    pub fn clear_color(red: f32, green: f32, blue: f32, alpha: f32) {
        unsafe {
            gl::ClearColor(red, green, blue, alpha);
        }
    }

    pub fn clear() {
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
        }
    }

    pub fn unbind_vao() {
        unsafe {
            gl::BindVertexArray(0);
        }
    }

    pub fn cull_enable() {
        unsafe {
            gl::Enable(gl::CULL_FACE);
        }
    }

    pub fn blend_enable() {
        unsafe {
            gl::Enable(gl::BLEND);
            gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        }
    }

    pub fn active_texture(unit: u32) {
        assert!(unit <= gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            gl::ActiveTexture(gl::TEXTURE0 + unit);
        }
    }

    pub fn bind_texture(identifier: u32) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, identifier);
        }
    }

    pub fn unbind_texture() {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, 0);
        }
    }

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }
}

pub fn c_name(name: &str) -> Vec<i8> {
    c_finish(&mut name.as_bytes().iter())
}

pub fn c_named(name: &str, number: &str) -> Vec<i8> {
    c_finish(&mut name.as_bytes().iter().chain(number.as_bytes().iter()))
}

fn c_finish(iter: &mut dyn Iterator<Item = &u8>) -> Vec<i8> {
    iter.filter(|&u| *u < 128u8).map(|u| *u as i8).chain(std::iter::once(0)).collect::<Vec<_>>()
}

pub struct Text2D {
    vertex_array: gl::types::GLuint,
    vertex_buffer: gl::types::GLuint,
}

impl Text2D {
    pub fn new() -> Self {
        let mut vertex_array = 0;
        let mut vertex_buffer = 0;

        unsafe {
            gl::GenVertexArrays(1, &mut vertex_array);
            gl::GenBuffers(1, &mut vertex_buffer);

            gl::BindVertexArray(vertex_array);

            gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
            gl::BufferData(gl::ARRAY_BUFFER, (6 * 4 * size_of::<f32>()) as isize, ptr::null(), gl::DYNAMIC_DRAW);

            gl::VertexAttribPointer(0, 4, gl::FLOAT, gl::FALSE, (4 * size_of::<f32>()) as i32, ptr::null());
            gl::EnableVertexAttribArray(0);

            gl::BindBuffer(gl::ARRAY_BUFFER, 0);
            gl::BindVertexArray(0);
        }

        Text2D { vertex_array, vertex_buffer }
    }

    pub fn bind(&self) {
        unsafe {
            gl::BindVertexArray(self.vertex_array);
        }
    }

    pub fn buffer_data(&self, data: &[f32]) {
        unsafe {
            gl::BindBuffer(gl::ARRAY_BUFFER, self.vertex_buffer);
            gl::BufferSubData(gl::ARRAY_BUFFER, 0, (data.len() * size_of::<f32>()) as isize, data.as_ptr().cast());
            gl::BindBuffer(gl::ARRAY_BUFFER, 0);
        }
    }

    pub fn draw(&self) {
        unsafe {
            gl::DrawArrays(gl::TRIANGLES, 0, 6);
        }
    }
}

impl Drop for Text2D {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteVertexArrays(1, &self.vertex_array);
            gl::DeleteBuffers(1, &self.vertex_buffer);
        }
    }
}

fn render_text(shader: &Program, quad: &Text2D, characters: &HashMap<char, Character>, text: &str, x: f32, y: f32, scale: f32, colour: Vec3) {
    shader.apply();
    shader.set_vec3("textColor", colour);
    ogl::active_texture(0);
    quad.bind();

    let mut x = x;
    for c in text.chars() {
        let ch = characters.get(&c).unwrap();

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

        ogl::bind_texture(ch.texture_id);
        quad.buffer_data(&vertices);
        quad.draw();

        x += (ch.advance >> 6) as f32 * scale;
    }

    ogl::unbind_vao();
    ogl::unbind_texture();
}
