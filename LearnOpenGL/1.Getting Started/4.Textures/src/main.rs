use glfw::{Action, Context, Key, Window, WindowHint};
use std::fs::File;
use std::ptr;
use std::rc::Rc;

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;

pub mod shader;
use shader::shader::Program;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw
        .create_window(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_framebuffer_size_polling(true);
    window.focus();

    let gl = Rc::new(gl::Gl::load_with(|s| window.get_proc_address(s).cast()));

    let program = Program::new(Rc::clone(&gl)).link("src/4.2.texture.vs", "src/4.2.texture.fs");
    let vao = VertexArray::new(Rc::clone(&gl));

    stbi_flip_vertical(true);

    let texture1 = Texture::new(Rc::clone(&gl)).load("resources/textures/container.jpg", gl::RGB);
    let texture2 = Texture::new(Rc::clone(&gl)).load("resources/textures/awesomeface.png", gl::RGBA);

    program.apply();
    program.set_int("texture1", 0);
    program.set_int("texture2", 1);

    while !window.should_close() {
        process_input(&mut window);

        gl.clear(0.2, 0.3, 0.3, 1.0);

        texture1.bind_active(gl::TEXTURE0);
        texture2.bind_active(gl::TEXTURE1);

        program.apply();
        vao.draw();

        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&gl, &mut window, event);
        }
    }
}

fn process_input(window: &mut Window) {
    if window.get_key(Key::Escape) == Action::Press {
        window.set_should_close(true);
    }
}

fn handle_window_event(gl: &gl::Gl, _window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        glfw::WindowEvent::FramebufferSize(width, height) => unsafe {
            gl.Viewport(0, 0, width, height);
        },
        _ => {}
    }
}

mod gl {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

    impl self::Gl {
        pub fn clear(&self, red: f32, green: f32, blue: f32, alpha: f32) {
            unsafe {
                self.ClearColor(red, green, blue, alpha);
                self.Clear(self::COLOR_BUFFER_BIT);
            }
        }
    }
}

const VERTEX_COUNT: usize = 4;
const VERTEX_COMPONENTS: usize = 8;
const VERTICES: [f32; VERTEX_COMPONENTS * VERTEX_COUNT] = [
    // positions	// colors	// texture coords
    0.5, 0.5, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, // top right
    0.5, -0.5, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, // bottom right
    -0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, // bottom left
    -0.5, 0.5, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, // top left
];

const INDEX_COUNT: usize = 6;
const INDICES: [u8; INDEX_COUNT] = [
    0, 1, 3, // first triangle
    1, 2, 3, // second triangle
];

pub struct VertexArray {
    gl: Rc<gl::Gl>,
    vertex_array: gl::types::GLuint,
    vertex_buffer: gl::types::GLuint,
    index_buffer: gl::types::GLuint,
}

impl VertexArray {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut vertex_array = 0;
        let mut vertex_buffer = 0;
        let mut index_buffer = 0;
        let float_size = std::mem::size_of::<f32>();

        unsafe {
            gl.GenVertexArrays(1, &mut vertex_array);
            gl.GenBuffers(1, &mut vertex_buffer);
            gl.GenBuffers(1, &mut index_buffer);

            gl.BindVertexArray(vertex_array);

            gl.BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
            gl.BufferData(
                gl::ARRAY_BUFFER,
                (float_size * VERTEX_COMPONENTS * VERTEX_COUNT) as isize,
                VERTICES.as_ptr().cast(),
                gl::STATIC_DRAW,
            );

            gl.VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (VERTEX_COMPONENTS * float_size) as i32, ptr::null());
            gl.EnableVertexAttribArray(0);

            gl.VertexAttribPointer(
                1,
                3,
                gl::FLOAT,
                gl::FALSE,
                (VERTEX_COMPONENTS * float_size) as i32,
                ((3 * float_size) as *const usize).cast(),
            );
            gl.EnableVertexAttribArray(1);

            gl.VertexAttribPointer(
                2,
                2,
                gl::FLOAT,
                gl::FALSE,
                (VERTEX_COMPONENTS * float_size) as i32,
                ((6 * float_size) as *const usize).cast(),
            );
            gl.EnableVertexAttribArray(2);

            gl.BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
            gl.BufferData(gl::ELEMENT_ARRAY_BUFFER, INDEX_COUNT as isize, INDICES.as_ptr().cast(), gl::STATIC_DRAW);
        }

        VertexArray {
            gl,
            vertex_array,
            vertex_buffer,
            index_buffer,
        }
    }

    pub fn draw(&self) {
        unsafe {
            self.gl.BindVertexArray(self.vertex_array);

            self.gl.DrawElements(gl::TRIANGLES, INDEX_COUNT as i32, gl::UNSIGNED_BYTE, ptr::null());
        }
    }
}

impl Drop for VertexArray {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteVertexArrays(1, &self.vertex_array);
            self.gl.DeleteBuffers(1, &self.vertex_buffer);
            self.gl.DeleteBuffers(1, &self.index_buffer);
        }
    }
}

use stb_image::stb_image::bindgen;
use std::io::Read;

pub fn stbi_flip_vertical(flip: bool) {
    unsafe {
        bindgen::stbi_set_flip_vertically_on_load(flip as i32);
    }
}

struct Image {
    data: *mut u8,
    width: i32,
    height: i32,
}

impl Image {
    fn new(path: &str) -> Self {
        let mut file = File::open(path).unwrap();
        let mut contents = vec![];

        file.read_to_end(&mut contents).unwrap();

        let mut width = 0;
        let mut height = 0;
        let mut _components = 0;

        let data = unsafe {
            // stbi_load does not consistently succeed, even with an absolute path; fopen is the reason when failing
            bindgen::stbi_load_from_memory(contents.as_mut_ptr(), contents.len() as i32, &mut width, &mut height, &mut _components, 0)
        };

        Image { data, width, height }
    }
}

impl Drop for Image {
    fn drop(&mut self) {
        unsafe {
            bindgen::stbi_image_free(self.data.cast());
        }
    }
}

pub struct Texture {
    gl: Rc<gl::Gl>,
    texture: gl::types::GLuint,
}

impl Texture {
    pub fn new(gl: Rc<gl::Gl>) -> Self {
        let mut texture = 0;

        unsafe {
            gl.GenTextures(1, &mut texture);
        }

        Texture { gl, texture }
    }

    pub fn load(self, path: &str, format: gl::types::GLenum) -> Self {
        assert!((gl::RED..=gl::DEPTH_STENCIL).contains(&format));

        let gl = &self.gl;
        let texture = self.texture;

        unsafe {
            gl.BindTexture(gl::TEXTURE_2D, texture);

            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32);

            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
        }

        let image = Image::new(path);

        if image.data == ptr::null_mut() {
            println!("Failed to load texture");
        } else {
            unsafe {
                gl.TexImage2D(gl::TEXTURE_2D, 0, format as i32, image.width, image.height, 0, format, gl::UNSIGNED_BYTE, image.data.cast());
                gl.GenerateMipmap(gl::TEXTURE_2D);
            }
        }

        self
    }

    pub fn bind_active(&self, active: gl::types::GLenum) {
        assert!((gl::TEXTURE0..gl::TEXTURE0 + gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS).contains(&active));

        let gl = &self.gl;
        let texture = self.texture;

        unsafe {
            gl.ActiveTexture(active);
            gl.BindTexture(gl::TEXTURE_2D, texture);
        }
    }
}

impl Drop for Texture {
    fn drop(&mut self) {
        unsafe {
            self.gl.DeleteTextures(1, &self.texture);
        }
    }
}
