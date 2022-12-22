// PROGRAM.CPP
use glfw::{Action, Context, Key, WindowEvent, WindowHint, WindowMode};

pub mod ball_object;
use ball_object::ball_object::*;
pub mod game;
use game::game::*;
pub mod game_level;
use game_level::game_level::*;
pub mod game_object;
use game_object::game_object::*;
pub mod particle_generator;
use particle_generator::particle_generator::*;
pub mod post_processor;
use post_processor::post_processor::*;
pub mod power_up;
use power_up::power_up::*;
pub mod resource_manager;
use resource_manager::resource_manager::*;
pub mod shader;
use shader::shader::*;
pub mod sound_engine;
use sound_engine::sound_engine::*;
pub mod sprite_renderer;
use sprite_renderer::sprite_renderer::*;
pub mod text_renderer;
use text_renderer::text_renderer::*;
pub mod texture;
use texture::texture::*;

const SCREEN_WIDTH: u32 = 800;
const SCREEN_HEIGHT: u32 = 600;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).expect("GLFW token.");

    glfw.window_hint(WindowHint::ContextVersionMajor(3));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw
        .create_window(SCREEN_WIDTH, SCREEN_HEIGHT, "Breakout", WindowMode::Windowed)
        .expect("Tuple of window and events receiver.");
    window.make_current();
    window.focus();

    gl::load_with(|s| window.get_proc_address(s) as *const _);

    window.set_key_polling(true);
    window.set_framebuffer_size_polling(true);

    ogl::viewport(SCREEN_WIDTH as i32, SCREEN_HEIGHT as i32);
    ogl::blend_enable();

    let mut breakout = Game::new(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32);
    breakout.init();

    glfw.poll_events();

    let mut delta_time;
    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        delta_time = current_frame - last_frame;
        last_frame = current_frame;

        breakout.process_input(&mut window, delta_time);
        breakout.update(delta_time);

        ogl::clear_color(0.0, 0.0, 0.0, 1.0);
        ogl::clear();
        breakout.render(current_frame);

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&mut breakout, &mut window, event);
        }
    }
}

fn handle_window_event(breakout: &mut Game, window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        WindowEvent::FramebufferSize(width, height) => ogl::viewport(width, height),
        WindowEvent::Key(Key::Escape, _, Action::Press, _) => window.set_should_close(true),
        WindowEvent::Key(key, _, action, _) => {
            if key as i32 >= 0 && (key as i32) < 1024 {
                match action {
                    Action::Press => breakout.keys[key as usize] = true,
                    Action::Release => {
                        breakout.keys[key as usize] = false;
                        breakout.keys_processed[key as usize] = false;
                    }
                    _ => {}
                }
            }
        }
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
            gl::Clear(gl::COLOR_BUFFER_BIT);
        }
    }

    pub fn blend_enable() {
        unsafe {
            gl::Enable(gl::BLEND);
            gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        }
    }

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }
}
