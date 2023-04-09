use glfw::{Context, WindowEvent, WindowHint};

const SCR_WIDTH: u32 = 800;
const SCR_HEIGHT: u32 = 600;
const TEX_WIDTH: i32 = 1000;
const TEX_HEIGHT: i32 = 1000;

pub mod mesh;
use mesh::mesh::{Texture, VertexArray};
pub mod shader;
use shader::shader::Program;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(4));
    glfw.window_hint(WindowHint::ContextVersionMinor(3));
    glfw.window_hint(WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw
        .create_window(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_framebuffer_size_polling(true);
    glfw.set_swap_interval(glfw::SwapInterval::None);
    window.focus();

    gl::load_with(|s| window.get_proc_address(s) as *const _);

    let quad = VertexArray::new_quad();

    let mut max_compute_work_group_count = [0, 0, 0];
    let mut max_compute_work_group_size = [0, 0, 0];
    let mut max_compute_work_group_invocations = 0;

    for idx in 0..3 {
        ogl::get_indexed_integer_value(gl::MAX_COMPUTE_WORK_GROUP_COUNT, idx, &mut max_compute_work_group_count[idx as usize]);
        ogl::get_indexed_integer_value(gl::MAX_COMPUTE_WORK_GROUP_SIZE, idx, &mut max_compute_work_group_size[idx as usize]);
    }
    ogl::get_integer_value(gl::MAX_COMPUTE_WORK_GROUP_INVOCATIONS, &mut max_compute_work_group_invocations);

    println!("OpenGL Limitations: ");
    println!("maxmimum number of work groups in X dimension {}", max_compute_work_group_count[0]);
    println!("maxmimum number of work groups in Y dimension {}", max_compute_work_group_count[1]);
    println!("maxmimum number of work groups in Z dimension {}", max_compute_work_group_count[2]);

    println!("maxmimum size of a work group in X dimension {}", max_compute_work_group_size[0]);
    println!("maxmimum size of a work group in Y dimension {}", max_compute_work_group_size[1]);
    println!("maxmimum size of a work group in Z dimension {}", max_compute_work_group_size[2]);

    println!(
        "Number of invocations in a single local work group that may be dispatched to a compute shader {}",
        max_compute_work_group_invocations
    );

    let screen_quad = Program::new().link("src/screenQuad.vs", "src/screenQuad.fs");
    let compute_shader = Program::new().link_compute("src/computeShader.cs");

    screen_quad.apply();
    screen_quad.set_int("tex", 0);

    let texture = Texture::new();

    ogl::active_texture(0);
    texture.bind();

    let mut f_counter = 0;

    glfw.poll_events();

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        if f_counter > 500 {
            println!("FPS: {}", 1.0 / delta_time);
            f_counter = 0;
        } else {
            f_counter += 1;
        }

        compute_shader.apply();
        compute_shader.set_float("t", current_frame);

        ogl::dispatch_compute();

        ogl::memory_barrier();

        ogl::clear();
        screen_quad.apply();

        quad.bind();
        quad.draw();

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&mut window, event);
        }
    }
}

fn handle_window_event(_window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        WindowEvent::FramebufferSize(width, height) => ogl::viewport(width, height),
        _ => {}
    }
}

mod ogl {
    pub fn clear() {
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
        }
    }

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }

    pub fn active_texture(unit: u32) {
        assert!(unit < gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            gl::ActiveTexture(gl::TEXTURE0 + unit);
        }
    }

    pub fn get_integer_value(param: u32, data: &mut i32) {
        unsafe {
            gl::GetIntegerv(param, data);
        }
    }

    pub fn get_indexed_integer_value(param: u32, index: u32, data: &mut i32) {
        unsafe {
            gl::GetIntegeri_v(param, index, data);
        }
    }

    pub fn dispatch_compute() {
        unsafe {
            gl::DispatchCompute(crate::TEX_WIDTH as u32 / 10, crate::TEX_HEIGHT as u32 / 10, 1);
        }
    }

    pub fn memory_barrier() {
        unsafe {
            gl::MemoryBarrier(gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }
}
