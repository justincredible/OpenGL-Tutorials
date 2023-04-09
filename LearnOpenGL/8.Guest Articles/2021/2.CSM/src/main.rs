use glam::{Mat4, Quat, Vec3, Vec4};
use glfw::{Action, Context, CursorMode, Key, Window, WindowEvent, WindowHint};
use std::{f32::consts::PI, mem::size_of, ptr};

const SCR_WIDTH: u32 = 2560;
const SCR_HEIGHT: u32 = 1440;

const CAMERA_NEAR: f32 = 0.1;
const CAMERA_FAR: f32 = 500.0;
const SHADOW_CASCADE_LEVELS: [f32; 4] = [CAMERA_FAR / 50.0, CAMERA_FAR / 25.0, CAMERA_FAR / 10.0, CAMERA_FAR / 2.0];
const DEPTH_MAP_RESOLUTION: i32 = 4096;

pub mod camera;
use camera::camera::{Camera, Movement};
pub mod mesh;
use mesh::mesh::{Texture, VertexArray};
pub mod shader;
use shader::shader::Program;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(WindowHint::ContextVersionMajor(4));
    glfw.window_hint(WindowHint::ContextVersionMinor(6));
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

    ogl::depth_enable(true);

    let quad = VertexArray::new_quad();
    let cube = VertexArray::new_cube();

    let shader = Program::new().link("src/10.shadow_mapping.vs", "src/10.shadow_mapping.fs", None);
    let simple_depth_shader = Program::new().link("src/10.shadow_mapping_depth.vs", "src/10.shadow_mapping_depth.fs", Some("src/10.shadow_mapping_depth.gs"));
    let debug_depth_quad = Program::new().link("src/10.debug_quad.vs", "src/10.debug_quad_depth.fs", None);
    let debug_cascade_shader = Program::new().link("src/10.debug_cascade.vs", "src/10.debug_cascade.fs", None);

    let plane = VertexArray::new_plane();

    let wood_tex = Texture::new().load("resources/textures/wood.png");

    let light_fbo = Framebuffer::new();

    let matrices_ubo = UniformBufferObject::new();

    shader.apply();
    shader.set_int("diffuseTexture", 0);
    shader.set_int("shadowMap", 1);
    debug_depth_quad.apply();
    debug_depth_quad.set_int("depthMap", 0);

    glfw.poll_events();

    let mut model_matrices = Vec::new();
    for _ in 0..10 {
        let scale = Vec3::new(fastrand::f32() + 1.0, fastrand::f32() + 1.0, fastrand::f32() + 1.0);
        let rotation = Quat::from_axis_angle(Vec3::new(1.0, 0.0, 1.0), fastrand::f32() * PI);
        let translation = Vec3::new(fastrand::f32() * 20.0 - 10.0, fastrand::f32() * 20.0, fastrand::f32() * 20.0 - 10.0);
        model_matrices.push(Mat4::from_scale_rotation_translation(scale, rotation, translation));
    }
    let mut cache = Vec::new();
    let mut input_state = InputState {
        show_quad: false,
        c_press: Action::Release,
        f_press: Action::Release,
        plus_press: Action::Release,
        debug_layer: 0,
        light_dir: Vec3::new(20.0, 50.0, 20.0).normalize(),
    };

    let (x_pos, y_pos) = window.get_cursor_pos();
    let mut camera = Camera::new(Vec3::new(0.0, 0.0, 3.0), x_pos as f32, y_pos as f32);

    let mut last_frame = 0.0;

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        let delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_input(&mut camera, &mut window, &mut input_state, &mut cache, delta_time);

        ogl::clear_color(0.1, 0.1, 0.1, 1.0);
        ogl::clear();

        let light_matrices = get_light_space_matrices(&mut camera, input_state.light_dir);
        matrices_ubo.bind();
        for i in 0..light_matrices.len() {
            unsafe {
                gl::BufferSubData(
                    gl::UNIFORM_BUFFER,
                    (i * size_of::<Mat4>()) as isize,
                    size_of::<Mat4>() as isize,
                    light_matrices[i].to_cols_array().as_ptr().cast(),
                );
            }
        }
        ogl::unbind_ubo();

        simple_depth_shader.apply();

        light_fbo.bind();
        ogl::viewport(DEPTH_MAP_RESOLUTION, DEPTH_MAP_RESOLUTION);
        ogl::clear();
        ogl::cull_front_face(true);
        render_scene(&simple_depth_shader, &mut model_matrices, &plane, &cube);
        ogl::cull_front_face(false);
        ogl::unbind_framebuffer();

        ogl::viewport(SCR_WIDTH as i32, SCR_HEIGHT as i32);
        ogl::clear();

        shader.apply();

        let projection = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, CAMERA_NEAR, CAMERA_FAR);
        let view = camera.view_matrix();
        shader.set_mat4("projection", projection);
        shader.set_mat4("view", view);
        shader.set_vec3("viewPos", camera.position());
        shader.set_vec3("lightDir", input_state.light_dir);
        shader.set_float("farPlane", CAMERA_FAR);
        shader.set_int("cascadeCount", SHADOW_CASCADE_LEVELS.len() as i32);
        for i in 0..SHADOW_CASCADE_LEVELS.len() {
            shader.set_float(&("cascadePlaneDistances[".to_string() + &(i.to_string() + "]")), SHADOW_CASCADE_LEVELS[i]);
        }
        ogl::active_texture(0);
        wood_tex.bind();
        ogl::active_texture(1);
        light_fbo.bind_texture();
        render_scene(&shader, &mut model_matrices, &plane, &cube);

        if cache.len() > 0 {
            ogl::blend_enable(true);
            ogl::blend_func();
            debug_cascade_shader.apply();
            debug_cascade_shader.set_mat4("projection", projection);
            debug_cascade_shader.set_mat4("view", view);
            draw_cascade_volume_visualizers(&cache, &debug_cascade_shader);
            ogl::blend_enable(false);
        }

        debug_depth_quad.apply();
        debug_depth_quad.set_int("layer", input_state.debug_layer);
        ogl::active_texture(0);
        light_fbo.bind_texture();
        if input_state.show_quad {
            quad.bind();
            quad.draw();
        }

        window.swap_buffers();

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&mut camera, &mut window, event);
        }
    }
}

fn render_scene(shader: &Program, model_matrices: &mut Vec<Mat4>, plane: &VertexArray, cube: &VertexArray) {
    shader.set_mat4("model", Mat4::IDENTITY);
    plane.bind();
    plane.draw();

    cube.bind();
    for model in model_matrices {
        shader.set_mat4("model", *model);
        cube.draw()
    }
}

fn draw_cascade_volume_visualizers(light_matrices: &Vec<Mat4>, shader: &Program) {
    const INDICES: [u32; 36] = [0, 2, 3, 0, 3, 1, 4, 6, 2, 4, 2, 0, 5, 7, 6, 5, 6, 4, 1, 3, 7, 1, 7, 5, 6, 7, 3, 6, 3, 2, 1, 5, 4, 0, 1, 4];

    const COLORS: [Vec4; 3] = [Vec4::new(1.0, 0.0, 0.0, 0.5), Vec4::new(0.0, 1.0, 0.0, 0.5), Vec4::new(0.0, 0.0, 1.0, 0.5)];

    let mut visualizer_vaos = Vec::new();
    let mut visualizer_vbos = Vec::new();
    let mut visualizer_ebos = Vec::new();

    for i in 0..light_matrices.len() {
        let corners = get_frustum_corners_world_space(light_matrices[i]);
        let mut vec3s = Vec::new();
        for v in corners {
            vec3s.push(v.truncate());
        }

        visualizer_vaos.push(0);
        visualizer_vbos.push(0);
        visualizer_ebos.push(0);

        unsafe {
            gl::GenVertexArrays(1, &mut visualizer_vaos[i]);
            gl::GenBuffers(1, &mut visualizer_vbos[i]);
            gl::GenBuffers(1, &mut visualizer_ebos[i]);

            gl::BindVertexArray(visualizer_vaos[i]);

            gl::BindBuffer(gl::ARRAY_BUFFER, visualizer_vbos[i]);
            gl::BufferData(gl::ARRAY_BUFFER, (vec3s.len() * size_of::<Vec3>()) as isize, vec3s.as_ptr().cast(), gl::STATIC_DRAW);

            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, visualizer_ebos[i]);
            gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (36 * size_of::<u32>()) as isize, INDICES.as_ptr().cast(), gl::STATIC_DRAW);

            gl::EnableVertexAttribArray(0);
            gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, size_of::<Vec3>() as i32, ptr::null());

            gl::BindVertexArray(visualizer_vaos[i]);
            shader.set_vec4("color", COLORS[i % 3]);
            gl::DrawElements(gl::TRIANGLES, 36, gl::UNSIGNED_INT, ptr::null());

            gl::DeleteBuffers(1, &visualizer_vbos[i]);
            gl::DeleteBuffers(1, &visualizer_ebos[i]);
            gl::DeleteVertexArrays(1, &visualizer_vaos[i]);

            gl::BindVertexArray(0);
        }
    }
}

pub struct InputState {
    show_quad: bool,
    c_press: Action,
    f_press: Action,
    plus_press: Action,
    debug_layer: i32,
    light_dir: Vec3,
}

fn process_input(camera: &mut Camera, window: &mut Window, input_state: &mut InputState, cache: &mut Vec<Mat4>, delta_time: f32) {
    if window.get_key(Key::Escape) == Action::Press {
        window.set_should_close(true);
    }

    camera.speed = if window.get_key(Key::LeftShift) == Action::Press { 2.5 * 10.0 } else { 2.5 };

    if window.get_key(Key::W) == Action::Press {
        camera.process_keyboard(Movement::Forward, delta_time);
    }
    if window.get_key(Key::S) == Action::Press {
        camera.process_keyboard(Movement::Backward, delta_time);
    }
    if window.get_key(Key::A) == Action::Press {
        camera.process_keyboard(Movement::Left, delta_time);
    }
    if window.get_key(Key::D) == Action::Press {
        camera.process_keyboard(Movement::Right, delta_time);
    }

    if window.get_key(Key::F) == Action::Release && input_state.f_press == Action::Press {
        input_state.show_quad = !input_state.show_quad;
    }
    input_state.f_press = window.get_key(Key::F);

    if window.get_key(Key::KpAdd) == Action::Release && input_state.plus_press == Action::Press {
        input_state.debug_layer = (input_state.debug_layer + 1) % SHADOW_CASCADE_LEVELS.len() as i32;
    }
    input_state.plus_press = window.get_key(Key::KpAdd);

    if window.get_key(Key::C) == Action::Release && input_state.c_press == Action::Press {
        *cache = get_light_space_matrices(camera, input_state.light_dir);
    }
    input_state.c_press = window.get_key(Key::C);
}

fn handle_window_event(camera: &mut Camera, window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        WindowEvent::FramebufferSize(width, height) => ogl::viewport(width, height),
        WindowEvent::Scroll(_, y_offset) => camera.process_scroll(y_offset as f32),
        WindowEvent::CursorPos(_x_pos, _y_pos) => {
            let (x_pos, y_pos) = window.get_cursor_pos();

            camera.process_mouse(x_pos as f32, y_pos as f32, true);
        }
        _ => {}
    }
}

fn get_frustum_corners_world_space(projview: Mat4) -> Vec<Vec4> {
    let inv = projview.inverse();

    let mut frustum_corners = Vec::new();
    for x in 0..2 {
        for y in 0..2 {
            for z in 0..2 {
                let pt = inv * Vec4::new(2.0 * x as f32 - 1.0, 2.0 * y as f32 - 1.0, 2.0 * z as f32 - 1.0, 1.0);
                frustum_corners.push(pt / pt.w);
            }
        }
    }

    frustum_corners
}

fn get_light_space_matrix(camera: &mut Camera, light_dir: Vec3, near_plane: f32, far_plane: f32) -> Mat4 {
    let proj = Mat4::perspective_lh(camera.zoom(), SCR_WIDTH as f32 / SCR_HEIGHT as f32, near_plane, far_plane);
    let corners = get_frustum_corners_world_space(proj * camera.view_matrix());

    let mut center = Vec3::ZERO;
    for v in &corners {
        center += v.truncate();
    }
    center /= corners.len() as f32;

    let light_view = look_at(center + light_dir, center, Vec3::Y);

    let mut min_x = f32::MAX;
    let mut max_x = f32::MIN;
    let mut min_y = f32::MAX;
    let mut max_y = f32::MIN;
    let mut min_z = f32::MAX;
    let mut max_z = f32::MIN;

    for v in corners {
        let trf = light_view * v;
        min_x = f32::min(min_x, trf.x);
        max_x = f32::max(max_x, trf.x);
        min_y = f32::min(min_y, trf.y);
        max_y = f32::max(max_y, trf.y);
        min_z = f32::min(min_z, trf.z);
        max_z = f32::max(max_z, trf.z);
    }

    let z_mult = 10.0;
    if min_z < 0.0 {
        min_z *= z_mult;
    } else {
        min_z /= z_mult;
    }
    if max_z < 0.0 {
        max_z /= z_mult;
    } else {
        max_z *= z_mult;
    }

    let light_projection = Mat4::orthographic_lh(min_x, max_x, min_y, max_y, min_z, max_z);

    light_projection * light_view
}

fn get_light_space_matrices(camera: &mut Camera, light_dir: Vec3) -> Vec<Mat4> {
    let mut ret = Vec::new();
    for i in 0..=SHADOW_CASCADE_LEVELS.len() {
        if i == 0 {
            ret.push(get_light_space_matrix(camera, light_dir, CAMERA_NEAR, SHADOW_CASCADE_LEVELS[i]));
        } else if i < SHADOW_CASCADE_LEVELS.len() {
            ret.push(get_light_space_matrix(camera, light_dir, SHADOW_CASCADE_LEVELS[i - 1], SHADOW_CASCADE_LEVELS[i]));
        } else {
            ret.push(get_light_space_matrix(camera, light_dir, SHADOW_CASCADE_LEVELS[i - 1], CAMERA_FAR));
        }
    }
    ret
}

fn look_at(eye: Vec3, center: Vec3, up: Vec3) -> Mat4 {
    let f = (center - eye).normalize();
    let up = up.normalize();
    let s = f.cross(up).normalize();
    let u = s.cross(f).normalize();

    Mat4::from_cols_array(&[s.x, u.x, f.x, 0.0, s.y, u.y, f.y, 0.0, s.z, u.z, f.z, 0.0, -s.dot(eye), -u.dot(eye), -f.dot(eye), 1.0])
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

    pub fn viewport(width: i32, height: i32) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }

    pub fn depth_enable(on: bool) {
        unsafe {
            if on {
                gl::Enable(gl::DEPTH_TEST);
            } else {
                gl::Disable(gl::DEPTH_TEST);
            }
        }
    }

    pub fn cull_front_face(front: bool) {
        unsafe {
            if front {
                gl::CullFace(gl::FRONT);
            } else {
                gl::CullFace(gl::BACK);
            }
        }
    }

    pub fn blend_enable(on: bool) {
        unsafe {
            if on {
                gl::Enable(gl::BLEND);
            } else {
                gl::Disable(gl::BLEND);
            }
        }
    }

    pub fn blend_func() {
        unsafe {
            gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        }
    }

    pub fn unbind_ubo() {
        unsafe {
            gl::BindBuffer(gl::UNIFORM_BUFFER, 0);
        }
    }

    pub fn unbind_framebuffer() {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }
    }

    pub fn active_texture(unit: u32) {
        assert!(unit < gl::MAX_COMBINED_TEXTURE_IMAGE_UNITS);

        unsafe {
            gl::ActiveTexture(gl::TEXTURE0 + unit);
        }
    }
}

pub struct Framebuffer {
    framebuffer: gl::types::GLuint,
    texture: gl::types::GLuint,
}

impl Framebuffer {
    pub fn new() -> Self {
        let mut framebuffer = 0;
        let mut texture = 0;

        unsafe {
            gl::GenFramebuffers(1, &mut framebuffer);
            gl::GenTextures(1, &mut texture);

            gl::BindTexture(gl::TEXTURE_2D_ARRAY, texture);
            gl::TexImage3D(
                gl::TEXTURE_2D_ARRAY,
                0,
                gl::DEPTH_COMPONENT32F as i32,
                DEPTH_MAP_RESOLUTION,
                DEPTH_MAP_RESOLUTION,
                (SHADOW_CASCADE_LEVELS.len() + 1) as i32,
                0,
                gl::DEPTH_COMPONENT,
                gl::FLOAT,
                ptr::null(),
            );

            let nearest = gl::NEAREST as i32;
            let clamp_border = gl::CLAMP_TO_BORDER as i32;
            gl::TexParameteri(gl::TEXTURE_2D_ARRAY, gl::TEXTURE_MIN_FILTER, nearest);
            gl::TexParameteri(gl::TEXTURE_2D_ARRAY, gl::TEXTURE_MAG_FILTER, nearest);
            gl::TexParameteri(gl::TEXTURE_2D_ARRAY, gl::TEXTURE_WRAP_S, clamp_border);
            gl::TexParameteri(gl::TEXTURE_2D_ARRAY, gl::TEXTURE_WRAP_T, clamp_border);

            let border_color = [1.0, 1.0, 1.0, 1.0];
            gl::TexParameterfv(gl::TEXTURE_2D_ARRAY, gl::TEXTURE_BORDER_COLOR, border_color.as_ptr());

            gl::BindFramebuffer(gl::FRAMEBUFFER, framebuffer);
            gl::FramebufferTexture(gl::FRAMEBUFFER, gl::DEPTH_ATTACHMENT, texture, 0);
            gl::DrawBuffer(gl::NONE);
            gl::ReadBuffer(gl::NONE);

            if gl::CheckFramebufferStatus(gl::FRAMEBUFFER) != gl::FRAMEBUFFER_COMPLETE {
                println!("ERROR::FRAMEBUFFER:: Framebuffer is not complete!")
            }
            gl::BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        Framebuffer { framebuffer, texture }
    }

    pub fn bind(&self) {
        unsafe {
            gl::BindFramebuffer(gl::FRAMEBUFFER, self.framebuffer);
        }
    }

    pub fn bind_texture(&self) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D_ARRAY, self.texture);
        }
    }
}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteFramebuffers(1, &self.framebuffer);
            gl::DeleteTextures(1, &self.texture);
        }
    }
}

pub struct UniformBufferObject {
    id: u32,
}

impl UniformBufferObject {
    pub fn new() -> Self {
        let mut id = 0;

        unsafe {
            gl::GenBuffers(1, &mut id);
            gl::BindBuffer(gl::UNIFORM_BUFFER, id);
            gl::BufferData(gl::UNIFORM_BUFFER, size_of::<Mat4>() as isize * 16, ptr::null(), gl::STATIC_DRAW);
            gl::BindBufferBase(gl::UNIFORM_BUFFER, 0, id);
            gl::BindBuffer(gl::UNIFORM_BUFFER, 0);
        }

        UniformBufferObject { id }
    }

    pub fn bind(&self) {
        unsafe {
            gl::BindBuffer(gl::UNIFORM_BUFFER, self.id);
        }
    }
}

impl Drop for UniformBufferObject {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteBuffers(1, &self.id);
        }
    }
}
