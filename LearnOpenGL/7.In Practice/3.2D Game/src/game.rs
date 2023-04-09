pub mod game {
    use crate::{BallObject, GameLevel, GameObject, ParticleGenerator, PostProcessor, PowerUp, ResourceManager, SoundEngine, SpriteRenderer, TextRenderer};
    use glam::{Mat4, Vec2, Vec3};
    use glfw::Window;
    use std::mem::take;

    enum GameState {
        Active,
        Menu,
        Win,
    }

    enum Direction {
        Up,
        Right,
        Down,
        Left,
    }

    impl TryFrom<i32> for Direction {
        type Error = ();

        fn try_from(v: i32) -> Result<Self, Self::Error> {
            match v {
                0 => Ok(Direction::Up),
                1 => Ok(Direction::Right),
                2 => Ok(Direction::Down),
                3 => Ok(Direction::Left),
                _ => Err(()),
            }
        }
    }

    const KEYS_SIZE: usize = 1024;
    const PLAYER_SIZE: Vec2 = Vec2::new(100.0, 20.0);
    const PLAYER_VELOCITY: f32 = 500.0;
    const INITIAL_BALL_VELOCITY: Vec2 = Vec2::new(100.0, -350.0);
    const BALL_RADIUS: f32 = 12.5;

    pub struct Game {
        state: GameState,
        pub keys: [bool; KEYS_SIZE],
        pub keys_processed: [bool; KEYS_SIZE],
        screen_size: (u32, u32),
        levels: Vec<GameLevel>,
        power_ups: Vec<PowerUp>,
        level: u32,
        lives: u32,
        shake_time: f32,
        resources: ResourceManager,
        ex: Option<GameEx>,
    }

    struct GameEx {
        renderer: SpriteRenderer,
        player: GameObject,
        ball: BallObject,
        particles: ParticleGenerator,
        effects: PostProcessor,
        text: TextRenderer,
        sound_engine: SoundEngine,
    }

    type Collision = (bool, Direction, Vec2);

    impl Game {
        pub fn new(width: u32, height: u32) -> Self {
            Game {
                state: GameState::Menu,
                keys: [false; KEYS_SIZE],
                keys_processed: [false; KEYS_SIZE],
                screen_size: (width, height),
                levels: Vec::new(),
                power_ups: Vec::new(),
                level: 0,
                lives: 3,
                shake_time: 0.0,
                resources: ResourceManager::new(),
                ex: None,
            }
        }

        pub fn init(&mut self) {
            let (width, height) = self.screen_size;

            let projection = Mat4::orthographic_lh(0.0, width as f32, height as f32, 0.0, -1.0, 1.0);
            let sprite_shader = ResourceManager::load_shader("src/sprite.vs", "src/sprite.fs");
            sprite_shader.r#use();
            sprite_shader.set_int("sprite", 0);
            sprite_shader.set_mat4("projection", projection);
            let particle_shader = ResourceManager::load_shader("src/particle.vs", "src/particle.fs");
            particle_shader.r#use();
            particle_shader.set_int("sprite", 0);
            particle_shader.set_mat4("projection", projection);
            let post_processing_shader = ResourceManager::load_shader("src/post_processing.vs", "src/post_processing.fs");

            self.resources.load_texture("resources/textures/background.jpg", false, "background");
            self.resources.load_texture("resources/textures/awesomeface.png", true, "face");
            self.resources.load_texture("resources/textures/block.png", false, "block");
            self.resources.load_texture("resources/textures/block_solid.png", false, "block_solid");
            self.resources.load_texture("resources/textures/paddle.png", true, "paddle");
            self.resources.load_texture("resources/textures/particle.png", true, "particle");
            self.resources.load_texture("resources/textures/powerup_speed.png", true, "powerup_speed");
            self.resources.load_texture("resources/textures/powerup_sticky.png", true, "powerup_sticky");
            self.resources.load_texture("resources/textures/powerup_passthrough.png", true, "powerup_passthrough");
            self.resources.load_texture("resources/textures/powerup_increase.png", true, "powerup_increase");
            self.resources.load_texture("resources/textures/powerup_confuse.png", true, "powerup_confuse");
            self.resources.load_texture("resources/textures/powerup_chaos.png", true, "powerup_chaos");

            let renderer = SpriteRenderer::new(sprite_shader);
            let particles = ParticleGenerator::new(particle_shader, self.resources.get_texture("particle"), 500);
            let effects = PostProcessor::new(post_processing_shader, width, height);
            let mut text = TextRenderer::new(width as i32, height as i32);
            text.load("resources/fonts/OCRAEXT.TTF", 24);

            let mut one = GameLevel::new();
            one.load("resources/levels/one.lvl", width, height / 2, &self.resources);
            let mut two = GameLevel::new();
            two.load("resources/levels/two.lvl", width, height / 2, &self.resources);
            let mut three = GameLevel::new();
            three.load("resources/levels/three.lvl", width, height / 2, &self.resources);
            let mut four = GameLevel::new();
            four.load("resources/levels/four.lvl", width, height / 2, &self.resources);
            self.levels.push(one);
            self.levels.push(two);
            self.levels.push(three);
            self.levels.push(four);

            let player_pos = Vec2::new(width as f32 / 2.0 - PLAYER_SIZE.x / 2.0, height as f32 - PLAYER_SIZE.y);
            let player = GameObject::new_with(player_pos, PLAYER_SIZE, self.resources.get_texture("paddle"), Vec3::ONE, Vec2::ZERO);
            let ball_pos = player_pos + Vec2::new(PLAYER_SIZE.x / 2.0 - BALL_RADIUS, -BALL_RADIUS * 2.0);
            let ball = BallObject::new_with(ball_pos, BALL_RADIUS, INITIAL_BALL_VELOCITY, self.resources.get_texture("face"));

            let mut sound_engine = SoundEngine::new();
            sound_engine.play_2d("resources/audio/breakout.wav", true);

            self.ex = Some(GameEx {
                renderer,
                player,
                ball,
                particles,
                effects,
                text,
                sound_engine,
            });
        }

        pub fn update(&mut self, dt: f32) {
            let (width, height) = self.screen_size;

            let ball = &mut self.ex.as_mut().unwrap().ball;

            ball.r#move(dt, width);

            self.do_collisions();

            let ex = self.ex.as_mut().unwrap();

            let particles = &mut ex.particles;
            particles.update(dt, &ex.ball.as_go(), 2, Vec2::splat(ex.ball.radius / 2.0));

            self.update_power_ups(dt);

            let ex = self.ex.as_mut().unwrap();

            if self.shake_time > 0.0 {
                self.shake_time -= dt;
                if self.shake_time <= 0.0 {
                    ex.effects.shake = false;
                }
            }
            if ex.ball.position().y >= height as f32 {
                self.lives -= 1;

                if self.lives == 0 {
                    self.reset_level();
                    self.state = GameState::Menu;
                }
                self.reset_player();
            }
            if let GameState::Active = self.state {
                if self.levels[self.level as usize].is_completed() {
                    self.reset_level();
                    self.reset_player();
                    self.ex.as_mut().unwrap().effects.chaos = true;
                    self.state = GameState::Win;
                }
            }
        }

        pub fn process_input(&mut self, _window: &mut Window, dt: f32) {
            let (width, _height) = self.screen_size;

            match self.state {
                GameState::Menu => {
                    if self.keys[glfw::ffi::KEY_ENTER as usize] && !self.keys_processed[glfw::ffi::KEY_ENTER as usize] {
                        self.state = GameState::Active;
                        self.keys_processed[glfw::ffi::KEY_ENTER as usize] = true;
                    }
                    if self.keys[glfw::ffi::KEY_W as usize] && !self.keys_processed[glfw::ffi::KEY_W as usize] {
                        self.level = (self.level + 1) % 4;
                        self.keys_processed[glfw::ffi::KEY_W as usize] = true;
                    }
                    if self.keys[glfw::ffi::KEY_S as usize] && !self.keys_processed[glfw::ffi::KEY_S as usize] {
                        if self.level > 0 {
                            self.level -= 1;
                        } else {
                            self.level = 3;
                        }
                        self.keys_processed[glfw::ffi::KEY_S as usize] = true;
                    }
                }

                GameState::Win => {
                    if self.keys[glfw::ffi::KEY_ENTER as usize] {
                        self.keys_processed[glfw::ffi::KEY_ENTER as usize] = true;
                        self.ex.as_mut().unwrap().effects.chaos = false;
                        self.state = GameState::Menu;
                    }
                }

                GameState::Active => {
                    let velocity = PLAYER_VELOCITY * dt;
                    let ex = self.ex.as_mut().unwrap();
                    let player = &mut ex.player;
                    let ball = &mut ex.ball;

                    if self.keys[glfw::ffi::KEY_A as usize] {
                        if player.position.x >= 0.0 {
                            player.position.x -= velocity;
                            if ball.stuck {
                                ball.position_mut().x -= velocity;
                            }
                        }
                    }
                    if self.keys[glfw::ffi::KEY_D as usize] {
                        if player.position.x <= width as f32 - player.size.x {
                            player.position.x += velocity;
                            if ball.stuck {
                                ball.position_mut().x += velocity;
                            }
                        }
                    }
                    if self.keys[glfw::ffi::KEY_SPACE as usize] {
                        ball.stuck = false;
                    }
                }
            }
        }

        pub fn render(&mut self, current_time: f32) {
            let (width, height) = self.screen_size;
            let ex = self.ex.as_mut().unwrap();
            let text = &ex.text;

            let effects = &ex.effects;
            effects.begin_render();
            let renderer = &ex.renderer;
            renderer.draw_sprite(
                &self.resources.get_texture("background"),
                Vec2::new(0.0, 0.0),
                Vec2::new(width as f32, height as f32),
                0.0,
                Vec3::ZERO,
            );
            self.levels[self.level as usize].draw(&renderer);
            ex.player.draw(renderer);
            for power_up in &self.power_ups {
                if !power_up.is_destroyed() {
                    power_up.draw(renderer);
                }
            }
            ex.particles.draw();
            ex.ball.draw(renderer);
            effects.end_render();
            effects.render(current_time);
            text.render_text(&("Lives:".to_string() + &self.lives.to_string()), 5.0, 5.0, 1.0, Vec3::ONE);

            if let GameState::Menu = self.state {
                text.render_text("Press ENTER to start", 250.0, height as f32 / 2.0, 1.0, Vec3::ZERO);
                text.render_text("Press W or S to select level", 245.0, height as f32 / 2.0 + 20.0, 0.75, Vec3::ZERO);
            }
            if let GameState::Win = self.state {
                text.render_text("You WON!!!", 320.0, height as f32 / 2.0 - 20.0, 1.0, Vec3::new(0.0, 1.0, 0.0));
                text.render_text("Press ENTER to retry or ESC to quit", 130.0, height as f32 / 2.0, 1.0, Vec3::new(1.0, 1.0, 0.0));
            }
        }

        pub fn reset_level(&mut self) {
            let (width, height) = self.screen_size;

            match self.level {
                0 => self.levels[0].load("resources/levels/one.lvl", width, height / 2, &self.resources),
                1 => self.levels[1].load("resources/levels/two.lvl", width, height / 2, &self.resources),
                2 => self.levels[2].load("resources/levels/three.lvl", width, height / 2, &self.resources),
                3 => self.levels[3].load("resources/levels/four.lvl", width, height / 2, &self.resources),
                _ => panic!("Unexpected level."),
            }

            self.lives = 3;
        }

        pub fn reset_player(&mut self) {
            let (width, height) = self.screen_size;
            let ex = self.ex.as_mut().unwrap();
            let player = &mut ex.player;
            let ball = &mut ex.ball;
            let effects = &mut ex.effects;

            player.size = PLAYER_SIZE;
            player.position = Vec2::new(width as f32 / 2.0 - PLAYER_SIZE.x / 2.0, height as f32 - PLAYER_SIZE.y);
            ball.reset(player.position + Vec2::new(PLAYER_SIZE.x / 2.0 - BALL_RADIUS, -(BALL_RADIUS * 2.0)), INITIAL_BALL_VELOCITY);
            effects.chaos = false;
            effects.confuse = false;
            ball.pass_through = false;
            ball.sticky = false;
            player.color = Vec3::ONE;
            *ball.color() = Vec3::ONE;
        }

        fn update_power_ups(&mut self, dt: f32) {
            let active_types = self.power_ups.iter().filter(|pu| pu.activated).map(|pu| pu.r#type.clone()).collect::<Vec<_>>();
            for power_up in &mut self.power_ups {
                let velocity = *power_up.velocity();
                *power_up.position() += velocity * dt;
                if power_up.activated {
                    power_up.duration -= dt;

                    let ex = self.ex.as_mut().unwrap();
                    let player = &mut ex.player;
                    let ball = &mut ex.ball;
                    let effects = &mut ex.effects;

                    if power_up.duration <= 0.0 {
                        power_up.activated = false;
                        if power_up.r#type == "sticky" {
                            if !active_types.contains(&"sticky".to_string()) {
                                ball.sticky = false;
                                player.color = Vec3::ONE;
                            }
                        } else if power_up.r#type == "pass-through" {
                            if !active_types.contains(&"pass-through".to_string()) {
                                ball.pass_through = false;
                                *ball.color() = Vec3::ONE;
                            }
                        } else if power_up.r#type == "confuse" {
                            if !active_types.contains(&"confuse".to_string()) {
                                effects.confuse = false;
                            }
                        } else if power_up.r#type == "chaos" {
                            if !active_types.contains(&"chaos".to_string()) {
                                effects.chaos = false;
                            }
                        }
                    }
                }
            }

            self.power_ups.retain(|power_up| !power_up.is_destroyed() || power_up.activated);
        }

        fn spawn_power_ups(&mut self, block: &GameObject) {
            let powerup_speed = self.resources.get_texture("powerup_speed");
            let powerup_sticky = self.resources.get_texture("powerup_sticky");
            let powerup_passthrough = self.resources.get_texture("powerup_passthrough");
            let powerup_increase = self.resources.get_texture("powerup_increase");
            let powerup_confuse = self.resources.get_texture("powerup_confuse");
            let powerup_chaos = self.resources.get_texture("powerup_chaos");

            if should_spawn(75) {
                self.power_ups
                    .push(PowerUp::new("speed".to_string(), Vec3::new(0.5, 0.5, 1.0), 0.0, block.position, powerup_speed));
            }
            if should_spawn(75) {
                self.power_ups
                    .push(PowerUp::new("sticky".to_string(), Vec3::new(1.0, 0.5, 1.0), 20.0, block.position, powerup_sticky));
            }
            if should_spawn(75) {
                self.power_ups.push(PowerUp::new(
                    "pass-through".to_string(),
                    Vec3::new(0.5, 1.0, 0.5),
                    10.0,
                    block.position,
                    powerup_passthrough,
                ));
            }
            if should_spawn(75) {
                self.power_ups.push(PowerUp::new(
                    "pad-size-increase".to_string(),
                    Vec3::new(1.0, 0.6, 0.4),
                    0.0,
                    block.position,
                    powerup_increase,
                ));
            }
            if should_spawn(15) {
                self.power_ups
                    .push(PowerUp::new("confuse".to_string(), Vec3::new(1.0, 0.3, 0.3), 15.0, block.position, powerup_confuse));
            }
            if should_spawn(15) {
                self.power_ups
                    .push(PowerUp::new("chaos".to_string(), Vec3::new(0.9, 0.25, 0.25), 15.0, block.position, powerup_chaos));
            }
        }

        pub fn activate_power_up(&mut self, power_up: &PowerUp) {
            let ex = self.ex.as_mut().unwrap();
            let player = &mut ex.player;
            let ball = &mut ex.ball;
            let effects = &mut ex.effects;

            if power_up.r#type == "speed" {
                *ball.velocity() *= 1.2;
            } else if power_up.r#type == "sticky" {
                ball.sticky = true;
                player.color = Vec3::new(1.0, 0.5, 1.0);
            } else if power_up.r#type == "pass-through" {
                ball.pass_through = true;
                *ball.color() = Vec3::new(1.0, 0.5, 0.5);
            } else if power_up.r#type == "pad-size-increase" {
                player.size.x += 50.0;
            } else if power_up.r#type == "confuse" {
                if !effects.chaos {
                    effects.confuse = true;
                }
            } else if power_up.r#type == "chaos" {
                if !effects.confuse {
                    effects.chaos = true;
                }
            }
        }

        pub fn do_collisions(&mut self) {
            let (_width, height) = self.screen_size;

            let mut levels = take(&mut self.levels);
            for r#box in &mut levels[self.level as usize].bricks {
                if !r#box.destroyed {
                    let collision = check_collision(&self.ex.as_ref().unwrap().ball, r#box);
                    if collision.0 {
                        if !r#box.is_solid {
                            r#box.destroyed = true;
                            self.spawn_power_ups(r#box);
                            self.ex.as_mut().unwrap().sound_engine.play_2d("resources/audio/bleep.wav", false);
                        } else {
                            self.shake_time = 0.05;
                            self.ex.as_mut().unwrap().effects.shake = true;
                            self.ex.as_mut().unwrap().sound_engine.play_2d("resources/audio/solid.wav", false);
                        }
                        let ball = &mut self.ex.as_mut().unwrap().ball;
                        let dir = collision.1;
                        let diff_vector = collision.2;
                        if !ball.pass_through || r#box.is_solid {
                            match dir {
                                Direction::Left | Direction::Right => {
                                    ball.velocity().x = -ball.velocity().x;
                                    let penetration = ball.radius - f32::abs(diff_vector.x);

                                    match dir {
                                        Direction::Left => ball.position_mut().x += penetration,
                                        _ => ball.position_mut().x -= penetration,
                                    }
                                }
                                _ => {
                                    ball.velocity().y = -ball.velocity().y;
                                    let penetration = ball.radius - f32::abs(diff_vector.y);

                                    match dir {
                                        Direction::Up => ball.position_mut().y -= penetration,
                                        _ => ball.position_mut().y += penetration,
                                    }
                                }
                            }
                        }
                    }
                }
            }
            self.levels = levels;

            let mut power_ups = take(&mut self.power_ups);
            for power_up in &mut power_ups {
                if !*power_up.destroyed() {
                    if power_up.position().y >= height as f32 {
                        *power_up.destroyed() = true;
                    }
                    if check_collision_aabb(&self.ex.as_ref().unwrap().player, power_up.as_go()) {
                        self.activate_power_up(power_up);
                        *power_up.destroyed() = true;
                        power_up.activated = true;
                        self.ex.as_mut().unwrap().sound_engine.play_2d("resources/audio/powerup.wav", false);
                    }
                }
            }
            self.power_ups = power_ups;

            let ex = self.ex.as_mut().unwrap();
            let player = &mut ex.player;
            let ball = &mut ex.ball;

            let result = check_collision(&ball, &player);
            if !ball.stuck && result.0 {
                let center_board = player.position.x + player.size.x / 2.0;
                let distance = (ball.position().x + ball.radius) - center_board;
                let percentage = distance * 2.0 / player.size.x;
                let strength = 2.0;
                let old_velocity = ball.velocity().length();
                ball.velocity().x = INITIAL_BALL_VELOCITY.x * percentage * strength;
                *ball.velocity() = ball.velocity().normalize() * old_velocity;
                ball.velocity().y = -1.0 * f32::abs(ball.velocity().y);

                ball.stuck = ball.sticky;

                ex.sound_engine.play_2d("resources/audio/bleep.wav", false);
            }
        }
    }

    fn should_spawn(chance: u32) -> bool {
        let random = fastrand::u32(..) % chance;
        random == 0
    }

    fn check_collision_aabb(one: &GameObject, two: &GameObject) -> bool {
        // AABB - AABB collision
        let collision_x = one.position.x + one.size.x >= two.position.x && two.position.x + two.size.x >= one.position.x;
        let collision_y = one.position.y + one.size.y >= two.position.y && two.position.y + two.size.y >= one.position.y;
        collision_x && collision_y
    }

    fn check_collision(one: &BallObject, two: &GameObject) -> Collision {
        // AABB - Circle collision
        let center = *one.position() + one.radius;
        let aabb_half_extents = Vec2::new(two.size.x / 2.0, two.size.y / 2.0);
        let aabb_center = Vec2::new(two.position.x + aabb_half_extents.x, two.position.y + aabb_half_extents.y);
        let difference = center - aabb_center;
        let clamped = difference.clamp(-aabb_half_extents, aabb_half_extents);
        let closest = aabb_center + clamped;
        let difference = closest - center;

        if difference.length() < one.radius {
            (true, vector_direction(difference), difference)
        } else {
            (false, Direction::Up, Vec2::ZERO)
        }
    }

    fn vector_direction(target: Vec2) -> Direction {
        let compass = [Vec2::new(0.0, 1.0), Vec2::new(1.0, 0.0), Vec2::new(0.0, -1.0), Vec2::new(-1.0, 0.0)];
        let mut max = 0.0;
        let mut best_match = 2;
        for i in 0..compass.len() {
            let dot_product = target.normalize_or_zero().dot(compass[i]);
            if dot_product > max {
                max = dot_product;
                best_match = i as i32;
            }
        }

        best_match.try_into().unwrap()
    }
}
