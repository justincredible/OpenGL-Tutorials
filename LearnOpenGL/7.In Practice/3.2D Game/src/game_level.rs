pub mod game_level {
    use crate::{GameObject, ResourceManager, SpriteRenderer};
    use glam::{Vec2, Vec3};
    use std::{
        fs::File,
        io::{BufRead, BufReader},
    };

    pub struct GameLevel {
        pub bricks: Vec<GameObject>,
    }

    impl GameLevel {
        pub fn new() -> Self {
            GameLevel { bricks: Vec::new() }
        }

        pub fn load(&mut self, path: &str, level_width: u32, level_height: u32, resources: &ResourceManager) {
            self.bricks.clear();
            let reader = BufReader::new(File::open(path).unwrap());
            let mut tile_data = Vec::new();

            for line in reader.lines() {
                let mut row = Vec::new();
                for tile_code in line.unwrap().split(' ') {
                    row.push(tile_code.parse::<u32>().unwrap());
                }
                tile_data.push(row);
            }
            if tile_data.len() > 0 {
                self.init(tile_data, level_width, level_height, resources);
            }
        }

        pub fn draw(&self, renderer: &SpriteRenderer) {
            for tile in &self.bricks {
                if !tile.destroyed {
                    tile.draw(renderer);
                }
            }
        }

        pub fn is_completed(&self) -> bool {
            for tile in &self.bricks {
                if !tile.is_solid && !tile.destroyed {
                    return false;
                }
            }
            true
        }

        fn init(&mut self, tile_data: Vec<Vec<u32>>, level_width: u32, level_height: u32, resources: &ResourceManager) {
            let height = tile_data.len();
            let width = tile_data[0].len();
            let unit_width = level_width as f32 / width as f32;
            let unit_height = level_height as f32 / height as f32;

            for y in 0..height {
                for x in 0..width {
                    let pos = Vec2::new(unit_width * x as f32, unit_height * y as f32);
                    let size = Vec2::new(unit_width, unit_height);

                    if tile_data[y][x] == 1 {
                        let block_solid = resources.get_texture("block_solid");
                        let mut obj = GameObject::new_with(pos, size, block_solid, Vec3::new(0.8, 0.8, 0.7), Vec2::ZERO);
                        obj.is_solid = true;
                        self.bricks.push(obj);
                    } else if tile_data[y][x] > 1 {
                        let color = match tile_data[y][x] {
                            2 => Vec3::new(0.2, 0.6, 1.0),
                            3 => Vec3::new(0.0, 0.7, 0.0),
                            4 => Vec3::new(0.8, 0.8, 0.4),
                            5 => Vec3::new(1.0, 0.5, 0.0),
                            _ => Vec3::ONE,
                        };

                        let block = resources.get_texture("block");
                        self.bricks.push(GameObject::new_with(pos, size, block, color, Vec2::ZERO));
                    }
                }
            }
        }
    }
}
