pub mod resource_manager {
    use crate::{Shader, Texture};
    use stb_image::stb_image::bindgen;
    use std::{collections::HashMap, fs::File, io::Read, rc::Rc};

    pub struct ResourceManager {
        textures: HashMap<Box<str>, Rc<Texture>>,
    }

    impl ResourceManager {
        pub fn new() -> Self {
            ResourceManager { textures: HashMap::new() }
        }

        pub fn load_shader(vs_file: &str, fs_file: &str) -> Shader {
            let mut vertex_source = Vec::new();
            File::open(vs_file).unwrap().read_to_end(&mut vertex_source).unwrap();
            vertex_source.push(0);

            let mut fragment_source = Vec::new();
            File::open(fs_file).unwrap().read_to_end(&mut fragment_source).unwrap();
            fragment_source.push(0);

            Shader::compile(vertex_source, fragment_source)
        }

        pub fn load_texture(&mut self, path: &str, alpha: bool, name: &str) -> &Rc<Texture> {
            self.textures.insert(Box::from(name), Rc::new(ResourceManager::load_texture_from_file(path, alpha)));
            self.textures.get(name).unwrap()
        }

        pub fn get_texture(&self, name: &str) -> Rc<Texture> {
            Rc::clone(&self.textures[name])
        }

        fn load_texture_from_file(path: &str, alpha: bool) -> Texture {
            let mut texture = Texture::new();
            if alpha {
                texture.internal_format = gl::RGBA as i32;
                texture.image_format = gl::RGBA;
            }
            let mut file = File::open(path).unwrap();
            let mut contents = vec![];

            file.read_to_end(&mut contents).unwrap();

            let mut width = 0;
            let mut height = 0;
            let mut components = 0;
            unsafe {
                let data = bindgen::stbi_load_from_memory(contents.as_mut_ptr(), contents.len() as i32, &mut width, &mut height, &mut components, 0);
                texture.generate(width, height, data.cast());
                bindgen::stbi_image_free(data.cast());
            }
            texture
        }
    }
}
