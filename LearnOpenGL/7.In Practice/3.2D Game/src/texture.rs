pub mod texture {
    use std::ffi::c_void;

    pub struct Texture {
        pub id: u32,
        screen_size: (i32, i32),
        pub internal_format: i32,
        pub image_format: u32,
        wrap: (i32, i32),
        filter: (i32, i32),
    }

    impl Drop for Texture {
        fn drop(&mut self) {
            unsafe {
                gl::DeleteTextures(1, &self.id);
            }
        }
    }

    impl Texture {
        pub fn new() -> Self {
            let mut id = 0;

            unsafe {
                gl::GenTextures(1, &mut id);
            }

            Texture {
                id,
                screen_size: (0, 0),
                internal_format: gl::RGB as i32,
                image_format: gl::RGB,
                wrap: (gl::REPEAT as i32, gl::REPEAT as i32),
                filter: (gl::LINEAR as i32, gl::LINEAR as i32),
            }
        }

        pub fn generate(&mut self, width: i32, height: i32, data: *const c_void) {
            self.screen_size = (width, height);

            unsafe {
                gl::BindTexture(gl::TEXTURE_2D, self.id);
                gl::TexImage2D(gl::TEXTURE_2D, 0, self.internal_format, width, height, 0, self.image_format, gl::UNSIGNED_BYTE, data);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, self.wrap.0);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, self.wrap.1);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, self.filter.0);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, self.filter.1);
                gl::BindTexture(gl::TEXTURE_2D, 0);
            }
        }

        pub fn bind(&self) {
            unsafe {
                gl::BindTexture(gl::TEXTURE_2D, self.id);
            }
        }
    }
}
