pub mod sound_engine {
    use al_sys::*;
    use std::{collections::HashMap, fs::File, io::Read, ptr, rc::Rc};

    pub struct SoundEngine {
        api: Rc<AlApi>,
        device: *mut ALCdevice,
        context: *mut ALCcontext,
        sounds: HashMap<Box<str>, Sound>,
    }

    impl Drop for SoundEngine {
        fn drop(&mut self) {
            unsafe {
                self.api.alcMakeContextCurrent(ptr::null_mut());
                self.api.alcDestroyContext(self.context);
                self.api.alcCloseDevice(self.device);
            }
        }
    }

    impl SoundEngine {
        pub fn new() -> Self {
            let api = Rc::new(AlApi::load_default().unwrap());

            unsafe {
                let device = api.alcOpenDevice(ptr::null_mut());
                let context = api.alcCreateContext(device, ptr::null_mut());
                api.alcMakeContextCurrent(context);

                SoundEngine {
                    api,
                    device,
                    context,
                    sounds: HashMap::new(),
                }
            }
        }

        pub fn play_2d(&mut self, path: &str, looping: bool) {
            unsafe {
                match self.sounds.get(path) {
                    Some(sound) => {
                        let mut state = 0;
                        self.api.alGetSourcei(sound.source, AL_SOURCE_STATE, &mut state);
                        if state == AL_PLAYING {
                            self.api.alSourceRewind(sound.source);
                        }
                        self.api.alSourcePlay(sound.source);
                    }
                    None => {
                        self.sounds.insert(Box::from(path), Sound::new(&self.api, path, looping));
                        let sound = self.sounds.get(path).unwrap();
                        self.api.alSourcePlay(sound.source);
                    }
                }
            }
        }
    }

    pub struct Sound {
        api: Rc<AlApi>,
        source: ALuint,
        buffer: ALuint,
    }

    type Header = (u16, u16, i32, i32);

    impl Drop for Sound {
        fn drop(&mut self) {
            unsafe {
                let mut state = 0;
                self.api.alGetSourcei(self.source, AL_SOURCE_STATE, &mut state);
                if state == AL_PLAYING {
                    self.api.alSourceStop(self.source);
                }
                self.api.alDeleteSources(1, &self.source);
                self.api.alDeleteBuffers(1, &self.buffer);
            }
        }
    }

    impl Sound {
        pub fn new(api: &Rc<AlApi>, path: &str, looping: bool) -> Self {
            let api = Rc::clone(api);

            let mut source = 0;
            let mut buffer = 0;

            unsafe {
                api.alGenSources(1, &mut source);

                api.alSourcef(source, AL_PITCH, 1.0);
                api.alSourcef(source, AL_GAIN, 1.0);
                api.alSourcefv(source, AL_POSITION, [0.0, 0.0, 0.0].as_ptr());
                api.alSourcefv(source, AL_VELOCITY, [0.0, 0.0, 0.0].as_ptr());
                api.alSourcei(source, AL_LOOPING, if looping { AL_TRUE } else { AL_FALSE } as i32);

                api.alGenBuffers(1, &mut buffer);

                let (data, (channels, bps, frequency, size)) = Sound::load_wave(path);
                let format = match (channels, bps) {
                    (1, 8) => AL_FORMAT_MONO8,
                    (1, 16) => AL_FORMAT_MONO16,
                    (2, 8) => AL_FORMAT_STEREO8,
                    (2, 16) => AL_FORMAT_STEREO16,
                    _ => panic!("undefined"),
                };

                api.alBufferData(buffer, format, data.as_ptr().cast(), size, frequency);

                api.alSourcei(source, AL_BUFFER, buffer as i32);
            }

            Sound { api, source, buffer }
        }

        fn load_wave(path: &str) -> (Vec<u8>, Header) {
            let mut bytes = Vec::new();
            let size = match File::open(path) {
                Ok(mut file) => file.read_to_end(&mut bytes).unwrap(),
                Err(error) => {
                    println!("{}", error);
                    panic!("{}", path);
                }
            };

            if size < 44 {
                panic!("Incorrect file format for WAV.");
            }

            let (header, sample) = bytes.split_at(44);

            (Vec::from(sample), Sound::parse_header(header))
        }

        fn parse_header(header: &[u8]) -> Header {
            let mut chan = 0;
            for b in header[22..24].into_iter().rev() {
                chan *= 256;
                chan += *b as u16;
            }
            let mut rate = 0;
            for b in header[24..28].into_iter().rev() {
                rate *= 256;
                rate += *b as i32;
            }
            let mut width = 0;
            for b in header[34..36].into_iter().rev() {
                width *= 256;
                width += *b as u16;
            }
            let mut size = 0;
            for b in header[40..44].into_iter().rev() {
                size *= 256;
                size += *b as i32;
            }

            (chan, width, rate, size)
        }
    }
}
