# Rastertek OpenGL

Ri: The tutorial series for DirectX 11 at [RasterTek](https://rastertek.com/tutdx11.html) written in OpenGL and Haskell.
Ti: The tutorial series for OpenGL at [RasterTek](https://rastertek.com/tutgl40.html) written in Haskell.

These tutorials can be run via [Stack](https://docs.haskellstack.org/).

Tutorials 14 and 31 require the DLL contained in the [OpenAL](https://www.openal.org/downloads/) install directory. Rename the DLL libopenal-1.dll, and copy it to the Stack project root. Finally, in a shell run `stack exec -- pacman -S mingw-w64-x86_64-openal`. If you want to skip those tutorials, remove the openal-ffi dependencies below.

Add the following to package.yaml dependencies:
- OpenGL # this is for the Ti tutorials
- OpenGLRaw
- GLFW-b
- bytestring
- directory
- linear
- random
- openal-ffi
- Win32

Add to stack.yaml extra-deps
- openal-ffi-0.0.1

Copy all the files for a tutorial into the src folder. Move Main.hs to the app folder, and move the asset and glsl folders to the project root. Now you can `stack run`.

# Credits

## Windowing

Windowing uses GLFW based off these [Haskell OpenGL tutorials](https://github.com/madjestic/Haskell-OpenGL-Tutorial).

## Sound

OpenAL worked nicely with the tutorial from [IndieGameDev](https://indiegamedev.net/2020/02/15/the-complete-guide-to-openal-with-c-part-1-playing-a-sound/).

## Tessellation

The guest article from [LearnOpenGL](https://learnopengl.com/Guest-Articles/2021/Tessellation/Tessellation) provided familiarity with the control and evaluation shaders.

# Known issues

## CPU usage

The implementation using the PDH library seems simple enough, but I had great difficulty trying to create bindings to it. I may revisit this at a later time.

## Frustum Culling

Culling can happen to visible spheres in surprising ways. Something went amiss in translation.
