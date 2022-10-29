# Rastertek OpenGL
- The tutorial series for OpenGL at [RasterTek](http://rastertek.com/tutgl40.html) written in Haskell.
- The tutorial series for DirectX 11 at [RasterTek](https://rastertek.com/tutdx11.html) written in OpenGL and Haskell.

## Windowing

Windowing uses GLFW based off these [Haskell OpenGL tutorials](https://github.com/madjestic/Haskell-OpenGL-Tutorial).

## Ti

Ti folders use Graphics.Rendering.OpenGL like the windowing tutorials.

## Ri

Ri folder use the raw OpenGL bindings from Graphics.GL.

## Sound

OpenAL worked nicely with the tutorial from [IndieGameDev](https://indiegamedev.net/2020/02/15/the-complete-guide-to-openal-with-c-part-1-playing-a-sound/).

## Tessellation

The guest article from [LearnOpenGL](https://learnopengl.com/Guest-Articles/2021/Tessellation/Tessellation) provided familiarity with the control and evaluation shaders.

## CPU usage

The implementation using the PDH library seems simple enough, but I had great difficulty trying to create bindings to it. I may revisit this at a later time, but if I was proficient with the FFI I would be using DirectX on Windows.
