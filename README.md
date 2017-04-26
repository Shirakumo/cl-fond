## About cl-fond
This is a bindings library to [libfond](https://github.com/Shirakumo/libfond), a simple OpenGL text rendering engine.

## How To
Precompiled versions of the underlying library are included in this. If you want to build it manually however, refer to the [libfond](https://github.com/Shirakumo/libfond) repository.

Load the system through ASDF or Quicklisp:

    (ql:quickload :cl-fond)

Before you can create any of the objects, you need to have an OpenGL 3.3+ context current. Creating a context is outside of the scope of this library. Please refer to your framework. Once you do have a context handy, you can create a font object:

    (defvar *font* (cl-fond:make-font #p"~/test.ttf" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?"))

The string there specifies the character set for which the font should be loaded. You will only be able to render characters from that set. Note that the more characters you have and the bigger you make the font size, the larger the texture it will have to allocate to map out the characters. If there is an error of some kind, like being unable to read the file or fit it into a texture atlas, a condition is signalled. Otherwise the font should be readily loaded and you can start working with it.

    (compute-extent *font* "Hello there!")

If you actually want to render text, you are given two options:

1. Using the readily provided buffer class
2. Rendering the text manually

The former has the advantage of being much simpler, at the cost of rendering to an offscreen texture instead of directly to the screen. The latter gives you more control over how you place and render the text at the cost of having to write your own shaders to do it. Let's look at the easy way first:

    (defvar *buffer* (cl-fond:make-buffer *font* 512 30))

With a default font size of 20px, 30px for the height should be enough room. This function may again error if it fails to properly allocate the resources it needs in the back. Once it is ready you can start rendering text to it.

    (cl-fond:render *buffer* "Sup man." :y (height *font*))

We set the `y` coordinate to the height of the font here, as libfond sets `0,0` in the top left corner and `x,y` of the text on the left of its baseline. Thus in order to make the text visible on the buffer, we need to offset the y coordinate by at least the font's height. Once rendered, it returns the OpenGL texture that it rendered to. You can then render this texture onto a quad in your primary render code. Keep in mind that you need to respect the aspect ratio of the buffer, and that scaling its texture beyond the specified size will make things look blurry.

If you want to manually render the characters instead, you'll want to use `compute-text` as follows:

    (cl-fond:compute-text *font* "Heyo.")

Returned by it will be an OpenGL vertex array to draw with and the number of elements it contains. After binding your shader program and setting things up properly, you can then draw the characters as follows:

    (gl:bind-texture :texture-2d (cl-fond:atlas *font*))
    (gl:bind-vertex-array vao)
    (%gl:draw-elements :triangles count :unsigned-int 0)

The VAO contains two `vec2` inputs: the position and the texture coordinate. Note that the position is scaled to pixel size. You will likely want to adapt this in your vertex shader to fit appropriately. In your fragment shader you will only want to read out the `r` component of the texture. Every other component is 0.

See the default [vertex](src/shader/to_texture.vert) and [fragment](src/shader/to_texture.frag) shaders used by the buffer for an example.
