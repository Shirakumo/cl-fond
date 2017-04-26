#|
 This file is a part of cl-fond
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fond.cffi)

;; low-level.lisp
(docs:define-docs
  (variable *here*
    "Variable containing the path to the low-level.lisp file.")
  
  (variable *static*
    "Variable containing the path to the static directory.
That directory contains the precompiled library binaries.")
  
  (type font
    "This is the primary struct that contains all relevant information about a font.

You must allocate this struct yourself and make sure that
it is zeroed out before you do anything with it. Not
zeroing out will land you in a world of pain.

See FONT-FILE
See FONT-INDEX
See FONT-SIZE
See FONT-CHARACTERS
See FONT-CODEPOINTS
See FONT-WIDTH
See FONT-HEIGHT
See FONT-OVERSAMPLE
See FONT-ATLAS
See FONT-FONTDATA
See FONT-CHARDATA
See FONT-FONTINFO
See FONT-CONVERTED-CODEPOINTS
See LOAD-FONT
See LOAD-FONT-FIT
See FREE-FONT
See COMPUTE-TEXT
See COMPUTE-TEXT-U
See COMPUTE-EXTENT
See COMPUTE-EXTENT-U")

  (function font-file
    "Accessor for the path to the TTF file.

See FONT")

  (function font-index
    "Accessor for the index of the font within the TTF file.

You probably don't need to set this.

See FONT")

  (function font-size
    "The vertical font size in pixels.

If you render it above this resolution, you'll get blurring.

See FONT")

  (function font-characters
    "A UTF8 encoded string of characters that this font instance will be able to render.

Must be null-terminated.

See FONT")

  (function font-codepoints
    "An array of Unicode codepoints that this font instance will be able to render.

Must be null-terminated. This is automatically filled in for
you if it is NULL and the characters field is provided instead.

See FONT")

  (function font-width
    "The width of the glyph texture atlas.

See FONT")

  (function font-height
    "The height of the glyph texture atlas.

See FONT")

  (function font-oversample
    "How much oversampling should be done.

Higher oversampling might improve the quality of the rendering,
but will need a much bigger atlas size:
  width*oversampling,height*oversampling

See FONT")

  (function font-atlas
    "The OpenGL texture ID for the atlas.

See FONT")

  (function font-fontdata
    "This is an internal field.

See FONT")

  (function font-chardata
    "This is an internal field.

See FONT")

  (function font-fontinfo
    "This is an internal field.

See FONT")

  (function font-converted-codepoints
    "This is an internal field.

See FONT")

  (type buffer
    "This struct allows for convenience in rendering, as it will render text for you into a texture.

You must allocate this struct yourself and make sure that
it is zeroed out before you do anything with it. Not
zeroing out will land you in a world of pain.

See BUFFER-FONT
See BUFFER-TEXTURE
See BUFFER-WIDTH
See BUFFER-HEIGHT
See BUFFER-PROGRAM
See BUFFER-FRAMEBUFFER
See FREE-BUFFER
See LOAD-BUFFER
See RENDER-BUFFER
See RENDER-BUFFER-U")

  (function buffer-font
    "Pointer to the font that it renders.

This can be exchanged for another font, if you should
decide to want to render with a different font.

See BUFFER")

  (function buffer-texture
    "The OpenGL texture ID to which this buffer renders to.

See BUFFER")

  (function buffer-width
    "The width of the texture.

See BUFFER")

  (function buffer-height
    "The height of the texture.

See BUFFER")

  (function buffer-program
    "This is an internal field.

See BUFFER")

  (function buffer-framebuffer
    "This is an internal field.

See BUFFER")

  (type extent
    "This struct contains information about the extents of a text.

You must allocate this struct yourself and make sure that
it is zeroed out before you do anything with it. Not
zeroing out will land you in a world of pain.

See EXTENT-L
See EXTENT-R
See EXTENT-T
See EXTENT-B
See EXTENT-GAP
See COMPUTE-EXTENT
See COMPUTE-EXTENT-U")

  (function extent-l
    "How far to the left the text extends from zero.

See EXTENT")

  (function extent-r
    "How far to the right the text extends from zero.

See EXTENT")

  (function extent-t
    "How far up the text extends from its baseline.

See EXTENT")

  (function extent-b
    "How far down the text extends from its baseline.

See EXTENT")

  (function extent-gap
    "The gap between lines of the text.

See EXTENT")

  (function free-font
    "Free all the data that was allocated into the struct by LOAD-FONT*

This will /not/ free the characters array, the file
array, or the codepoints array if the codepoints
array was not computed by LOAD-FONT*

See FONT")

  (function load-font
    "Load the font struct and allocate the necessary OpenGL data.

The following fields must be set in the struct:
  file
  size
  width
  height
  characters or codepoints

See FONT")

  (function load-font-fit
    "Load the font struct, attempting to fit an atlas automatically.

This may not result in the most compact atlas possible.
max_size is the maximum size of the width or height
that can be reached before it gives up.

The following fields must be set in the struct:
  file
  size
  characters or codepoints

See FONT")

  (function compute-text
    "Compute the Vertex Array Object to render the given text.

Here, n and vao are output arguments, containing the
number of elements and the OpenGL VAO ID respectively.
The text must be UTF8 encoded and null-terminated. The
returned VAO contains two arrays, at location 0 and 1,
with both being vec2s. The first being the vertex
coordinates and the second being the texture
coordinates. The vertex coordinates start at 0 and
increase in x and y as per the font's size. You are
responsible for scaling it as appropriate for your
display. The texture coordinates are for the font's
atlas texture. If the text contains a Linefeed
character (U+000A) a new line is started automatically
by resetting X to 0 and decreasing Y by the necessary
height for a new line.

See FONT")

  (function compute-text-u
    "Same as COMPUTE-EXTENT but taking an UTF32 encoded string and its size.

See COMPUTE-TEXT
See FONT")

  (function compute-extent
    "Compute the extent of the given text.

You must allocate the extent struct yourself and make
sure it is zeroed out.

See FONT
See EXTENT")

  (function compute-extent-u
    "Same as COMPUTE-EXTENT but taking an UTF32 encoded string and its size.

See COMPUTE-EXTENT
See FONT
See EXTENT")

  (function free-buffer
    "Free all the data that was allocated into the struct by fond_load_buffer.

This will /not/ free the font struct.

See BUFFER")

  (function load-buffer
    "Load the buffer struct and allocate the necessary OpenGL data.

The following fields must be set in the struct:
  font
  width
  height

See BUFFER")

  (function render-buffer
    "Render the given text to the buffer's texture.

The text will be rendered at the given offset, with x
and y being in pixels. color can be either 0 for white
text, or an array of four floats, representing RGBA
of the text's colour in that order.

See BUFFER")

  (function render-buffer-u
    "Same as RENDER-BUFFER but taking an UTF32 encoded string and its size.

See RENDER-BUFFER
See BUFFER")

  (function decode-utf8
    "Decode the given UTF8 string into an UTF32 string.

The resulting string is put into decoded, and its size is
put into size. This is used by the non _u functions to
decode the string. You may want to use this internally,
if you need to re-use the same string often and don't wnat
to pay the conversion cost.")

  (function fond-error
    "Return the current error code.")

  (function fond-error-string
    "Return a string for a human-readable error message of the given error code."))

(in-package #:org.shirakumo.fraf.fond)

;; wrapper.lisp
(docs:define-docs
  (type fond-error
    "Condition signalled when an error occurs in the underlying libfond.

See ERROR-CODE")

  (function error-code
    "The error code that the condition carries.

See FOND-ERROR
See CL-FOND-CFFI:FOND-ERROR-STRING")

  (function show-error
    "Check whether any errors occurred in the underlying library and signal an error if necessary.

See FOND-ERROR
See CL-FOND-CFFI:FOND-ERROR")

  (function calloc
    "Allocate memory for the given foreign type and zero it out.")

  (function string->char*
    "Convert the given string to an UTF8 encoded C-string.

Returns a pointer to the allocated data. You are responsible
for freeing it when appropriate.")

  (type c-object
    "Base class for all objects that are tied to a foreign object somehow.

See HANDLE
See ALLOCATE-HANDLE
See FREE-HANDLE
See FREE")

  (function handle
    "Accessor to the pointer to the foreign object.

See C-OBJECT")

  (function allocate-handle
    "Allocate and return a pointer to the foreign object for this class.

See C-OBJECT")

  (function free-handle
    "Return a function that, when called, frees the given foreign object for this class.

See C-OBJECT")

  (function free
    "Immediately frees the foreign object associated with the instance.")

  (type font
    "This is a representation of a font file.

Using this class you can draw and compute text for a specific
font at a specific size. The library will take care of rendering
the font glyphs to an atlas to allow fast rendering of text.

See C-OBJECT
See MAKE-FONT
See COMPUTE-TEXT
See COMPUTE-EXTENT
See FILE
See SIZE
See TEXT-HEIGHT
See WIDTH
See HEIGHT
See TEXTURE
See CHARSET")

  (function make-font
    "Create a new font instance for the given TTF file and character set.

The charset should be a string containing all characters that
the font should be able to draw. The index is the index of the
font within the file. Some TTF files can contain multiple fonts
in one. Usually, you won't need to set this parameter. Oversample
is the oversampling factor that should be used. Using
oversampling might result in better quality rendering, at the
cost of massively increasing the necessary size of the atlas.
The necessary size is multiplied by the oversampling factor in
both dimensions. The width and height are optional factors that
when given specify the starting texture atlas size to use to
attempt to fit the glyphs. If this size is too small, an error
is signalled.

See FONT")

  (function compute-text
    "Compute the vertex array object to draw the given text.

Returns two values: the OpenGL VAO ID, and the number of
elements stored in the VAO.

In order to actually use this, you will need to bind the
font's texture and use a vertex and fragment shader that
draw the VAO as appropriate. The VAO contains two arrays
of vec2s for this purpose: the first being the vertex
coordinates, and the second being the texture coordinates.
The texture only contains a red component, with the other
components being set to zero.

See FONT")

  (function compute-extent
    "Compute the extent of the given text in pixels.

Returns a property list with the following entries:
:L    --- The extent to the left from zero.
:R    --- The extent to the right from zero.
:T    --- The extent up from the baseline.
:B    --- The extent down from the baseline.
:GAP  --- The gap between two lines of text.

See FONT")

  (function file
    "Returns the pathname for the TTF file this font contains.

See FONT")

  (function size
    "Returns the font's size in pixels.

See FONT")

  (function text-height
    "Returns the height of a text line from its baseline.

See FONT")

  (function width
    "Returns the width of the object's texture.

See TEXTURE
See FONT
See BUFFER")

  (function height
    "Returns the height of the object's texture.

See TEXTURE
See FONT
See BUFFER")

  (function texture
    "Returns the OpenGL texture ID for the texture contained in the object.

See WIDTH
See HEIGHT
See FONT
See BUFFER")

  (function charset
    "Returns a string containing all the characters this font instance is capable of drawing.

See FONT")

  (type buffer
    "This is a representation of a text buffer.

The buffer allows you to conveniently draw text to a texture.
You will then merely need to draw the texture to the screen,
rather than needing to manually create a shader and handle the
drawing yourself.

See C-OBJECT
See FONT
See MAKE-BUFFER
See RENDER
See WIDTH
See HEIGHT
See TEXTURE")

  (function font
    "Accessor to the font that this buffer uses to render with.

You can change this at any time to switch what to render with.")

  (function make-buffer
    "Create a new buffer instance.

The font is the font instance to render with, and the width
and height specify the dimensions of the texture that this
buffer renders to.

See BUFFER
See FONT")

  (function render
    "Render the given text to the buffer.

The X and Y coordinates are offsets inside the texture,
counting from the top left, as is usual for graphical
applications. Thus Y grows downwards, rather than upwards
like in OpenGL. The color should be a list of floats in
the range of [0,1] representing RGBA in that order, 
defaulting to 1 for each component.

Returns the OpenGL texture ID that we rendered to."))
