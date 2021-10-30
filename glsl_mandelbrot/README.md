# Yet Another Mandelbrot Renderer

This project was done as a side-side (metaside?) project during [magicpixel](https://github.com/Mulan-Szechuan-Sauce/magicpixel).
  
It uses OpenGL to render the Mandelbrot set via shaders. It loses fidelity at around 1.5e-4 scale.

With an ok graphics card, it should run at an interactive speed. On mine I get around 150 FPS. You can use WASD or click/drag to pan, and scroll to zoom.

### Full rendering

I used linear interpolated historgram coloring from red to green to blue. For a better color distribution I used a decaying palette selection.

![Full Brot](https://github.com/elimirks/miscprojects/blob/master/glsl_mandelbrot/images/fullbrot.png?raw=true)

### Close zoom fidelity loss

Poor fidelity at 1.54e-4 scale

![Fidelity](https://github.com/elimirks/miscprojects/blob/master/glsl_mandelbrot/images/poor_res_1.54eminus4.png?raw=true)

### Other cool photos

![Green everywhere!](https://github.com/elimirks/miscprojects/blob/master/glsl_mandelbrot/images/green_boi.png?raw=true)

Trail of red between minibrots.

![Trail](https://github.com/elimirks/miscprojects/blob/master/glsl_mandelbrot/images/trail.png?raw=true)

More "nested" minibrots.

![Mini "nested" mandelbrots](https://github.com/elimirks/miscprojects/blob/master/glsl_mandelbrot/images/minibrot.png?raw=true)
