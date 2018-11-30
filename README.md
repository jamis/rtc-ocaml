# OCaml Ray Tracer

This is an implementation of a basic recursive (Whitted) ray tracer,
in OCaml, following my book, "The Ray Tracer Challenge". It implements the following primitives:

* Spheres
* Planes
* Cubes
* Cylinders
* Cones
* Triangles

as well as CSG operations on all of the above. Shadows, reflections, and transmission with refraction are all implemented, as are a small number of solid textures (stripes, rings, gradients, and checkers).

As this was my first ever program in OCaml, the serious OCaml student will (no doubt!) find many questionable things herein. I'd love to know how it might be better implemented, so please let me know if you'd have tackled something differently.


## Building the ray tracer

Assuming you have OCaml installed, as well as the oUnit library for unit testing, you should be able to simply invoke `make`. This will produce two deliverables: `tests.native` and `chapter.native`. The former will run the test suite for the ray tracer, and the latter will (if invoked with a chapter number) produce a deliverable for the corresponding chapter.

~~~
$ ./chapter.native 16
wrote `16-csg.ppm'
$
~~~


If you like the ray tracer, please consider buying my book! It walks you through writing an entire ray tracer, test-first, from scratch, in whichever programming language and environment you prefer. (I've personally used it to write ray tracers in Ruby, C, and now OCaml.)


## Reading the ray tracer

If all you want to do is browse the code, start by looking at `progs/chapter.ml`. This is the entry point for all of the programs I wrote to demo the features of each chapter. Chapter 7 is the first program with the "World" abstraction, but I don't think I started to hit my stride with OCaml until chapter 11 ("reflections"). Your opinions of my grasp of OCaml idom may well differ from my own. :)

In general, a scene is rendered in the following steps:

### 1. Create some shapes, with textures and transformations.

~~~ocaml
let room =
  let cube = RTCCube.build() in
  let tx = compose [ Translate (0., 1., 0.); UScale 5. ] in
  let material =
    let pattern =
      let c1 = solid 1. 1. 1. and c2 = solid 0.9 0.9 0.9 in
      let tx  = compose [ UScale 0.05 ] in
      RTCPattern.transform (RTCPattern.checkers c1 c2) tx
    in
    RTCMaterial.build ~pattern:(Some pattern) ~ambient:0.1 ~diffuse:0.7 ~reflective:0.05 ()
  in
  RTCShape.transform (RTCShape.texture cube material) tx
in
...
~~~

### 2. Create at least one light source.

This implementation supports multiple light sources, but you only really need
one per scene. (I mean, you could do without any lights at all, but why?)

~~~ocaml
let light = RTCLight.point (RTCTuple.point (-2.) 5. (-2.))
                           (RTCColor.build 1. 1. 1.)
in
...
~~~

### 3. Create a world with the shapes and lights you created.

~~~ocaml
let world = RTCWorld.build ~shapes:[shape1; shape2; etc]
                           ~lights:[light1; light2; etc]
                           ()
in
...
~~~

### 4. Construct a view transformation to orient the world.

~~~ocaml
let view =
  let from_p = RTCTuple.point 0. 2. (-4.9) in
  let to_p = RTCTuple.point 0. 0.5 0. in
  let up_v = RTCTuple.vector 0. 1. 0. in
  RTCTransform.view from_p to_p up_v
in
...
~~~

### 5. Instantiate a camera.

The camera is the data structure that defines the image that will be generated.
It takes a width and a height (for the resulting canvas), a field of view
(in radians), and a view transformation.

~~~ocaml
let camera = RTCCamera.build 400 200 0.9 view in
...
~~~

### 6. Render the scene to a canvas.

~~~ocaml
let image = RTCCamera.render camera world in
...
~~~

### 7. Save the canvas to a file in PPM format.

~~~ocaml
let ppm = RTCCanvas.to_ppm image in
let f = open_out "scene.ppm" in
output_string f ppm
~~~

So, yeah. Not exactly "1, 2, 3", but not terrible, either!


## License

This ray tracer is provided under the terms of the MIT license, by Jamis Buck <jamis@jamisbuck.org>.
