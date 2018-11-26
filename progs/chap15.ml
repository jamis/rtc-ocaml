open RTCComposition

let solid r g b = RTCPattern.Solid (RTCColor.build r g b)
let color = RTCColor.build

let tri = RTCTriangle.build
let p = RTCTuple.point

let make_dodecahedron () =
  let s = 0.5 and ns = -0.5 in
  let l = 0.809016995 and nl = -0.809016995 in
  let pi2 = Float.pi /. 2. in
  let tent () =
    let t1 = tri (p ns s 0.) (p nl 0. nl) (p s  s  0.) in
    let t2 = tri (p  s s 0.) (p  l 0. nl) (p nl 0. nl) in
    let t3 = tri (p ns s 0.) (p nl 0.  l) (p s  s  0.) in
    let t4 = tri (p s  s 0.) (p l  0.  l) (p nl 0. l ) in
    let t5 = tri (p ns s 0.) (p nl 0.  l) (p nl 0. nl) in
    let t6 = tri (p s  s 0.) (p l  0.  l) (p l  0. nl) in
    RTCGroup.build ~children:[t1; t2; t3; t4; t5; t6] ()
  in
  let c1 =
    let shape = tent ()
    and transform = RTCTransform.translation 0. l 0. in
    RTCShape.transform shape transform
  and c2 =
    let shape = tent ()
    and transform = compose [ Translate (0., l, 0.); Scale (1., -1., 1.) ] in
    RTCShape.transform shape transform
  and c3 =
    let shape = tent ()
    and transform = compose [ RotateY pi2; Translate (0., l, 0.); RotateX pi2 ] in
    RTCShape.transform shape transform
  and c4 =
    let shape = tent ()
    and transform = compose [ RotateY pi2; Translate (0., l, 0.); RotateX (-.pi2) ] in
    RTCShape.transform shape transform
  and c5 =
    let shape = tent ()
    and transform = compose [ RotateY pi2; Translate (0., l, 0.); RotateZ pi2 ] in
    RTCShape.transform shape transform
  and c6 =
    let shape = tent ()
    and transform = compose [ RotateY pi2; Translate (0., l, 0.); RotateZ (-.pi2) ] in
    RTCShape.transform shape transform
  in
  let shape = RTCGroup.build ~children:[c1; c2; c3; c4; c5; c6] () in
  let transform = RTCTransform.rotation_x 0.523598775598299 in
  RTCShape.transform shape transform

let make_pyramid3 () =
  let ns = -0.57735 and l = 1.1547 in
  let t1 = tri (p 0. 1. 0.) (p (-1.) 0. ns) (p 1. 0. ns) in
  let t2 = tri (p 0. 1. 0.) (p 0. 0. l) (p (-1.) 0. ns) in
  let t3 = tri (p 0. 1. 0.) (p 0. 0. l) (p 1. 0. ns) in
  let t4 = tri (p 0. 0. l) (p 1. 0. ns) (p (-1.) 0. ns) in
  RTCGroup.build ~children:[t1; t2; t3; t4] ()

let make_pyramid4 () =
  let p1 = p 0. 1. 0.
  and p2 = p (-1.) 0. (-1.) and p3 = p 1. 0. (-1.)
  and p4 = p 1. 0. 1. and p5 = p (-1.) 0. 1. in
  let t1 = tri p1 p2 p3 and t2 = tri p1 p3 p4 and t3 = tri p1 p4 p5
  and t4 = tri p1 p5 p2 and t5 = tri p2 p3 p4 and t6 = tri p2 p4 p5 in
  RTCGroup.build ~children:[t1; t2; t3; t4; t5; t6] ()

let geometrical_scene () =
  let room =
    let cube = RTCCube.build () in
    let transform = compose [ Translate (0., 1., 0.); UScale 5. ] in
    let material =
      let pattern =
        let checkers = RTCPattern.checkers (solid 1. 1. 1.) (solid 0.9 0.9 0.9) in
        let transform = RTCTransform.scaling 0.05 0.05 0.05 in
        RTCPattern.transform checkers transform
      in
      RTCMaterial.build ~pattern:(Some pattern) ~ambient:0.1 ~diffuse:0.7 ~reflective:0.05 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in
  let d12 =
    let shape = make_dodecahedron () in
    let transform = RTCMatrix.mult shape.transform (compose [ Translate (0., 1., 0.); UScale 0.6 ]) in
    let material = RTCMaterial.build ~color:(color 1. 0.5 0.5) ~diffuse:0.9 ~ambient:0.1 ~specular:0. () in
    RTCShape.transform (RTCShape.texture shape material) transform
  in
  let p4 =
    let shape = make_pyramid4 () in
    let material = RTCMaterial.build ~color:(color 1. 1. 0.2) ~diffuse:0.6 ~ambient:0.1 () in
    let transform = compose [ RotateY 0.3; UScale 0.2; Translate (0., 0.0001, -1.5) ] in
    RTCShape.transform (RTCShape.texture shape material) transform
  in
  let p3 =
    let shape = make_pyramid3 () in
    let material = RTCMaterial.build ~color:(color 0.2 1. 1.) ~diffuse:0.6 ~ambient:0.1 () in
    let transform = compose [ UScale 0.333; Translate (-2., 0.0001, 0.) ] in
    RTCShape.transform (RTCShape.texture shape material) transform
  in

  let light = RTCLight.point (p (-2.) 5. (-2.)) (color 1. 1. 1.) in

  let world = RTCWorld.build ~shapes:[room; d12; p4; p3] ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 3. 2. (-4.9) in
    let to_p = RTCTuple.point (-0.5) 0.2 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 0.8 view in

  (world, camera)

let file_scene filename transform material =
  let parser = RTCObjFile.parse_file filename in
  let group = RTCShape.texture (RTCShape.transform (RTCObjFile.to_group parser) transform) material in
  let light = RTCLight.point (p (-10.) 10. (-10.)) (color 1. 1. 1.) in
  let world = RTCWorld.build ~shapes:[group] ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 0. 0. (-5.) in
    let to_p = RTCTuple.point 0. 0. 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 (Float.pi /. 4.) view in

  (world, camera)

let run () =
  (* let (world, camera) = geometrical_scene () in *)
  let transform = compose [ Translate (0., 0., -6.); UScale 0.1; RotateX (-.Float.pi /. 2.) ] in
  let material = RTCMaterial.build ~color:(color 1. 0.3 0.5) ~diffuse:0.8 ~ambient:0.1 ~specular:0.4 ~shininess:40. () in
  let (world, camera) = file_scene "models/teapot-large.obj" transform material in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "15-triangles.ppm" in
  output_string f ppm;
  Printf.printf "wrote `15-triangles.ppm'\n"
