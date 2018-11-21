open RTCComposition

let solid r g b = RTCPattern.Solid (RTCColor.build r g b)
let color = RTCColor.build

let scene () =
  let floor_ceiling =
    let cube = RTCCube.build () in
    let transform = compose [ Translate (0., 1., 0.); Scale (20., 7., 20.) ] in
    let material =
      let pattern =
        let c1 = solid 0. 0. 0. in
        let c2 = solid 0.25 0.25 0.25 in
        let transform = RTCTransform.scaling 0.07 0.07 0.07 in
        RTCPattern.transform (RTCPattern.checkers c1 c2) transform
      in
      RTCMaterial.build ~pattern:(Some pattern) ~ambient:0.25
                        ~diffuse:0.7 ~specular:0.9 ~shininess:300.
                        ~reflective:0.1 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let walls =
    let cube = RTCCube.build () in
    let transform = RTCTransform.scaling 10. 10. 10. in
    let material =
      let pattern =
        let c1 = solid 0.4863 0.3765 0.2941 in
        let c2 = solid 0.3725 0.2902 0.2275 in
        let transform = RTCTransform.scaling 0.05 20. 0.05 in
        RTCPattern.transform (RTCPattern.checkers c1 c2) transform
      in
      RTCMaterial.build ~pattern:(Some pattern) ~ambient:0.1
                        ~diffuse:0.7 ~specular:0.9 ~shininess:300.
                        ~reflective:0.1 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let table_top =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (3., 0.1, 2.); Translate (0., 3.1, 0.) ] in
    let material =
      let pattern =
        let c1 = solid 0.5529 0.4235 0.3255 in
        let c2 = solid 0.6588 0.5098 0.4000 in
        let transform = compose [ RotateY 0.1; Scale (0.05, 0.05, 0.05) ] in
        RTCPattern.transform (RTCPattern.stripe c1 c2) transform
      in
      RTCMaterial.build ~pattern:(Some pattern) ~ambient:0.1
                        ~diffuse:0.7 ~specular:0.9 ~shininess:300.
                        ~reflective:0.2 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let leg x z =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.1, 1.5, 0.1); Translate (x, 1.5, z) ]in
    let material = RTCMaterial.build ~color:(color 0.5529 0.4235 0.3255)
                                     ~ambient:0.2 ~diffuse:0.7 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let leg1 = leg 2.7 (-1.7) in
  let leg2 = leg 2.7 1.7 in
  let leg3 = leg (-2.7) (-1.7) in
  let leg4 = leg (-2.7) 1.7 in

  let glass_cube =
    let cube = { (RTCCube.build ()) with shadow=false } in
    let transform = compose [ UScale 0.25; RotateY 0.2; Translate (0., 3.450001, 0.) ] in
    let material = RTCMaterial.build ~color:(color 1. 1. 0.8)
                                     ~ambient:0. ~diffuse:0.3
                                     ~specular:0.9 ~shininess:300.
                                     ~reflective:0.7 ~transparency:0.7
                                     ~refractive_index:1.5 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let little_cube1 =
    let cube = RTCCube.build () in
    let transform = compose [ UScale 0.15; RotateY (-0.4); Translate (1., 3.35, -0.9) ] in
    let material = RTCMaterial.build ~color:(color 1. 0.5 0.5)
                                     ~diffuse:0.4 ~reflective:0.6 ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let little_cube2 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.15, 0.07, 0.15); RotateY 0.4; Translate (-1.5, 3.27, 0.3) ] in
    let material = RTCMaterial.build ~color:(color 1. 1. 0.5) () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let little_cube3 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.2, 0.05, 0.05); RotateY 0.4; Translate (0., 3.25, 1.) ] in
    let material = RTCMaterial.build ~color:(color 0.5 1. 0.5) () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let little_cube4 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.05, 0.2, 0.05); RotateY 0.8; Translate (-0.6, 3.4, -1.) ] in
    let material = RTCMaterial.build ~color:(color 0.5 0.5 1.) () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let little_cube5 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.05, 0.2, 0.05); RotateY 0.8; Translate (2., 3.4, 1.) ] in
    let material = RTCMaterial.build ~color:(color 0.5 1. 1.) () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let frame1 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.05, 1., 1.);Translate (-10., 4., 1.) ] in
    let material = RTCMaterial.build ~color:(color 0.7098 0.2471 0.2196) ~diffuse:0.6 () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let frame2 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.05, 0.4, 0.4); Translate (-10., 3.4, 2.7) ] in
    let material = RTCMaterial.build ~color:(color 0.2667 0.2706 0.6902) ~diffuse:0.6 () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let frame3 =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (0.05, 0.4, 0.4);Translate (-10., 4.6, 2.7) ] in
    let material = RTCMaterial.build ~color:(color 0.3098 0.5961 0.3098) ~diffuse:0.6 () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let mirror_frame =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (5., 1.5, 0.05);Translate (-2., 3.5, 9.95) ] in
    let material = RTCMaterial.build ~color:(color 0.3882 0.2627 0.1882) ~diffuse:0.7 () in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let mirror =
    let cube = RTCCube.build () in
    let transform = compose [ Scale (4.8, 1.4, 0.06);Translate (-2., 3.5, 9.95) ] in
    let material = RTCMaterial.build ~color:(color 0. 0. 0.)
                                     ~ambient:0. ~diffuse:0. ~specular:1.
                                     ~shininess:300. ~reflective:1. ()
    in
    RTCShape.transform (RTCShape.texture cube material) transform
  in

  let light = RTCLight.point (RTCTuple.point 0. 6.9 (-5.)) (color 1. 1. 1.) in

  let shapes = [ floor_ceiling; walls;
                 table_top; leg1; leg2; leg3; leg4;
                 glass_cube;
                 little_cube1; little_cube2; little_cube3; little_cube4; little_cube5;
                 frame1; frame2; frame3;
                 mirror_frame; mirror ]
  in

  let world = RTCWorld.build ~shapes:shapes ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 8. 6. (-8.) in
    let to_p = RTCTuple.point 0. 3. 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 0.7854 view in

  (world, camera)


let run () =
  let (world, camera) = scene () in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "12-table.ppm" in
  output_string f ppm;
  Printf.printf "wrote `12-table.ppm'\n"
