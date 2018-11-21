open RTCComposition

let solid r g b = RTCPattern.Solid (RTCColor.build r g b)

let scene () =
  let wall_material =
    let c1 = solid 0.45 0.45 0.45 in
    let c2 = solid 0.55 0.55 0.55 in
    let transform = compose [Scale (0.25, 0.25, 0.25); RotateY 1.5708] in
    let p = RTCPattern.transform (RTCPattern.stripe c1 c2) transform in
    RTCMaterial.build ~pattern:(Some p) ~ambient:0. ~diffuse:0.4 ~specular:0. ~reflective:0.3 ()
  in

  let floor =
    let plane = RTCPlane.build () in
    let transform = RTCTransform.rotation_y 0.31415 in
    let checkers =
      let c1 = solid 0.35 0.35 0.35 in
      let c2 = solid 0.65 0.65 0.65 in
      let p = RTCPattern.checkers c1 c2 in
      RTCMaterial.build ~pattern:(Some p) ~specular:0. ~reflective:0.4 ()
    in
    RTCShape.transform (RTCShape.texture plane checkers) transform
  in

  let ceiling =
    let plane = RTCPlane.build () in
    let transform = RTCTransform.translation 0. 5. 0. in
    let material = RTCMaterial.build ~color:(RTCColor.build 0.8 0.8 0.8) ~ambient:0.3 ~specular:0. () in
    RTCShape.transform (RTCShape.texture plane material) transform
  in

  let west_wall =
    let plane = RTCPlane.build () in
    let transform = compose [RotateY 1.5708; RotateZ 1.5708; Translate (-5., 0., 0.)] in
    RTCShape.transform (RTCShape.texture plane wall_material) transform
  in

  let east_wall =
    let plane = RTCPlane.build () in
    let transform = compose [RotateY 1.5708; RotateZ 1.5708; Translate (5., 0., 0.)] in
    RTCShape.transform (RTCShape.texture plane wall_material) transform
  in

  let north_wall =
    let plane = RTCPlane.build () in
    let transform = compose [RotateX 1.5708; Translate (0., 0., 5.)] in
    RTCShape.transform (RTCShape.texture plane wall_material) transform
  in

  let south_wall =
    let plane = RTCPlane.build () in
    let transform = compose [RotateX 1.5708; Translate (0., 0., -5.)] in
    RTCShape.transform (RTCShape.texture plane wall_material) transform
  in

  let bg_ball1 =
    let sphere = RTCSphere.build () in
    let transform = compose [Scale (0.4, 0.4, 0.4); Translate (4.6, 0.4, 1.)] in
    let material = RTCMaterial.build ~color:(RTCColor.build 0.8 0.5 0.3) ~shininess:50. () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let bg_ball2 =
    let sphere = RTCSphere.build () in
    let transform = compose [Scale (0.3, 0.3, 0.3); Translate (4.7, 0.3, 0.4)] in
    let material = RTCMaterial.build ~color:(RTCColor.build 0.9 0.4 0.5) ~shininess:50. () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let bg_ball3 =
    let sphere = RTCSphere.build () in
    let transform = compose [Scale (0.5, 0.5, 0.5); Translate (-1., 0.5, 4.5)] in
    let material = RTCMaterial.build ~color:(RTCColor.build 0.4 0.9 0.6) ~shininess:50. () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let bg_ball4 =
    let sphere = RTCSphere.build () in
    let transform = compose [Scale (0.3, 0.3, 0.3); Translate (-1.7, 0.3, 4.7)] in
    let material = RTCMaterial.build ~color:(RTCColor.build 0.4 0.6 0.9) ~shininess:50. () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let red_sphere =
    let sphere = RTCSphere.build () in
    let transform = RTCTransform.translation (-0.6) 1. 0.6 in
    let material = RTCMaterial.build ~color:(RTCColor.build 1. 0.3 0.2) ~specular:0.4 ~shininess:5. () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let blue_glass_sphere =
    let sphere = RTCSphere.build () in
    let transform = compose [Scale (0.7, 0.7, 0.7); Translate (0.6, 0.7, -0.6)] in
    let material = RTCMaterial.build ~color:(RTCColor.build 0. 0. 0.2) ~ambient:0. ~diffuse:0.4 ~specular:0.9 ~shininess:300. ~reflective:0.9 ~transparency:0.9 ~refractive_index:1.5 () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let green_glass_sphere =
    let sphere = RTCSphere.build () in
    let transform = compose [Scale (0.5, 0.5, 0.5); Translate (-0.7, 0.5, -0.8)] in
    let material = RTCMaterial.build ~color:(RTCColor.build 0. 0.2 0.) ~ambient:0. ~diffuse:0.4 ~specular:0.9 ~shininess:300. ~reflective:0.9 ~transparency:0.9 ~refractive_index:1.5 () in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let light = RTCLight.point (RTCTuple.point (-4.9) 4.9 (-1.)) (RTCColor.build 1. 1. 1.) in

  let shapes = [floor; ceiling; west_wall; east_wall; north_wall; south_wall;
                bg_ball1; bg_ball2; bg_ball3; bg_ball4;
                red_sphere; blue_glass_sphere; green_glass_sphere] in

  let world = RTCWorld.build ~shapes:shapes ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point (-2.6) 1.5 (-3.9) in
    let to_p = RTCTuple.point (-0.6) 1. (-0.8) in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 600 300 1.15 view in

  (world, camera)


let air_bubble_scene () =
  let wall =
    let plane = RTCPlane.build () in
    let checkers =
      let c1 = solid 0.15 0.15 0.15 in
      let c2 = solid 0.85 0.85 0.85 in
      let p = RTCPattern.checkers c1 c2 in
      RTCMaterial.build ~pattern:(Some p) ~ambient:0.8 ~diffuse:0.2 ~specular:0. ()
    in
    let transform = compose [RotateX 1.5708; Translate (0., 0., 10.)] in
    RTCShape.transform (RTCShape.texture plane checkers) transform
  in

  let glass_ball =
    let sphere = RTCSphere.build () in
    let material = RTCMaterial.build ~ambient:0. ~diffuse:0. ~specular:0.9
                                     ~shininess: 300.
                                     ~reflective:0.9 ~transparency:0.9
                                     ~refractive_index:1.5 ()
    in
    RTCShape.texture sphere material
  in

  let hollow_center =
    let sphere = RTCSphere.build () in
    let transform = RTCTransform.scaling 0.5 0.5 0.5 in
    let material = RTCMaterial.build ~ambient:0. ~diffuse:0. ~specular:0.9
                                     ~shininess:300.
                                     ~reflective: 0.9 ~transparency:0.9
                                     ~refractive_index:1.0000034 ()
    in
    RTCShape.transform (RTCShape.texture sphere material) transform
  in

  let light = RTCLight.point (RTCTuple.point (-20.) 80. (-20.)) (RTCColor.build 1. 1. 1.) in

  let world = RTCWorld.build ~shapes:[wall; glass_ball; hollow_center] ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 0. 0. (-5.) in
    let to_p = RTCTuple.point 0. 0. 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 300 300 0.45 view in

  (world, camera)


let run () =
  let (world, camera) = scene () in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "11-reflect-refract.ppm" in
  output_string f ppm;
  Printf.printf "wrote `11-reflect-refract.ppm'\n"
