open RTCComposition

let half_pi = Float.pi /. 2.
let qtr_pi = Float.pi /. 4.

let scene () =
  let pattern' = RTCPattern.checkers (RTCColor.build 1. 0.2 0.4) (RTCColor.build 0.9 0.9 1.0) in
  let pattern = Some (RTCPattern.transform pattern' (RTCTransform.scaling 0.5 0.5 0.5)) in

  let floor = RTCShape.texture (RTCPlane.build ())
                               (RTCMaterial.build ~pattern:pattern
                                        ~specular:0.
                                        ())
  in

  let tx = RTCTransform.translation 0. 0. 5. in
  let ry = RTCTransform.rotation_y (-.qtr_pi) in
  let rx = RTCTransform.rotation_x half_pi in
  let left_wall' = RTCShape.transform (RTCPlane.build ()) (List.fold_left RTCMatrix.mult tx [ry; rx]) in
  let left_wall = RTCShape.texture left_wall' floor.material in

  let tx = RTCTransform.translation 0. 0. 5. in
  let ry = RTCTransform.rotation_y qtr_pi in
  let rx = RTCTransform.rotation_x half_pi in
  let right_wall' = RTCShape.transform (RTCPlane.build ()) (List.fold_left RTCMatrix.mult tx [ry; rx]) in
  let right_wall = RTCShape.texture right_wall' floor.material in

  let tx = compose [Scale (0.2, 0.2, 0.2); RotateX (-.Float.pi /. 3.)] in
  let ring = RTCPattern.transform (RTCPattern.ring (RTCColor.build 0. 0.3 0.6) (RTCColor.build 0.1 1. 0.8)) tx in
  let middle' = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation (-0.5) 1. 0.5) in
  let middle = RTCShape.texture middle' (RTCMaterial.build ~pattern:(Some ring)
                                         ~diffuse:0.7
                                         ~specular:0.3
                                         ())
  in

  let tx = compose [Scale (0.2, 0.2, 0.2); RotateZ (-.Float.pi /. 4.)] in
  let stripe = RTCPattern.transform (RTCPattern.stripe (RTCColor.build 0. 0.3 0.6) (RTCColor.build 0.1 1. 0.8)) tx in
  let tx = compose [Scale (0.5, 0.5, 0.5); Translate (1.5, 0.5, -0.5)] in
  let right' = RTCShape.transform (RTCSphere.build ()) tx in
  let right = RTCShape.texture right' (RTCMaterial.build ~pattern:(Some stripe)
                                        ~diffuse:0.7
                                        ~specular:0.3
                                        ())
  in

  let tx = compose [Scale (2., 2., 2.); Translate (-1., 0., 0.)] in
  let gradient = RTCPattern.transform (RTCPattern.gradient (RTCColor.build 1. 0. 0.) (RTCColor.build 0. 1. 1.)) tx in
  let tx = compose [Scale (0.33, 0.33, 0.33); Translate (-1.5, 0.33, -0.75)] in
  let left' = RTCShape.transform (RTCSphere.build ()) tx in
  let left = RTCShape.texture left' (RTCMaterial.build ~pattern:(Some gradient)
                                        ~diffuse:0.7
                                        ~specular:0.3
                                        ())
  in

  let light = RTCLight.point (RTCTuple.point (-10.) 10. (-10.)) (RTCColor.build 1. 1. 1.) in

  let shapes = [floor; left_wall; right_wall; middle; right; left] in

  RTCWorld.build ~shapes:shapes ~lights:[light] ()


let () =
  let world = scene () in

  let view = RTCTransform.view (RTCTuple.point 0. 1.5 (-5.)) (RTCTuple.point 0. 1. 0.) (RTCTuple.vector 0. 1. 0.) in
  let camera = RTCCamera.build 600 300 (Float.pi /. 3.) view in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "patterns.ppm" in
  output_string f ppm
