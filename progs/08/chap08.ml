exception NotATransform of string

open RTCComposition

let sphere_with transform material =
  let s = RTCSphere.build () in
  let s' = RTCShape.transform s transform in
  RTCShape.texture s' material

let scene () =
  let mbase = RTCMaterial.build ~ambient:0.2
                                ~diffuse:0.8
                                ~specular:0.3
                                ~shininess:200.
                                ()
  in

  let wall = sphere_with (compose [Scale (200., 200., 0.01); Translate (0., 0., 20.)])
                         (RTCMaterial.build ~ambient:0. ~diffuse:0.5 ~specular:0. ())
  in

  let palm = sphere_with (compose [Scale (4., 3., 3.); Translate (0., 0., -15.)])
                         { mbase with color=(RTCColor.build 0.1 0.1 1.) }
  in

  let wrist = sphere_with (compose [Scale (3., 3., 3.); Translate (-4., 0., -21.); RotateZ (Float.pi /. 4.)])
                          { mbase with color=(RTCColor.build 0.1 1. 1.) }
  in

  let thumb = sphere_with (compose [Scale (1., 3., 1.); Translate (-2., 2., -16.)])
                          { mbase with color=(RTCColor.build 0.1 0.1 1.) }
  in

  let index = sphere_with (compose [Scale (3., 0.75, 0.75); Translate (3., 2., -22.)])
                          { mbase with color=(RTCColor.build 1. 1. 0.1) }
  in

  let middle = sphere_with (compose [Scale (3., 0.75, 0.75); Translate (4., 1., -19.)])
                           { mbase with color=(RTCColor.build 0.1 1. 0.5) }
  in

  let ring = sphere_with (compose [Scale (3., 0.75, 0.75); Translate (4., 0., -18.)])
                         { mbase with color=(RTCColor.build 0.1 1. 0.1) }
  in

  let pinky = sphere_with (compose [Scale (2.5, 0.6, 0.6); Translate (1., 0., 0.); RotateZ (-.Float.pi /. 10.); Translate (3., -1.5, -20.)])
                          { mbase with color=(RTCColor.build 0.1 0.5 1.) }
  in

  let light = RTCLight.point (RTCTuple.point 0. 0. (-100.)) (RTCColor.build 1. 1. 1.) in
  RTCWorld.build ~shapes:[wall; palm; wrist; thumb; index; middle; ring; pinky] ~lights:[light] ()

let () =
  let world = scene () in

  let view = RTCTransform.view (RTCTuple.point 40. 0. (-70.)) (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 1. 0.) in
  let camera = RTCCamera.build 1000 500 (Float.pi /. 6.) view in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "puppets.ppm" in
  output_string f ppm
