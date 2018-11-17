let half_pi = Float.pi /. 2.
let qtr_pi = Float.pi /. 4.

let scene () =
  let floor = new RTCSphere.shape in
  floor#set_transform (RTCTransform.scaling 10. 0.01 10.);
  floor#set_material (RTCMaterial.build ~color:(RTCColor.build 1. 0.9 0.9)
                                        ~specular:0.
                                        ());

  let left_wall = new RTCSphere.shape in
  let tx = RTCTransform.translation 0. 0. 5. in
  let ry = RTCTransform.rotation_y (-.qtr_pi) in
  let rx = RTCTransform.rotation_x half_pi in
  let sx = RTCTransform.scaling 10. 0.01 10. in
  left_wall#set_transform (List.fold_left RTCMatrix.mult tx [ry; rx; sx]);
  left_wall#set_material floor#material;

  let right_wall = new RTCSphere.shape in
  let tx = RTCTransform.translation 0. 0. 5. in
  let ry = RTCTransform.rotation_y qtr_pi in
  let rx = RTCTransform.rotation_x half_pi in
  let sx = RTCTransform.scaling 10. 0.01 10. in
  right_wall#set_transform (List.fold_left RTCMatrix.mult tx [ry; rx; sx]);
  right_wall#set_material floor#material;

  let middle = new RTCSphere.shape in
  middle#set_transform (RTCTransform.translation (-0.5) 1. 0.5);
  middle#set_material (RTCMaterial.build ~color:(RTCColor.build 0.1 1. 0.5)
                                         ~diffuse:0.7
                                         ~specular:0.3
                                         ());

  let right = new RTCSphere.shape in
  let tx = RTCTransform.translation 1.5 0.5 (-0.5) in
  let sx = RTCTransform.scaling 0.5 0.5 0.5 in
  right#set_transform (RTCMatrix.mult tx sx);
  right#set_material (RTCMaterial.build ~color:(RTCColor.build 0.5 1. 0.1)
                                        ~diffuse:0.7
                                        ~specular:0.3
                                        ());

  let left = new RTCSphere.shape in
  let tx = RTCTransform.translation (-1.5) 0.33 (-0.75) in
  let sx = RTCTransform.scaling 0.33 0.33 0.33 in
  left#set_transform (RTCMatrix.mult tx sx);
  left#set_material (RTCMaterial.build ~color:(RTCColor.build 1. 0.8 0.1)
                                        ~diffuse:0.7
                                        ~specular:0.3
                                        ());

  let light = RTCLight.point (RTCTuple.point (-10.) 10. (-10.)) (RTCColor.build 1. 1. 1.) in

  let shapes = [floor; left_wall; right_wall; middle; right; left] in

  RTCWorld.build ~shapes:shapes ~lights:[light] ()


let () =
  let world = scene () in

  let view = RTCTransform.view (RTCTuple.point 0. 1.5 (-5.)) (RTCTuple.point 0. 1. 0.) (RTCTuple.vector 0. 1. 0.) in
  let camera = RTCCamera.build 600 300 (Float.pi /. 3.) view in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "scene.ppm" in
  output_string f ppm
