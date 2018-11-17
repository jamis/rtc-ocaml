exception NotATransform of string

type transform =
  | Translate of float * float * float
  | Scale of float * float * float
  | RotateX of float
  | RotateY of float
  | RotateZ of float

let compose tforms =
  let rec aux acc = function
    | [] -> acc
    | Translate (dx, dy, dz) :: tforms -> aux (RTCMatrix.mult (RTCTransform.translation dx dy dz) acc) tforms
    | Scale (sx, sy, sz) :: tforms -> aux (RTCMatrix.mult (RTCTransform.scaling sx sy sz) acc) tforms
    | RotateX theta :: tforms -> aux (RTCMatrix.mult (RTCTransform.rotation_x theta) acc) tforms
    | RotateY theta :: tforms -> aux (RTCMatrix.mult (RTCTransform.rotation_y theta) acc) tforms
    | RotateZ theta :: tforms -> aux (RTCMatrix.mult (RTCTransform.rotation_z theta) acc) tforms
  in
  aux RTCMatrix.identity tforms

let scene () =
  let mbase = RTCMaterial.build ~ambient:0.2
                                ~diffuse:0.8
                                ~specular:0.3
                                ~shininess:200.
                                ()
  in

  let wall = new RTCSphere.shape in
  wall#set_transform (compose [Scale (200., 200., 0.01); Translate (0., 0., 20.)]);
  wall#set_material (RTCMaterial.build ~ambient:0. ~diffuse:0.5 ~specular:0. ());

  let palm = new RTCSphere.shape in
  palm#set_material { mbase with color=(RTCColor.build 0.1 0.1 1.) };
  palm#set_transform (compose [Scale (4., 3., 3.); Translate (0., 0., -15.)]);

  let wrist = new RTCSphere.shape in
  wrist#set_material { mbase with color=(RTCColor.build 0.1 1. 1.) };
  wrist#set_transform (compose [Scale (3., 3., 3.); Translate (-4., 0., -21.); RotateZ (Float.pi /. 4.)]);

  let thumb = new RTCSphere.shape in
  thumb#set_material { mbase with color=(RTCColor.build 0.1 0.1 1.) };
  thumb#set_transform (compose [Scale (1., 3., 1.); Translate (-2., 2., -16.)]);

  let index = new RTCSphere.shape in
  index#set_material { mbase with color=(RTCColor.build 1. 1. 0.1) };
  index#set_transform (compose [Scale (3., 0.75, 0.75); Translate (3., 2., -22.)]);

  let middle = new RTCSphere.shape in
  middle#set_material { mbase with color=(RTCColor.build 0.1 1. 0.5) };
  middle#set_transform (compose [Scale (3., 0.75, 0.75); Translate (4., 1., -19.)]);

  let ring = new RTCSphere.shape in
  ring#set_material { mbase with color=(RTCColor.build 0.1 1. 0.1) };
  ring#set_transform (compose [Scale (3., 0.75, 0.75); Translate (4., 0., -18.)]);

  let pinky = new RTCSphere.shape in
  pinky#set_material { mbase with color=(RTCColor.build 0.1 0.5 1.) };
  pinky#set_transform (compose [Scale (2.5, 0.6, 0.6); Translate (1., 0., 0.); RotateZ (-.Float.pi /. 10.); Translate (3., -1.5, -20.)]);

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
