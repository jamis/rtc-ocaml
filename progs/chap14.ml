open RTCComposition

let solid r g b = RTCPattern.Solid (RTCColor.build r g b)
let color = RTCColor.build

let make_hexagon () =
  let make_corner () =
    let corner = RTCSphere.build ()
    and transform = compose [ UScale 0.25; Translate (0., 0., -1.)] in
    RTCShape.transform corner transform
  and make_edge () =
    let edge = RTCCylinder.build ~minimum:0. ~maximum:1. ()
    and transform = compose [ Scale (0.25, 1., 0.25); RotateZ (-.Float.pi /. 2.); RotateY (-.Float.pi /. 6.); Translate (0., 0., -1.)] in
    RTCShape.transform edge transform
  in
  let make_side () =
    let corner = make_corner ()
    and edge = make_edge () in
    RTCGroup.build ~children:[corner; edge] ()
  in
  let rec loop acc = function
    | 6 -> acc
    | n ->
      let angle = (float_of_int n) *. Float.pi /. 3. in
      let side = RTCShape.transform (make_side ()) (RTCTransform.rotation_y angle) in
      loop (side :: acc) (n + 1)
  in
  RTCGroup.build ~children:(loop [] 0) ()

let hexagon_scene () =
  let light = RTCLight.point (RTCTuple.point 5. 8. (-9.)) (color 1. 1. 1.) in

  let world = RTCWorld.build ~shapes:[make_hexagon ()] ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 2. 4. (-9.) in
    let to_p = RTCTuple.point 0. 0. 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 0.3 view in

  (world, camera)


let run () =
  let (world, camera) = hexagon_scene () in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "14-hexagon.ppm" in
  output_string f ppm;
  Printf.printf "wrote `14-hexagon.ppm'\n"
