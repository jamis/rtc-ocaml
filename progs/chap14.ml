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
  let hexagon =
    let shape = make_hexagon () in
    let material = RTCMaterial.build ~color:(color 1. 0.2 0.4) ~diffuse:0.8 ~specular:0.6 ~ambient:0. ~shininess:50. ~reflective:0.3 () in
    RTCShape.texture shape material
  in

  let light = RTCLight.point (RTCTuple.point 5. 8. (-9.)) (color 1. 1. 1.) in

  let world = RTCWorld.build ~shapes:[hexagon] ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 2. 4. (-9.) in
    let to_p = RTCTuple.point 0. 0. 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 800 400 0.3 view in

  (world, camera)


let deg rads = Float.pi *. rads /. 180.

let book_scene () =
  let leg () =
    let corner =
      let sphere = RTCSphere.build () in
      let transform = compose [ UScale 0.25; Translate (0., 0., -1.) ] in
      RTCShape.transform sphere transform
    in
    let edge =
      let cyl = RTCCylinder.build ~minimum:0. ~maximum:1. ~closed:false () in
      let transform = compose [ Scale (0.25, 1., 0.25); RotateZ (deg (-90.)); RotateY (deg (-30.)); Translate (0., 0., -1.) ] in
      RTCShape.transform cyl transform
    in
    RTCGroup.build ~children:[corner; edge] ()
  in
  let arm () =
    let cone = RTCCone.build ~minimum:(-1.) ~maximum:0. ~closed:false () in
    let transform = compose [ Scale (0.246062746062869, 1.370019388548759, 0.246062746062869); RotateX (deg (-45.)) ] in
    RTCShape.transform cone transform
  in
  let cap ?(arms=6) () =
    let inc = 2. *. Float.pi /. (float_of_int arms) in
    let rec loop acc = function
      | 0 -> acc
      | i ->
        let a = arm () in
        let angle = (float_of_int i) *. inc in
        let tx = RTCMatrix.mult (RTCTransform.rotation_y angle) a.transform in
        loop ((RTCShape.transform a tx) :: acc) (i - 1)
    in
    RTCGroup.build ~children:(loop [] arms) ()
  in
  let wacky ?(sides=6) () =
    let inc = 2. *. Float.pi /. (float_of_int sides) in
    let rec loop acc = function
      | 0 -> acc
      | i ->
        let l = leg () in
        let angle = (float_of_int i) *. inc in
        let tx = RTCMatrix.mult (RTCTransform.rotation_y angle) l.transform in
        loop ((RTCShape.transform l tx) :: acc) (i - 1)
    in
    let rim = loop [] sides in
    let top =
      let shape = cap ~arms:sides () in
      let tx = RTCTransform.translation 0. 1. 0. in
      RTCShape.transform shape tx
    in
    let bottom =
      let shape = cap ~arms:sides () in
      let tx = compose [ RotateX Float.pi; Translate (0., -1., 0.) ] in
      RTCShape.transform shape tx
    in
    RTCGroup.build ~children:(top :: bottom :: rim) ()
  in
  let backdrop =
    let plane = RTCPlane.build () in
    let material = RTCMaterial.build ~ambient:1. ~diffuse:0. ~specular:0. () in
    let transform = compose [ RotateX (deg 90.); Translate (0., 0., 100.) ] in
    RTCShape.transform (RTCShape.texture plane material) transform
  in
  let material = RTCMaterial.build ~ambient:0.2 ~diffuse:0.8 ~specular:0.7 ~shininess:20.0 () in
  let wacky_red =
    let red = { material with color=(RTCColor.build 0.9 0.2 0.4) } in
    let shape = wacky () in
    let transform = compose [ RotateY (deg 10.); RotateX (deg 25.); Translate (-2.8, 0., 0.) ] in
    RTCShape.transform (RTCShape.texture shape red) transform
  in
  let wacky_green =
    let green = { material with color=(RTCColor.build 0.2 0.9 0.4) } in
    let shape = wacky () in
    let transform = compose [ RotateY (deg 10.) ] in
    RTCShape.transform (RTCShape.texture shape green) transform
  in
  let wacky_blue =
    let blue = { material with color=(RTCColor.build 0.2 0.3 1.) } in
    let shape = wacky () in
    let transform = compose [ RotateY (deg (-10.)); RotateX (deg (-25.)); Translate (2.8, 0., 0.) ] in
    RTCShape.transform (RTCShape.texture shape blue) transform
  in

  let lights =
    let c = RTCColor.mults (color 1. 1. 1.) 0.25 in
    let l1 = RTCLight.point (RTCTuple.point 10000. 10000. (-10000.)) c in
    let l2 = RTCLight.point (RTCTuple.point (-10000.) 10000. (-10000.)) c in
    let l3 = RTCLight.point (RTCTuple.point 10000. (-10000.) (-10000.)) c in
    let l4 = RTCLight.point (RTCTuple.point (-10000.) (-10000.) (-10000.)) c in
    [ l1; l2; l3; l4 ]
  in

  let world = RTCWorld.build ~shapes:[backdrop; wacky_red; wacky_green; wacky_blue] ~lights:lights () in

  let view =
    let from_p = RTCTuple.point 0. 0. (-9.) in
    let to_p = RTCTuple.point 0. 0. 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 (deg 52.) view in

  (world, camera)

let run () =
  let (world, camera) = hexagon_scene () in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "14-groups.ppm" in
  output_string f ppm;
  Printf.printf "wrote `14-groups.ppm'\n"
