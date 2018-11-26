open RTCComposition

let solid r g b = RTCPattern.Solid (RTCColor.build r g b)
let color = RTCColor.build

let rads degrees = degrees *. Float.pi /. 180.0
let r2 = sqrt 2.

let scene () =
  let wedge transform =
    let cube = { (RTCCube.build ()) with shadow=false } in
    let tx = compose [ RotateY (rads 45.); Translate (r2, 0., 0.); Scale (1., 1., 0.15) ] in
    RTCShape.transform cube (RTCMatrix.mult transform tx)
  in
  let red_material = RTCMaterial.build ~color:(color 1. 0. 0.) ~ambient:0.2 () in
  let green_material = RTCMaterial.build ~color:(color 0. 1. 0.) ~ambient:0.2 () in
  let blue_material = RTCMaterial.build ~color:(color 0. 0. 1.) ~ambient:0.2 () in
  let dark_mirror = RTCMaterial.build ~color:RTCColor.black ~ambient:0. ~diffuse:0.4 ~reflective:0.5 () in
  let transparent = RTCMaterial.build ~color:RTCColor.black ~ambient:0. ~diffuse:0. ~reflective:0. ~transparency:1. ~refractive_index:1. () in

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

  let tricylinder =
    let left =
      let cyl = RTCCylinder.build ~minimum:(-1.) ~maximum:1. ~closed: true () in
      let transform = RTCTransform.scaling 0.5 1.1 0.5 in
      RTCShape.transform (RTCShape.texture cyl { red_material with ambient=0.1; diffuse=0.5; reflective=0.3 }) transform
    in
    let right =
      let left =
        let cyl = RTCCylinder.build ~minimum:(-1.) ~maximum:1. ~closed: true () in
        let transform = compose [ Scale (0.5, 1.1, 0.5); RotateX (rads 90.) ] in
        RTCShape.transform (RTCShape.texture cyl { green_material with ambient=0.1; diffuse=0.5; reflective=0.3 }) transform
      in
      let right =
        let cyl = RTCCylinder.build ~minimum:(-1.) ~maximum:1. ~closed: true () in
        let transform = compose [ Scale (0.5, 1.1, 0.5); RotateZ (rads 90.) ] in
        RTCShape.transform (RTCShape.texture cyl { blue_material with ambient=0.1; diffuse=0.5; reflective=0.3 }) transform
      in
      RTCCSG.intersection left right
    in
    let csg = RTCCSG.intersection left right in
    let transform = compose [ RotateY 0.4; RotateX (-0.1); RotateZ (-0.2); Translate (-1.5, 0.7, 0.) ] in
    RTCShape.transform csg transform
  in

  let box =
    let left =
      let sphere = RTCSphere.build () in
      let transform = compose [ UScale 1.4 ] in
      let material = RTCMaterial.build ~color:(color 0.1 0.1 0.1) ~ambient:0.2 ~diffuse:0.9 ~specular:1. ~shininess:50. () in
      RTCShape.transform (RTCShape.texture sphere material) transform
    in
    let right =
      let left = RTCShape.texture (RTCCube.build ()) dark_mirror in
      let right =
        let cylY =
          let cyl = RTCCylinder.build ~minimum:(-1.) ~maximum:1. ~closed:true () in
          let transform = RTCTransform.scaling 0.5 1.1 0.5 in
          RTCShape.transform (RTCShape.texture cyl red_material) transform
        in
        let cylZ =
          let cyl = RTCCylinder.build ~minimum:(-1.) ~maximum:1. ~closed:true () in
          let transform = compose [ Scale (0.5, 1.1, 0.5); RotateX (rads 90.) ] in
          RTCShape.transform (RTCShape.texture cyl green_material) transform
        in
        let cylX =
          let cyl = RTCCylinder.build ~minimum:(-1.) ~maximum:1. ~closed:true () in
          let transform = compose [ Scale (0.5, 1.1, 0.5); RotateZ (rads 90.) ] in
          RTCShape.transform (RTCShape.texture cyl blue_material) transform
        in
        RTCGroup.build ~children:[cylX; cylY; cylZ] ()
      in
      RTCCSG.difference left right
    in
    let csg = RTCCSG.intersection left right in
    let tx = compose [ Translate (0., 1., 0.); UScale 0.5; RotateY 1.3 ] in
    RTCShape.transform csg tx
  in

  let ball =
    let left = RTCShape.texture (RTCSphere.build ()) red_material in
    let right =
      let rec loop inc acc = function
        | 0 -> acc
        | n ->
          let angle = (float_of_int n) *. inc in
          let transform = RTCTransform.rotation_y angle in
          loop inc ((wedge transform) :: acc) (n - 1)
      in
      let children = loop (Float.pi /. 6.) [] 12 in
      RTCShape.texture (RTCGroup.build ~children:children ()) transparent
    in
    let csg = RTCCSG.intersection left right in
    let transform = compose [ Translate (0., 1., 0.); UScale 0.5; RotateY (-0.5); RotateX (-0.1); RotateZ 0.1; Translate (1.5, 0.25, 0.) ] in
    RTCShape.transform csg transform
  in

  let light = RTCLight.point (RTCTuple.point (-2.) 5. (-2.)) (color 1. 1. 1.) in
  let world = RTCWorld.build ~shapes:[room; tricylinder; box; ball] ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 0. 2. (-4.9) in
    let to_p = RTCTuple.point 0. 0.5 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 0.9 view in

  (world, camera)

let run () =
  let (world, camera) = scene () in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "16-csg.ppm" in
  output_string f ppm;
  Printf.printf "wrote `16-csg.ppm'\n"
