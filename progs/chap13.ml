open RTCComposition

let solid r g b = RTCPattern.Solid (RTCColor.build r g b)
let color = RTCColor.build

let scene () =
  let floor =
    let plane = RTCPlane.build () in
    let material =
      let pattern =
        let c1 = solid 0.5 0.5 0.5 in
        let c2 = solid 0.75 0.75 0.75 in
        let transform = compose [ UScale 0.25; RotateX 1.57; RotateZ 1.57; RotateY 0.3 ] in
        RTCPattern.transform (RTCPattern.checkers c1 c2) transform
      in
      RTCMaterial.build ~pattern:(Some pattern) ~ambient:0.2 ~diffuse:0.9 ~specular:0. ()
    in
    RTCShape.texture plane material
  in

  let c1 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.75 ~closed:true () in
    let transform = compose [ Scale (0.5, 1., 0.5); Translate (-1., 0., 1.) ] in
    let material = RTCMaterial.build ~color:(color 0. 0. 0.6)
                                     ~diffuse:0.1 ~specular:0.9 ~shininess:300.
                                     ~reflective:0.9 ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  (* concentric cylinders *)
  let c2 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.2 () in
    let transform = compose [ Scale (0.8, 1., 0.8); Translate (1., 0., 0.) ] in
    let material = RTCMaterial.build ~color:(color 1. 1. 0.3)
                                     ~ambient:0.1 ~diffuse:0.8 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let c3 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.3 () in
    let transform = compose [ Scale (0.6, 1., 0.6); Translate (1., 0., 0.) ] in
    let material = RTCMaterial.build ~color:(color 1. 0.9 0.4)
                                     ~ambient:0.1 ~diffuse:0.8 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let c4 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.4 () in
    let transform = compose [ Scale (0.4, 1., 0.4); Translate (1., 0., 0.) ] in
    let material = RTCMaterial.build ~color:(color 1. 0.8 0.5)
                                     ~ambient:0.1 ~diffuse:0.8 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let c5 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.5 ~closed:true () in
    let transform = compose [ Scale (0.2, 1., 0.2); Translate (1., 0., 0.) ] in
    let material = RTCMaterial.build ~color:(color 1. 0.7 0.6)
                                     ~ambient:0.1 ~diffuse:0.8 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  (* decorative cylinders *)

  let c6 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.3 ~closed:true () in
    let transform = compose [ Scale (0.05, 1., 0.05); Translate (0., 0., (-0.75)) ] in
    let material = RTCMaterial.build ~color:(color 1. 0. 0.)
                                     ~ambient:0.1 ~diffuse:0.9 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let c7 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.3 ~closed:true () in
    let transform = compose [ Scale (0.05, 1., 0.05); Translate (0., 0., 1.5); RotateY (-0.15); Translate (0., 0., (-2.25)) ] in
    let material = RTCMaterial.build ~color:(color 1. 1. 0.)
                                     ~ambient:0.1 ~diffuse:0.9 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let c8 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.3 ~closed:true () in
    let transform = compose [ Scale (0.05, 1., 0.05); Translate (0., 0., 1.5); RotateY (-0.3); Translate (0., 0., (-2.25)) ] in
    let material = RTCMaterial.build ~color:(color 0. 1. 0.)
                                     ~ambient:0.1 ~diffuse:0.9 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let c9 =
    let cyl = RTCCylinder.build ~minimum:0. ~maximum:0.3 ~closed:true () in
    let transform = compose [ Scale (0.05, 1., 0.05); Translate (0., 0., 1.5); RotateY (-0.45); Translate (0., 0., (-2.25)) ] in
    let material = RTCMaterial.build ~color:(color 0. 1. 1.)
                                     ~ambient:0.1 ~diffuse:0.9 ~specular:0.9
                                     ~shininess:300. ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  (* glass cylinder *)
  let c10 =
    let cyl = RTCCylinder.build ~minimum:0.0001 ~maximum:0.5 ~closed:true () in
    let transform = compose [ Scale (0.33, 1., 0.33); Translate (0., 0., (-1.5)) ] in
    let material = RTCMaterial.build ~color:(color 0.25 0. 0.)
                                     ~diffuse:0.1 ~specular:0.9 ~shininess:300.
                                     ~reflective:0.9 ~transparency:0.9 ~refractive_index:1.5 ()
    in
    RTCShape.transform (RTCShape.texture cyl material) transform
  in

  let light = RTCLight.point (RTCTuple.point 1. 6.9 (-4.9)) (color 1. 1. 1.) in

  let shapes = [ floor; c1; c2; c3; c4; c5; c6; c7; c8; c9; c10 ] in

  let world = RTCWorld.build ~shapes:shapes ~lights:[light] () in

  let view =
    let from_p = RTCTuple.point 8. 3.5 (-9.) in
    let to_p = RTCTuple.point 0. 0.3 0. in
    let up_v = RTCTuple.vector 0. 1. 0. in
    RTCTransform.view from_p to_p up_v
  in

  let camera = RTCCamera.build 400 200 0.3 view in

  (world, camera)


let run () =
  let (world, camera) = scene () in

  let image = RTCCamera.render camera world in
  let ppm = RTCCanvas.to_ppm image in
  let f = open_out "13-cylinders.ppm" in
  output_string f ppm;
  Printf.printf "wrote `13-cylinders.ppm'\n"
