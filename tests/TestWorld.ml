open OUnit2

let r2 = sqrt(2.)
let r2d2 = r2 /. 2.

let default_world () =
  let s1 = RTCSphere.build () in
  let m1 = RTCMaterial.build ~color:(RTCColor.build 0.8 1.0 0.6)
                             ~diffuse:0.7
                             ~specular:0.2
                             ()
  in
  let s1' = RTCShape.texture s1 m1 in
  let s2 = RTCSphere.build () in
  let s2' = RTCShape.transform s2 (RTCTransform.scaling 0.5 0.5 0.5) in
  let light = RTCLight.point (RTCTuple.point (-10.) 10. (-10.)) (RTCColor.build 1. 1. 1.) in
  RTCWorld.build ~shapes:[s1'; s2'] ~lights:[light] ()

let tests =
  "World" >:::
  [
    "Creating a world" >::
    (fun test_ctxt ->
      let w = RTCWorld.build () in
      assert_equal 0 (List.length w.shapes);
      assert_equal 0 (List.length w.lights));

    "The default world" >::
    (fun test_ctxt ->
      let light = RTCLight.point (RTCTuple.point (-10.) 10. (-10.)) (RTCColor.build 1. 1. 1.) in
      let m1 = RTCMaterial.build ~color:(RTCColor.build 0.8 1.0 0.6)
                                 ~diffuse:0.7
                                 ~specular:0.2
                                 ()
      in
      let m2 = RTCMaterial.build () in
      let t2 = RTCTransform.scaling 0.5 0.5 0.5 in
      let w = default_world () in
      let s1 = List.nth w.shapes 0 in
      let s2 = List.nth w.shapes 1 in
      let l1 = List.nth w.lights 0 in
      assert_equal l1 light;
      assert_equal m1 (RTCShape.material s1);
      assert_equal m2 (RTCShape.material s2);
      assert_equal RTCMatrix.identity s1.transform;
      assert_equal t2 s2.transform);

    "Intersect a world with a ray" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let xs = RTCWorld.intersect w r in
      assert_equal 4 (List.length xs);
      assert_equal 4. (List.nth xs 0).t;
      assert_equal 4.5 (List.nth xs 1).t;
      assert_equal 5.5 (List.nth xs 2).t;
      assert_equal 6. (List.nth xs 3).t);

    "Shading an intersection" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = List.nth w.shapes 0 in
      let i = RTCIntersection.build 4. shape [] in
      let comps = RTCComps.prepare i r [i] in
      let c = RTCWorld.shade_hit w comps 5 in
      assert (RTCColor.equal c (RTCColor.build 0.38066 0.47583 0.2855)));

    "Shading an intersection from the inside" >::
    (fun test_ctx ->
      let dw = default_world () in
      let w = { dw with lights=[ RTCLight.point (RTCTuple.point 0. 0.25 0.) (RTCColor.build 1. 1. 1.) ] } in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = List.nth w.shapes 1 in
      let i = RTCIntersection.build 0.5 shape [] in
      let comps = RTCComps.prepare i r [i] in
      let c = RTCWorld.shade_hit w comps 5 in
      assert (RTCColor.equal c (RTCColor.build 0.90498 0.90498 0.90498)));

    "The color when a ray misses" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 1. 0.) in
      let c = RTCWorld.color_at w r 5 in
      assert (RTCColor.equal c RTCColor.black));

    "The color when a ray hits" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let c = RTCWorld.color_at w r 5 in
      assert (RTCColor.equal c (RTCColor.build 0.38066 0.47583 0.2855)));

    "The color with an intersection behind the ray" >::
    (fun test_ctx ->
      let w = default_world () in
      let outer = (List.nth w.shapes 0) in
      let outer' = RTCShape.texture outer { (RTCShape.material outer) with ambient=1.0 } in
      let inner = (List.nth w.shapes 1) in
      let inner' = RTCShape.texture inner { (RTCShape.material inner) with ambient=1.0 } in
      let w' = { w with shapes=[outer'; inner'] } in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.75) (RTCTuple.vector 0. 0. (-1.)) in
      let c = RTCWorld.color_at w' r 5 in
      assert (RTCColor.equal c (RTCShape.material inner').color));

    "There is no shadow when nothing is collinear with point and light" >::
    (fun test_ctx ->
      let w = default_world () in
      let p = RTCTuple.point 0. 10. 0. in
      assert (not (RTCWorld.is_shadowed w (List.nth w.lights 0) p)));

    "The shadow when an object is between the point and the light" >::
    (fun test_ctx ->
      let w = default_world () in
      let p = RTCTuple.point 10. (-10.) 10. in
      assert (RTCWorld.is_shadowed w (List.nth w.lights 0) p));

    "There is no shadow when an object is behind the light" >::
    (fun test_ctx ->
      let w = default_world () in
      let p = RTCTuple.point (-20.) 20. (-20.) in
      assert (not (RTCWorld.is_shadowed w (List.nth w.lights 0) p)));

    "There is no shadow when an object is behind the point" >::
    (fun test_ctx ->
      let w = default_world () in
      let p = RTCTuple.point (-2.) 2. (-2.) in
      assert (not (RTCWorld.is_shadowed w (List.nth w.lights 0) p)));

    "shade_hit() is given an intersection in shadow" >::
    (fun test_ctx ->
      let light = RTCLight.point (RTCTuple.point 0. 0. (-10.)) (RTCColor.build 1. 1. 1.) in
      let s1 = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 0. 0. 10.) in
      let s2 = RTCSphere.build () in
      let w = RTCWorld.build ~shapes:[s1; s2] ~lights:[light] () in
      let r = RTCRay.build (RTCTuple.point 0. 0. 5.) (RTCTuple.vector 0. 0. 1.) in
      let i = RTCIntersection.build 4. s2 [] in
      let comps = RTCComps.prepare i r [i] in
      let c = RTCWorld.shade_hit w comps 5 in
      assert (RTCColor.equal c (RTCColor.build 0.1 0.1 0.1)));

    "The reflected color for a non-reflective material" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = (List.nth w.shapes 1) in
      let shape' = RTCShape.texture shape { (RTCShape.material shape) with ambient=1.0 } in
      let w' = { w with shapes=[(List.nth w.shapes 0); shape'] } in
      let i = RTCIntersection.build 1. shape' [] in
      let comps = RTCComps.prepare i r [i] in
      let color = RTCWorld.reflected_color w' comps 5 in
      assert (RTCColor.equal RTCColor.black color));

    "The reflected color for a reflective material" >::
    (fun test_ctxt ->
      let w' = default_world () in
      let shape' = RTCShape.texture (RTCPlane.build ()) (RTCMaterial.build ~reflective:0.5 ()) in
      let shape = RTCShape.transform shape' (RTCTransform.translation 0. (-1.) 0.) in
      let w = { w' with shapes=shape :: w'.shapes } in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-3.)) (RTCTuple.vector 0. (-.r2d2) r2d2) in
      let i = RTCIntersection.build r2 shape [] in
      let comps = RTCComps.prepare i r [i] in
      let color = RTCWorld.reflected_color w comps 5 in
      assert (RTCColor.equal color (RTCColor.build 0.19032 0.2379 0.14274)));

    "shade_hit() with a reflective material" >::
    (fun test_ctx ->
      let w' = default_world () in
      let shape' = RTCShape.texture (RTCPlane.build ()) (RTCMaterial.build ~reflective:0.5 ()) in
      let shape = RTCShape.transform shape' (RTCTransform.translation 0. (-1.) 0.) in
      let w = { w' with shapes=shape :: w'.shapes } in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-3.)) (RTCTuple.vector 0. (-.r2d2) r2d2) in
      let i = RTCIntersection.build r2 shape [] in
      let comps = RTCComps.prepare i r [i] in
      let color = RTCWorld.shade_hit w comps 5 in
      assert (RTCColor.equal color (RTCColor.build 0.87677 0.92436 0.82918)));

    "color_at() with mutually reflective surfaces" >::
    (fun test_ctx ->
      let reflective_plane () = RTCShape.texture (RTCPlane.build ()) (RTCMaterial.build ~reflective:1. ()) in
      let light = RTCLight.point (RTCTuple.point 0. 0. 0.) (RTCColor.build 1. 1. 1.) in
      let lower = RTCShape.transform (reflective_plane ()) (RTCTransform.translation 0. (-1.) 0.) in
      let upper = RTCShape.transform (reflective_plane ()) (RTCTransform.translation 0. 1. 0.) in
      let w = RTCWorld.build ~shapes:[lower; upper] ~lights:[light] () in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 1. 0.) in
      let _ = RTCWorld.color_at w r 5 in
      (* just assert that we reached here, thus showing that color_at terminates *)
      assert true);

    "The reflected color at the maximum recursive depth" >::
    (fun test_ctx ->
      let w' = default_world () in
      let shape' = RTCShape.texture (RTCPlane.build ()) (RTCMaterial.build ~reflective:0.5 ()) in
      let shape = RTCShape.transform shape' (RTCTransform.translation 0. (-1.) 0.) in
      let w = { w' with shapes=shape :: w'.shapes } in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-3.)) (RTCTuple.vector 0. (-.r2d2) r2d2) in
      let i = RTCIntersection.build r2 shape [] in
      let comps = RTCComps.prepare i r [i] in
      let color = RTCWorld.reflected_color w comps 0 in
      assert (RTCColor.equal color RTCColor.black));

    "The refracted color with an opaque surface" >::
    (fun test_ctx ->
      let default = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = (List.nth default.shapes 0) in
      let w = { default with shapes=shape :: (List.tl default.shapes) } in
      let xs =
        let x1 = RTCIntersection.build 4. shape [] in
        let x2 = RTCIntersection.build 6. shape [] in
        [ x1; x2 ]
      in
      let comps = RTCComps.prepare (List.hd xs) r xs in
      let color = RTCWorld.refracted_color w comps 5 in
      assert (RTCColor.equal RTCColor.black color));

    "The refracted color at the maximum recursive depth" >::
    (fun test_ctx ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = TestSphere.glass_sphere () in
      let w =
        let default = default_world () in
        { default with shapes=shape :: (List.tl default.shapes) }
      in
      let xs =
        let x1 = RTCIntersection.build 4. shape [] in
        let x2 = RTCIntersection.build 6. shape [] in
        [ x1; x2 ]
      in
      let comps = RTCComps.prepare (List.hd xs) r xs in
      let color = RTCWorld.refracted_color w comps 0 in
      assert (RTCColor.equal RTCColor.black color));

    "The refracted color under total internal reflection" >::
    (fun test_ctx ->
      let r = RTCRay.build (RTCTuple.point 0. 0. r2d2) (RTCTuple.vector 0. 1. 0.) in
      let shape = TestSphere.glass_sphere () in
      let w =
        let default = default_world () in
        { default with shapes=shape :: (List.tl default.shapes) }
      in
      let xs =
        let x1 = RTCIntersection.build (-.r2d2) shape [] in
        let x2 = RTCIntersection.build r2d2 shape [] in
        [ x1; x2 ]
      in
      let comps = RTCComps.prepare (List.nth xs 1) r xs in
      let color = RTCWorld.refracted_color w comps 5 in
      assert (RTCColor.equal RTCColor.black color));

    "The refracted color with a refracted ray" >::
    (fun test_ctx ->
      let default = default_world () in
      let a =
        let first = List.nth default.shapes 0 in
        let material = { (RTCShape.material first) with ambient=1.0; pattern=Some (TestPattern.test_pattern ()) } in
        RTCShape.texture first material
      in
      let b =
        let second = List.nth default.shapes 1 in
        let material = { (RTCShape.material second) with transparency=1.; refractive_index=1.5 } in
        RTCShape.texture second material
      in
      let w = { default with shapes=[a; b] } in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.1) (RTCTuple.vector 0. 1. 0.) in
      let xs =
        let x1 = RTCIntersection.build (-0.9899) a [] in
        let x2 = RTCIntersection.build (-0.4899) b [] in
        let x3 = RTCIntersection.build 0.4899 b [] in
        let x4 = RTCIntersection.build 0.9899 a [] in
        [ x1; x2; x3; x4 ]
      in
      let comps = RTCComps.prepare (List.nth xs 2) r xs in
      let color = RTCWorld.refracted_color w comps 5 in
      assert (RTCColor.equal color (RTCColor.build 0. 0.99878 0.04724)));

    "shade_hit() with a transparent material" >::
    (fun test_ctxt ->
      let default = default_world () in
      let floor =
        let plane = RTCPlane.build () in
        let material = RTCMaterial.build ~transparency:0.5 ~refractive_index:1.5 () in
        let transform = RTCTransform.translation 0. (-1.) 0. in
        RTCShape.transform (RTCShape.texture plane material) transform
      in
      let ball =
        let sphere = RTCSphere.build () in
        let material = RTCMaterial.build ~color:(RTCColor.build 1. 0. 0.) ~ambient:0.5 () in
        let transform = RTCTransform.translation 0. (-3.5) (-0.5) in
        RTCShape.transform (RTCShape.texture sphere material) transform
      in
      let w = { default with shapes=floor :: ball :: default.shapes } in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-3.)) (RTCTuple.vector 0. (-.r2d2) r2d2) in
      let i = RTCIntersection.build r2 floor [] in
      let comps = RTCComps.prepare i r [i] in
      let color = RTCWorld.shade_hit w comps 5 in
      assert (RTCColor.equal color (RTCColor.build 0.93642 0.68642 0.68642)));

    "shade_hit() with a reflective, transparent material" >::
    (fun test_ctxt ->
      let default = default_world () in
      let floor =
        let plane = RTCPlane.build () in
        let material = RTCMaterial.build ~reflective:0.5 ~transparency:0.5 ~refractive_index:1.5 () in
        let transform = RTCTransform.translation 0. (-1.) 0. in
        RTCShape.transform (RTCShape.texture plane material) transform
      in
      let ball =
        let sphere = RTCSphere.build () in
        let material = RTCMaterial.build ~color:(RTCColor.build 1. 0. 0.) ~ambient:0.5 () in
        let transform = RTCTransform.translation 0. (-3.5) (-0.5) in
        RTCShape.transform (RTCShape.texture sphere material) transform
      in
      let w = { default with shapes=floor :: ball :: default.shapes } in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-3.)) (RTCTuple.vector 0. (-.r2d2) r2d2) in
      let i = RTCIntersection.build r2 floor [] in
      let comps = RTCComps.prepare i r [i] in
      let color = RTCWorld.shade_hit w comps 5 in
      assert (RTCColor.equal color (RTCColor.build 0.93391 0.69643 0.69243)));
  ]
