open OUnit2

let default_world () =
  let s1 = new RTCSphere.shape in
  let m1 = RTCMaterial.build ~color:(RTCColor.build 0.8 1.0 0.6)
                             ~diffuse:0.7
                             ~specular:0.2
                             ()
  in
  s1#set_material m1;
  let s2 = new RTCSphere.shape in
  s2#set_transform (RTCTransform.scaling 0.5 0.5 0.5);
  let light = RTCLight.point (RTCTuple.point (-10.) 10. (-10.)) (RTCColor.build 1. 1. 1.) in
  RTCWorld.build ~shapes:[s1; s2] ~lights:[light] ()

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
      assert_equal m1 s1#material;
      assert_equal m2 s2#material;
      assert_equal RTCMatrix.identity s1#transform;
      assert_equal t2 s2#transform);

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
      let i = RTCIntersection.build 4. shape in
      let comps = RTCComps.prepare i r in
      let c = RTCWorld.shade_hit w comps in
      assert (RTCColor.equal c (RTCColor.build 0.38066 0.47583 0.2855)));

    "Shading an intersection from the inside" >::
    (fun test_ctx ->
      let dw = default_world () in
      let w = { dw with lights=[ RTCLight.point (RTCTuple.point 0. 0.25 0.) (RTCColor.build 1. 1. 1.) ] } in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = List.nth w.shapes 1 in
      let i = RTCIntersection.build 0.5 shape in
      let comps = RTCComps.prepare i r in
      let c = RTCWorld.shade_hit w comps in
      assert (RTCColor.equal c (RTCColor.build 0.90498 0.90498 0.90498)));

    "The color when a ray misses" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 1. 0.) in
      let c = RTCWorld.color_at w r in
      assert (RTCColor.equal c RTCColor.black));

    "The color when a ray hits" >::
    (fun test_ctx ->
      let w = default_world () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let c = RTCWorld.color_at w r in
      assert (RTCColor.equal c (RTCColor.build 0.38066 0.47583 0.2855)));

    "The color with an intersection behind the ray" >::
    (fun test_ctx ->
      let w = default_world () in
      let outer = (List.nth w.shapes 0) in
      outer#set_material { outer#material with ambient=1.0 };
      let inner = (List.nth w.shapes 1) in
      inner#set_material { inner#material with ambient=1.0 };
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.75) (RTCTuple.vector 0. 0. (-1.)) in
      let c = RTCWorld.color_at w r in
      assert (RTCColor.equal c inner#material.color));
  ]
