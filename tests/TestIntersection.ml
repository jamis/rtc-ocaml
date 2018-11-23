open OUnit2

let build = RTCIntersection.build

let assert_option_intersection_equal expect actual =
  match (expect, actual) with
  | (None, None) -> assert true
  | (Some a, Some b) when a == b -> assert true
  | _ -> assert false

let tests =
  "Intersections" >:::
  [
    "An intersection encapsulates t and object" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i = build 3.5 s [] in
      assert_equal 3.5 i.t;
      assert (s == i.shape));

    "Aggregating intersections" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = build 2. s [] in
      let i2 = build 1. s [] in
      let xs = RTCIntersection.list [i1; i2] in
      assert_equal 2 (List.length xs);
      assert_equal 1. (List.nth xs 0).t;
      assert_equal 2. (List.nth xs 1).t);

    "The hit, when all intersections have positive t" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = build 1. s [] in
      let i2 = build 2. s [] in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal (Some i1) i);

    "The hit, when some intersections have negative t" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = build (-1.) s [] in
      let i2 = build 1. s [] in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal (Some i2) i);

    "The hit, when all intersections have negative t" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = build (-2.) s [] in
      let i2 = build (-1.) s [] in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal None i);

    "The hit is always the lowest non-negative intersection" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = build 5. s [] in
      let i2 = build 7. s [] in
      let i3 = build (-3.) s [] in
      let i4 = build 2. s [] in
      let xs = RTCIntersection.list [i1; i2; i3; i4] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal (Some i4) i);

    "Precomputing the state of an intersection" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCSphere.build () in
      let i = build 4. shape [] in
      let comps = RTCComps.prepare i r [i] in
      assert_equal i.t comps.t;
      assert (i.shape == comps.shape);
      assert (RTCTuple.equal comps.point (RTCTuple.point 0. 0. (-1.)));
      assert (RTCTuple.equal comps.eyev (RTCTuple.vector 0. 0. (-1.)));
      assert (RTCTuple.equal comps.normalv (RTCTuple.vector 0. 0. (-1.))));

    "The hit, when an intersection occurs on the outside" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCSphere.build () in
      let i = build 4. shape [] in
      let comps = RTCComps.prepare i r [i] in
      assert_equal false comps.inside);

    "The hit, when an intersection occurs on the inside" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCSphere.build () in
      let i = build 1. shape [] in
      let comps = RTCComps.prepare i r [i] in
      assert (RTCTuple.equal comps.point (RTCTuple.point 0. 0. 1.));
      assert (RTCTuple.equal comps.eyev (RTCTuple.vector 0. 0. (-1.)));
      assert_equal true comps.inside;
      assert (RTCTuple.equal comps.normalv (RTCTuple.vector 0. 0. (-1.))));

    "The hit should offset the point" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 0. 0. 1.) in
      let i = build 5. shape [] in
      let comps = RTCComps.prepare i r [i] in
      assert (comps.point.z < -.RTCConst.epsilon /. 2.));

    "Precomputing the reflection vector" >::
    (fun test_ctxt ->
      let shape = RTCPlane.build () in
      let r = RTCRay.build (RTCTuple.point 0. 1. (-1.)) (RTCTuple.vector 0. (-.(sqrt 2.)/.2.) ((sqrt 2.)/.2.)) in
      let i = build (sqrt 2.) shape [] in
      let comps = RTCComps.prepare i r [i] in
      assert (RTCTuple.equal comps.reflectv (RTCTuple.vector 0. ((sqrt 2.)/.2.) ((sqrt 2.)/.2.))));

    "Finding n1 and n2 at various intersections" >::
    (fun test_ctxt ->
      let a =
        let sphere = TestSphere.glass_sphere () in
        let transform = RTCTransform.scaling 2. 2. 2. in
        let material = { sphere.material with refractive_index=1.5 } in
        RTCShape.transform (RTCShape.texture sphere material) transform
      in
      let b =
        let sphere = TestSphere.glass_sphere () in
        let transform = RTCTransform.translation 0. 0. (-0.25) in
        let material = { sphere.material with refractive_index=2. } in
        RTCShape.transform (RTCShape.texture sphere material) transform
      in
      let c =
        let sphere = TestSphere.glass_sphere () in
        let transform = RTCTransform.translation 0. 0. 0.25 in
        let material = { sphere.material with refractive_index=2.5 } in
        RTCShape.transform (RTCShape.texture sphere material) transform
      in
      let r = RTCRay.build (RTCTuple.point 0. 0. (-4.)) (RTCTuple.vector 0. 0. 1.) in
      let xs =
        let x0 = build 2. a [] in
        let x1 = build 2.75 b [] in
        let x2 = build 3.25 c [] in
        let x3 = build 4.75 b [] in
        let x4 = build 5.25 c [] in
        let x5 = build 6. a [] in
        RTCIntersection.list [x0; x1; x2; x3; x4; x5]
      in
      let expect = [(0, 1.0, 1.5);
                    (1, 1.5, 2.0);
                    (2, 2.0, 2.5);
                    (3, 2.5, 2.5);
                    (4, 2.5, 1.5);
                    (5, 1.5, 1.0)]
      in
      let rec loop expect xrest = match expect with
        | [] -> ()
        | (index, n1, n2) :: rest ->
          match xrest with
          | [] -> ()
          | x :: xrest ->
            let comps = RTCComps.prepare x r xs in
            assert_equal ~msg:(Printf.sprintf "n1:%f != %f" n1 comps.n1) n1 comps.n1;
            assert_equal ~msg:(Printf.sprintf "n2:%f != %f" n2 comps.n2) n2 comps.n2;
            loop rest xrest
      in
      loop expect xs);

    "The under point is offset below the surface" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCShape.transform (TestSphere.glass_sphere ()) (RTCTransform.translation 0. 0. 1.) in
      let i = build 5. shape [] in
      let comps = RTCComps.prepare i r [i] in
      assert (comps.under_point.z > RTCConst.epsilon /. 2.));

    "The Schlick approximation under total internal reflection" >::
    (fun test_ctxt ->
      let shape = TestSphere.glass_sphere () in
      let r = RTCRay.build (RTCTuple.point 0. 0. (sqrt(2.)/.2.)) (RTCTuple.vector 0. 1. 0.) in
      let xs =
        let x1 = build (-.sqrt(2.)/.2.) shape [] in
        let x2 = build (sqrt(2.)/.2.) shape [] in
        [ x1; x2 ]
      in
      let comps = RTCComps.prepare (List.nth xs 1) r xs in
      assert_equal 1. (RTCComps.schlick comps));

    "The Schlick approximation with a perpendicular viewing angle" >::
    (fun test_ctxt ->
      let shape = TestSphere.glass_sphere () in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 1. 0.) in
      let xs =
        let x1 = build (-1.) shape [] in
        let x2 = build 1. shape [] in
        [ x1; x2 ]
      in
      let comps = RTCComps.prepare (List.nth xs 1) r xs in
      let reflectance = RTCComps.schlick comps in
      assert ((abs_float (reflectance -. 0.04)) < RTCConst.epsilon));

    "The Schlick approximation with small angle and n2 > n1" >::
    (fun test_ctxt ->
      let shape = TestSphere.glass_sphere () in
      let r = RTCRay.build (RTCTuple.point 0. 0.99 (-2.)) (RTCTuple.vector 0. 0. 1.) in
      let xs = [ build 1.8589 shape [] ] in
      let comps = RTCComps.prepare (List.nth xs 0) r xs in
      let reflectance = RTCComps.schlick comps in
      assert ((abs_float (reflectance -. 0.48873)) < RTCConst.epsilon));
  ]
