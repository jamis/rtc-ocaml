open OUnit2

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
      let i = RTCIntersection.build 3.5 s in
      assert_equal 3.5 i.t;
      assert (s == i.shape));

    "Aggregating intersections" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = RTCIntersection.build 2. s in
      let i2 = RTCIntersection.build 1. s in
      let xs = RTCIntersection.list [i1; i2] in
      assert_equal 2 (List.length xs);
      assert_equal 1. (List.nth xs 0).t;
      assert_equal 2. (List.nth xs 1).t);

    "The hit, when all intersections have positive t" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = RTCIntersection.build 1. s in
      let i2 = RTCIntersection.build 2. s in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal (Some i1) i);

    "The hit, when some intersections have negative t" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = RTCIntersection.build (-1.) s in
      let i2 = RTCIntersection.build 1. s in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal (Some i2) i);

    "The hit, when all intersections have negative t" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = RTCIntersection.build (-2.) s in
      let i2 = RTCIntersection.build (-1.) s in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal None i);

    "The hit is always the lowest non-negative intersection" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let i1 = RTCIntersection.build 5. s in
      let i2 = RTCIntersection.build 7. s in
      let i3 = RTCIntersection.build (-3.) s in
      let i4 = RTCIntersection.build 2. s in
      let xs = RTCIntersection.list [i1; i2; i3; i4] in
      let i = RTCIntersection.hit xs in
      assert_option_intersection_equal (Some i4) i);

    "Precomputing the state of an intersection" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCSphere.build () in
      let i = RTCIntersection.build 4. shape in
      let comps = RTCComps.prepare i r in
      assert_equal i.t comps.t;
      assert (i.shape == comps.shape);
      assert (RTCTuple.equal comps.point (RTCTuple.point 0. 0. (-1.)));
      assert (RTCTuple.equal comps.eyev (RTCTuple.vector 0. 0. (-1.)));
      assert (RTCTuple.equal comps.normalv (RTCTuple.vector 0. 0. (-1.))));

    "The hit, when an intersection occurs on the outside" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCSphere.build () in
      let i = RTCIntersection.build 4. shape in
      let comps = RTCComps.prepare i r in
      assert_equal false comps.inside);

    "The hit, when an intersection occurs on the inside" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCSphere.build () in
      let i = RTCIntersection.build 1. shape in
      let comps = RTCComps.prepare i r in
      assert (RTCTuple.equal comps.point (RTCTuple.point 0. 0. 1.));
      assert (RTCTuple.equal comps.eyev (RTCTuple.vector 0. 0. (-1.)));
      assert_equal true comps.inside;
      assert (RTCTuple.equal comps.normalv (RTCTuple.vector 0. 0. (-1.))));

    "The hit should offset the point" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let shape = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 0. 0. 1.) in
      let i = RTCIntersection.build 5. shape in
      let comps = RTCComps.prepare i r in
      assert (comps.point.z < -.RTCConst.epsilon /. 2.));
  ]
