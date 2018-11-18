open OUnit2

let tests =
  "Planes" >:::
  [
    "The normal of a plane is constant everywhere" >::
    (fun test_ctxt ->
      let p = RTCPlane.build () in
      let n1 = p.local_normal_at p (RTCTuple.point 0. 0. 0.) in
      let n2 = p.local_normal_at p (RTCTuple.point 10. 0. (-10.)) in
      let n3 = p.local_normal_at p (RTCTuple.point (-5.) 0. 150.) in
      let expect = RTCTuple.vector 0. 1. 0. in
      assert (RTCTuple.equal n1 expect);
      assert (RTCTuple.equal n2 expect);
      assert (RTCTuple.equal n3 expect));

    "Intersect with a ray parallel to the plane" >::
    (fun test_ctxt ->
      let p = RTCPlane.build () in
      let r = RTCRay.build (RTCTuple.point 0. 10. 0.) (RTCTuple.vector 0. 0. 1.) in
      let xs = p.local_intersect p r in
      assert_equal 0 (List.length xs));

    "Intersect with a coplanar ray" >::
    (fun test_ctxt ->
      let p = RTCPlane.build () in
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let xs = p.local_intersect p r in
      assert_equal 0 (List.length xs));

    "A ray intersecting a plane from above" >::
    (fun test_ctxt ->
      let p = RTCPlane.build () in
      let r = RTCRay.build (RTCTuple.point 0. 1. 0.) (RTCTuple.vector 0. (-1.) 0.) in
      let xs = p.local_intersect p r in
      assert_equal 1 (List.length xs);
      assert_equal 1. (List.nth xs 0).t;
      assert (p == (List.nth xs 0).shape));

    "A ray intersecting a plane from below" >::
    (fun test_ctxt ->
      let p = RTCPlane.build () in
      let r = RTCRay.build (RTCTuple.point 0. (-1.) 0.) (RTCTuple.vector 0. 1. 0.) in
      let xs = p.local_intersect p r in
      assert_equal 1 (List.length xs);
      assert_equal 1. (List.nth xs 0).t;
      assert (p == (List.nth xs 0).shape));
  ]
