open OUnit2

let tests =
  "Spheres" >:::
  [
    "A ray intersects a sphere at two points" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCSphere.build () in
      let xs = RTCShape.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal 4. (List.nth xs 0).t;
      assert_equal 6. (List.nth xs 1).t);

    "A ray intersects a sphere at a tangent" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 1. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCSphere.build () in
      let xs = RTCShape.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal 5. (List.nth xs 0).t;
      assert_equal 5. (List.nth xs 1).t);

    "A ray misses a sphere" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 2. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCSphere.build () in
      let xs = RTCShape.intersect s r in
      assert_equal 0 (List.length xs));

    "A ray originates inside a sphere" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCSphere.build () in
      let xs = RTCShape.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal (-1.) (List.nth xs 0).t;
      assert_equal 1. (List.nth xs 1).t);

    "A sphere is behind a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 5.) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCSphere.build () in
      let xs = RTCShape.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal (-6.) (List.nth xs 0).t;
      assert_equal (-4.) (List.nth xs 1).t);

    "Intersect sets the object on the intersection" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCSphere.build () in
      let xs = RTCShape.intersect s r in
      assert_equal 2 (List.length xs);
      assert (s == (List.nth xs 0).shape);
      assert (s == (List.nth xs 1).shape));

    "Intersecting a scaled sphere with a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCShape.transform (RTCSphere.build ()) (RTCTransform.scaling 2. 2. 2.) in
      let xs = RTCShape.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal 3. (List.nth xs 0).t;
      assert_equal 7. (List.nth xs 1).t);

    "Intersecting a translated sphere with a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 5. 0. 0.) in
      let xs = RTCShape.intersect s r in
      assert_equal 0 (List.length xs));

    "The normal on a sphere at a point on the x axis" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let n = RTCShape.normal_at s (RTCTuple.point 1. 0. 0.) in
      assert (RTCTuple.equal n (RTCTuple.vector 1. 0. 0.)));

    "The normal on a sphere at a point on the y axis" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let n = RTCShape.normal_at s (RTCTuple.point 0. 1. 0.) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 1. 0.)));

    "The normal on a sphere at a point on the z axis" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let n = RTCShape.normal_at s (RTCTuple.point 0. 0. 1.) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 0. 1.)));

    "The normal on a sphere at a non-axial point" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let r3 = sqrt(3.) /. 3. in
      let n = RTCShape.normal_at s (RTCTuple.point r3 r3 r3) in
      assert (RTCTuple.equal n (RTCTuple.vector r3 r3 r3)));

    "The normal is a normalized vector" >::
    (fun test_ctxt ->
      let s = RTCSphere.build () in
      let r3 = sqrt(3.) /. 3. in
      let n = RTCShape.normal_at s (RTCTuple.point r3 r3 r3) in
      assert (RTCTuple.equal n (RTCTuple.norm n)));
  ]
