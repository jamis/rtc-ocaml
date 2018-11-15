open OUnit2

let tests =
  "Spheres" >:::
  [
    "A ray intersects a sphere at two points" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      let xs = RTCSphere.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal 4. (List.nth xs 0).t;
      assert_equal 6. (List.nth xs 1).t);

    "A ray intersects a sphere at a tangent" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 1. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      let xs = RTCSphere.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal 5. (List.nth xs 0).t;
      assert_equal 5. (List.nth xs 1).t);

    "A ray misses a sphere" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 2. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      let xs = RTCSphere.intersect s r in
      assert_equal 0 (List.length xs));

    "A ray originates inside a sphere" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 0.) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      let xs = RTCSphere.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal (-1.) (List.nth xs 0).t;
      assert_equal 1. (List.nth xs 1).t);

    "A sphere is behind a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. 5.) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      let xs = RTCSphere.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal (-6.) (List.nth xs 0).t;
      assert_equal (-4.) (List.nth xs 1).t);

    "Intersect sets the object on the intersection" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      let xs = RTCSphere.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal s (List.nth xs 0).shape;
      assert_equal s (List.nth xs 1).shape);

    "A sphere's default transformation" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      assert (RTCMatrix.equal s#transform RTCMatrix.identity));

    "Changing a sphere's transformation" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let t = RTCTransform.translation 2. 3. 4. in
      s#set_transform t;
      assert (RTCMatrix.equal s#transform t));

    "Intersecting a scaled sphere with a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      s#set_transform (RTCTransform.scaling 2. 2. 2.);
      let xs = RTCSphere.intersect s r in
      assert_equal 2 (List.length xs);
      assert_equal 3. (List.nth xs 0).t;
      assert_equal 7. (List.nth xs 1).t);

    "Intersecting a translated sphere with a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = new RTCSphere.shape in
      s#set_transform (RTCTransform.translation 5. 0. 0.);
      let xs = RTCSphere.intersect s r in
      assert_equal 0 (List.length xs));
  ]
