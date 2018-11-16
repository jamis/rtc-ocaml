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

    "The normal on a sphere at a point on the x axis" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let n = RTCSphere.normal_at s (RTCTuple.point 1. 0. 0.) in
      assert (RTCTuple.equal n (RTCTuple.vector 1. 0. 0.)));

    "The normal on a sphere at a point on the y axis" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let n = RTCSphere.normal_at s (RTCTuple.point 0. 1. 0.) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 1. 0.)));

    "The normal on a sphere at a point on the z axis" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let n = RTCSphere.normal_at s (RTCTuple.point 0. 0. 1.) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 0. 1.)));

    "The normal on a sphere at a non-axial point" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let r3 = sqrt(3.) /. 3. in
      let n = RTCSphere.normal_at s (RTCTuple.point r3 r3 r3) in
      assert (RTCTuple.equal n (RTCTuple.vector r3 r3 r3)));

    "The normal is a normalized vector" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let r3 = sqrt(3.) /. 3. in
      let n = RTCSphere.normal_at s (RTCTuple.point r3 r3 r3) in
      assert (RTCTuple.equal n (RTCTuple.norm n)));

    "Computing the normal on a translated sphere" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      s#set_transform (RTCTransform.translation 0. 1. 0.);
      let n = RTCSphere.normal_at s (RTCTuple.point 0. 1.70711 (-0.70711)) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 0.70711 (-0.70711))));

    "Computing the normal on a transformed sphere" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let tx = RTCMatrix.mult (RTCTransform.scaling 1. 0.5 1.) (RTCTransform.rotation_z (Float.pi /. 5.)) in
      s#set_transform tx;
      let n = RTCSphere.normal_at s (RTCTuple.point 0. (sqrt(2.)/.2.) (-.sqrt(2.)/.2.)) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 0.97014 (-0.24254))));

    "A sphere has a default material" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let m = s#material in
      assert (RTCMaterial.equal m (RTCMaterial.build ())));

    "A sphere may be assigned a material" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let m = RTCMaterial.build ~ambient:1.0 () in
      s#set_material m;
      assert (RTCMaterial.equal m s#material));
  ]
