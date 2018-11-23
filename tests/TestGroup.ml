open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector

let tests =
  "Groups" >:::
  [
    "Creating a new group" >::
    (fun test_ctxt ->
      let g = RTCGroup.build () in
      assert (RTCMatrix.equal g.transform RTCMatrix.identity);
      assert (RTCGroup.is_empty g));

    "Adding a child to a group" >::
    (fun test_ctxt ->
      let s = TestShape.test_shape () in
      let g = RTCGroup.build ~children:[s] () in
      assert (not (RTCGroup.is_empty g));
      assert (List.mem s (RTCGroup.get_children g)));

    "Intersecting a ray with an empty group" >::
    (fun test_ctxt ->
      let g = RTCGroup.build () in
      let r = RTCRay.build (point 0. 0. 0.) (vector 0. 0. 1.) in
      let xs = RTCShape.intersect g r in
      assert_equal 0 (List.length xs));

    "Intersecting a ray with a non-empty group" >::
    (fun test_ctxt ->
      let s1 = RTCSphere.build () in
      let s2 =
        let sphere = RTCSphere.build () in
        let transform = RTCTransform.translation 0. 0. (-3.) in
        RTCShape.transform sphere transform
      in
      let s3 =
        let sphere = RTCSphere.build () in
        let transform = RTCTransform.translation 5. 0. 0. in
        RTCShape.transform sphere transform
      in
      let g = RTCGroup.build ~children:[s1; s2; s3] () in
      let r = RTCRay.build (point 0. 0. (-5.)) (vector 0. 0. 1.) in
      let xs = RTCShape.intersect g r in
      assert_equal 4 (List.length xs);
      assert_equal ~cmp:(==) s2 (List.nth xs 0).shape;
      assert_equal 1 (List.length (List.nth xs 0).trail);
      assert_equal ~cmp:(==) s2 (List.nth xs 1).shape;
      assert_equal ~cmp:(==) s1 (List.nth xs 2).shape;
      assert_equal ~cmp:(==) s1 (List.nth xs 3).shape);

    "Intersecting a transformed group" >::
    (fun test_ctxt ->
      let s =
        let sphere = RTCSphere.build () in
        let transform = RTCTransform.translation 5. 0. 0. in
        RTCShape.transform sphere transform
      in
      let g =
        let group = RTCGroup.build ~children:[s] () in
        let transform = RTCTransform.scaling 2. 2. 2. in
        RTCShape.transform group transform
      in
      let r = RTCRay.build (point 10. 0. (-10.)) (vector 0. 0. 1.) in
      let xs = RTCShape.intersect g r in
      assert_equal 2 (List.length xs));
  ]
