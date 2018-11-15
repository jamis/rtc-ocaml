open OUnit2

let tests =
  "Intersections" >:::
  [
    "An intersection encapsulates t and object" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let i = RTCIntersection.build 3.5 s in
      assert_equal 3.5 i.t;
      assert_equal s i.shape);

    "Aggregating intersections" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let i1 = RTCIntersection.build 2. s in
      let i2 = RTCIntersection.build 1. s in
      let xs = RTCIntersection.list [i1; i2] in
      assert_equal 2 (List.length xs);
      assert_equal 1. (List.nth xs 0).t;
      assert_equal 2. (List.nth xs 1).t);

    "The hit, when all intersections have positive t" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let i1 = RTCIntersection.build 1. s in
      let i2 = RTCIntersection.build 2. s in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_equal (Some i1) i);

    "The hit, when some intersections have negative t" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let i1 = RTCIntersection.build (-1.) s in
      let i2 = RTCIntersection.build 1. s in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_equal (Some i2) i);

    "The hit, when all intersections have negative t" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let i1 = RTCIntersection.build (-2.) s in
      let i2 = RTCIntersection.build (-1.) s in
      let xs = RTCIntersection.list [i1; i2] in
      let i = RTCIntersection.hit xs in
      assert_equal None i);

    "The hit is always the lowest non-negative intersection" >::
    (fun test_ctxt ->
      let s = new RTCSphere.shape in
      let i1 = RTCIntersection.build 5. s in
      let i2 = RTCIntersection.build 7. s in
      let i3 = RTCIntersection.build (-3.) s in
      let i4 = RTCIntersection.build 2. s in
      let xs = RTCIntersection.list [i1; i2; i3; i4] in
      let i = RTCIntersection.hit xs in
      assert_equal (Some i4) i);
  ]
