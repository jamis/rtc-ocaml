open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector

let setup_smooth_triangle name fn =
  let p1 = point 0. 1. 0. and p2 = point (-1.) 0. 0. and p3 = point 1. 0. 0.
  and n1 = vector 0. 1. 0. and n2 = vector (-1.) 0. 0. and n3 = vector 1. 0. 0. in
  let tri = RTCTriangle.smooth p1 p2 p3 n1 n2 n3 in
  name >:: (fn tri (p1, p2, p3) (n1, n2, n3))

let tests =
  "Triangles" >:::
  [
    "Constructing a triangle" >::
    (fun test_ctxt ->
      let p1 = point 0. 1. 0. in
      let p2 = point (-1.) 0. 0. in
      let p3 = point 1. 0. 0. in
      let tri = RTCTriangle.build p1 p2 p3 in
      let data = RTCTriangle.data tri in
      assert_tuple_equal data.p1 p1;
      assert_tuple_equal data.p2 p2;
      assert_tuple_equal data.p3 p3;
      assert_tuple_equal data.e1 (vector (-1.) (-1.) 0.);
      assert_tuple_equal data.e2 (vector (1.) (-1.) 0.);
      assert_tuple_equal data.normal (vector 0. 0. (-1.)));

    "Finding the normal on a triangle" >::
    (fun test_ctxt ->
      let tri = RTCTriangle.build (point 0. 1. 0.) (point (-1.) 0. 0.) (point 1. 0. 0.) in
      let data = RTCTriangle.data tri in
      let n1 = tri.local_normal_at tri (point 0. 0.5 0.) in
      let n2 = tri.local_normal_at tri (point (-0.5) 0.75 0.) in
      let n3 = tri.local_normal_at tri (point 0.5 0.25 0.) in
      assert_tuple_equal n1 data.normal;
      assert_tuple_equal n2 data.normal;
      assert_tuple_equal n3 data.normal);

    "Intersecting a ray parallel to the triangle" >::
    (fun test_ctxt ->
      let tri = RTCTriangle.build (point 0. 1. 0.) (point (-1.) 0. 0.) (point 1. 0. 0.) in
      let r = RTCRay.build (point 0. (-1.) (-2.)) (vector 0. 1. 0.) in
      let xs = tri.local_intersect tri ~trail:[] r in
      assert_equal 0 (List.length xs));

    "A ray misses the p1-p3 edge" >::
    (fun test_ctxt ->
      let tri = RTCTriangle.build (point 0. 1. 0.) (point (-1.) 0. 0.) (point 1. 0. 0.) in
      let r = RTCRay.build (point 1. 1. (-2.)) (vector 0. 0. 1.) in
      let xs = tri.local_intersect tri ~trail:[] r in
      assert_equal 0 (List.length xs));

    "A ray misses the p1-p2 edge" >::
    (fun test_ctxt ->
      let tri = RTCTriangle.build (point 0. 1. 0.) (point (-1.) 0. 0.) (point 1. 0. 0.) in
      let r = RTCRay.build (point (-1.) 1. (-2.)) (vector 0. 0. 1.) in
      let xs = tri.local_intersect tri ~trail:[] r in
      assert_equal 0 (List.length xs));

    "A ray misses the p2-p3 edge" >::
    (fun test_ctxt ->
      let tri = RTCTriangle.build (point 0. 1. 0.) (point (-1.) 0. 0.) (point 1. 0. 0.) in
      let r = RTCRay.build (point 0. (-1.) (-2.)) (vector 0. 0. 1.) in
      let xs = tri.local_intersect tri ~trail:[] r in
      assert_equal 0 (List.length xs));

    "A ray strikes a triangle" >::
    (fun test_ctxt ->
      let tri = RTCTriangle.build (point 0. 1. 0.) (point (-1.) 0. 0.) (point 1. 0. 0.) in
      let r = RTCRay.build (point 0. 0.5 (-2.)) (vector 0. 0. 1.) in
      let xs = tri.local_intersect tri ~trail:[] r in
      assert_equal 1 (List.length xs);
      assert_in_epsilon 2. (List.nth xs 0).t);

    setup_smooth_triangle "Constructing a smooth triangle"
    (fun tri ps ns test_ctxt ->
      let (p1, p2, p3) = ps in
      let (n1, n2, n3) = ns in
      let data = RTCTriangle.data tri in
      assert_tuple_equal data.p1 p1;
      assert_tuple_equal data.p2 p2;
      assert_tuple_equal data.p3 p3;
      assert_tuple_equal data.n1 n1;
      assert_tuple_equal data.n2 n2;
      assert_tuple_equal data.n3 n3);

    setup_smooth_triangle "An intersection with a smooth triangle stores u/v"
    (fun tri _ _ test_ctxt ->
      let r = RTCRay.build (point (-0.2) 0.3 (-2.)) (vector 0. 0. 1.) in
      let xs = tri.local_intersect tri r in
      assert_in_epsilon 0.45 (List.nth xs 0).u;
      assert_in_epsilon 0.25 (List.nth xs 0).v);

    setup_smooth_triangle "A smooth triangle uses u/v to interpolate the normal"
    (fun tri _ _ test_ctxt ->
      let i = RTCIntersection.build ~u:0.45 ~v:0.25 1. tri [] in
      let n = RTCShape.normal_at ~hit:(Some i) tri [] (point 0. 0. 0.) in
      assert_tuple_equal n (vector (-0.5547) 0.83205 0.));

    setup_smooth_triangle "Preparing the normal on a smooth triangle"
    (fun tri _ _ test_ctxt ->
      let i = RTCIntersection.build ~u:0.45 ~v:0.25 1. tri [] in
      let r = RTCRay.build (point (-0.2) 0.3 (-2.)) (vector 0. 0. 1.) in
      let comps = RTCComps.prepare i r [i] in
      assert_tuple_equal comps.normalv (vector (-0.5547) 0.83205 0.));
  ]
