open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector

let tests =
  "Cones" >:::
  [
    "Intersecting a cone with a ray" >::
    (fun test_ctxt ->
      let c = RTCCone.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (origin, direction, t0, t1) :: scenarios ->
          let r = RTCRay.build origin (RTCTuple.norm direction) in
          let xs = c.local_intersect c r in
          assert_equal 2 (List.length xs);
          assert_in_epsilon t0 (List.nth xs 0).t;
          assert_in_epsilon t1 (List.nth xs 1).t;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 0. 0. (-5.), vector 0. 0. 1., 5., 5. );
        ( point 0. 0. (-5.), vector 1. 1. 1., 8.66025, 8.66025 );
        ( point 1. 1. (-5.), vector (-0.5) (-1.) 1., 4.55006, 49.44994 ) ]);

    "Intersecting a cone with a ray parallel to one of its halves" >::
    (fun test_ctxt ->
      let shape = RTCCone.build () in
      let r = RTCRay.build (point 0. 0. (-1.)) (RTCTuple.norm (vector 0. 1. 1.)) in
      let xs = shape.local_intersect shape r in
      assert_equal 1 (List.length xs);
      assert_in_epsilon 0.35355 (List.nth xs 0).t);

    "Intersecting a cone's end caps" >::
    (fun test_ctxt ->
      let c = RTCCone.build ~minimum:(-0.5) ~maximum:0.5 ~closed:true () in
      let rec run_scenarios = function
        | [] -> ()
        | (origin, direction, count) :: scenarios ->
          let r = RTCRay.build origin (RTCTuple.norm direction) in
          let xs = c.local_intersect c r in
          assert_equal count (List.length xs);
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 0. 0. (-5.00), vector 0. 1. 0., 0 );
        ( point 0. 0. (-0.25), vector 0. 1. 1., 2 );
        ( point 0. 0. (-0.25), vector 0. 1. 0., 4 ) ]);

    "Computing the normal vector on a cone" >::
    (fun test_ctxt ->
      let c = RTCCone.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (point, normal) :: scenarios ->
          let n = c.local_normal_at c point in
          assert_tuple_equal n normal;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 0. 0. 0., vector 0. 0. 0. );
        ( point 1. 1. 1., vector 1. (-.sqrt(2.)) 1. );
        ( point (-1.) (-1.) 0., vector (-1.) 1. 0. ) ]);
  ]
