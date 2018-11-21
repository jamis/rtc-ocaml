open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector

let tests =
  "Cubes" >:::
  [
    "A ray intersects a sphere at two points" >::
    (fun test_ctxt ->
      let c = RTCCube.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (origin, direction, t1, t2) :: scenarios ->
          let r = RTCRay.build origin direction in
          let xs = c.local_intersect c r in
          assert_equal 2 (List.length xs);
          assert_in_epsilon t1 (List.nth xs 0).t;
          assert_in_epsilon t2 (List.nth xs 1).t;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 5. 0.5 0.,    vector (-1.) 0. 0., 4., 6. );
        ( point (-5.) 0.5 0., vector 1. 0. 0.,    4., 6. );
        ( point 0.5 5. 0.,    vector 0. (-1.) 0., 4., 6. );
        ( point 0.5 (-5.) 0., vector 0. 1. 0.,    4., 6. );
        ( point 0.5 0. 5.,    vector 0. 0. (-1.), 4., 6. );
        ( point 0.5 0. (-5.), vector 0. 0. 1.,    4., 6. );
        ( point 0. 0.5 0.,    vector 0. 0. 1.,  (-1.), 1. ) ]);

    "A ray misses a cube" >::
    (fun test_ctxt ->
      let c = RTCCube.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (origin, direction) :: scenarios ->
          let r = RTCRay.build origin direction in
          let xs = c.local_intersect c r in
          assert_equal 0 (List.length xs);
          run_scenarios scenarios
      in
      run_scenarios [
        ( point (-2.) 0. 0., vector 0.2673 0.5345 0.8018 );
        ( point 0. (-2.) 0., vector 0.8018 0.2673 0.5345 );
        ( point 0. 0. (-2.), vector 0.5345 0.8018 0.2673 );
        ( point 2. 0. 2.,    vector 0. 0. (-1.) );
        ( point 0. 2. 2.,    vector 0. (-1.) 0. );
        ( point 2. 2. 0.,    vector (-1.) 0. 0. ) ]);

    "The normal on the surface of a cube" >::
    (fun test_ctxt ->
      let c = RTCCube.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (point, expect) :: scenarios ->
          let normal = c.local_normal_at c point in
          assert_tuple_equal expect normal;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point   1.     0.5  (-0.8), vector 1. 0. 0. );
        ( point (-1.)  (-0.2)   0.9,  vector (-1.) 0. 0. );
        ( point (-0.4)   1.   (-0.1), vector 0. 1. 0. );
        ( point   0.3  (-1.)  (-0.7), vector 0. (-1.) 0. );
        ( point (-0.6)   0.3    1.,   vector 0. 0. 1. );
        ( point   0.4    0.4  (-1.),  vector 0. 0. (-1.) );
        ( point   1.     1.     1.,   vector 1. 0. 0. );
        ( point (-1.)  (-1.)  (-1.),  vector (-1.) 0. 0. ) ]);
  ]
