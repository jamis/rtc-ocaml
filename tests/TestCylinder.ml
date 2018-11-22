open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector

let tests =
  "Cylinders" >:::
  [
    "A ray misses a cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (origin, direction) :: scenarios ->
          let r = RTCRay.build origin (RTCTuple.norm direction) in
          let xs = c.local_intersect c r in
          assert_equal 0 (List.length xs);
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 1. 0. 0.,     vector 0. 1. 0. );
        ( point 0. 0. 0.,     vector 0. 1. 0. );
        ( point 0. 0. (-5.),  vector 1. 1. 1. ) ]);

    "A ray strikes a cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (origin, direction, t1, t2) :: scenarios ->
          let r = RTCRay.build origin (RTCTuple.norm direction) in
          let xs = c.local_intersect c r in
          assert_equal 2 (List.length xs);
          assert_in_epsilon t1 (List.nth xs 0).t;
          assert_in_epsilon t2 (List.nth xs 1).t;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 1.  0. (-5.),  vector 0. 0. 1.,  5., 5. );
        ( point 0.  0. (-5.),  vector 0. 0. 1.,  4., 6. );
        ( point 0.5 0. (-5.),  vector 0.1 1. 1., 6.80798, 7.08872 ) ]);

    "Normal vector on a cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build () in
      let rec run_scenarios = function
        | [] -> ()
        | (point, normal) :: scenarios ->
          let n = c.local_normal_at c point in
          assert_tuple_equal n normal;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point   1.    0.    0.,  vector   1.  0.   0. );
        ( point   0.    5.  (-1.), vector   0.  0. (-1.) );
        ( point   0.  (-2.)   1.,  vector   0.  0.   1. );
        ( point (-1.)   1.    0.,  vector (-1.) 0.   0. ) ]);

    "The default minimum and maximum for a cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build () in
      match c.shape with
      | Cylinder (minimum, maximum, _) ->
        assert_equal Float.neg_infinity minimum;
        assert_equal Float.infinity maximum
      | _ -> failwith "expected a cylinder");

    "The default closed value for a cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build () in
      match c.shape with
      | Cylinder (_, _, closed) -> assert (not closed)
      | _ -> failwith "expected a cylinder");

    "Intersecting a constrained cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build ~minimum:1. ~maximum:2. () in
      let rec run_scenarios = function
        | [] -> ()
        | (point, direction, count) :: scenarios ->
          let r = RTCRay.build point (RTCTuple.norm direction) in
          let xs = c.local_intersect c r in
          assert_equal count (List.length xs);
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 0. 1.5   0.,  vector 0.1 1. 0., 0 );
        ( point 0. 3.  (-5.), vector 0.  0. 1., 0 );
        ( point 0. 0.  (-5.), vector 0.  0. 1., 0 );
        ( point 0. 2.  (-5.), vector 0.  0. 1., 0 );
        ( point 0. 1.  (-5.), vector 0.  0. 1., 0 );
        ( point 0. 1.5 (-2.), vector 0.  0. 1., 2 ) ]);

    "Intersecting the caps of a closed cylinder" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build ~minimum:1. ~maximum:2. ~closed:true () in
      let rec run_scenarios = function
        | [] -> ()
        | (point, direction, count) :: scenarios ->
          let r = RTCRay.build point (RTCTuple.norm direction) in
          let xs = c.local_intersect c r in
          assert_equal count (List.length xs);
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 0.   3.    0.,  vector 0. (-1.) 0., 2 );
        ( point 0.   3.  (-2.), vector 0. (-1.) 2., 2 );
        ( point 0.   4.  (-2.), vector 0. (-1.) 1., 2 );
        ( point 0.   0.  (-2.), vector 0.   1.  2., 2 );
        ( point 0. (-1.) (-2.), vector 0.   1.  1., 2 ) ]);

    "The normal vector on a cylinder's end caps" >::
    (fun test_ctxt ->
      let c = RTCCylinder.build ~minimum:1. ~maximum:2. ~closed:true () in
      let rec run_scenarios = function
        | [] -> ()
        | (point, normal) :: scenarios ->
          let n = c.local_normal_at c point in
          assert_tuple_equal n normal;
          run_scenarios scenarios
      in
      run_scenarios [
        ( point 0.  1. 0.,  vector 0. (-1.) 0. );
        ( point 0.5 1. 0.,  vector 0. (-1.) 0. );
        ( point 0.  1. 0.5, vector 0. (-1.) 0. );
        ( point 0.  2. 0.,  vector 0.   1.  0. );
        ( point 0.5 2. 0.,  vector 0.   1.  0. );
        ( point 0.  2. 0.5, vector 0.   1.  0. ) ]);
  ]
