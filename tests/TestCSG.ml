open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector
let x t s = RTCIntersection.build t s []

let evaluate_rule table fn =
  let rec loop = function
    | [] -> ()
    | (lhit, inl, inr, expect) :: rest ->
      let result = fn lhit inl inr in
      assert_equal expect result;
      loop rest
  in
  loop table

let tests =
  "CSG" >:::
  [
    "CSG is created with an operation and two shapes" >::
    (fun test_ctxt ->
      let s1 = RTCSphere.build () and s2 = RTCCube.build () in
      let c = RTCCSG.union s1 s2 in
      match c.shape with
      | Union (left, right, _) ->
        assert_equal ~cmp:(==) left s1;
        assert_equal ~cmp:(==) right s2
      | _ -> assert_failure "expected a union of s1,s2");

    "Evaluating the rule for a CSG union operation" >::
    (fun test_ctxt ->
      let expects = [
        ( true,  true,  true,  false );
        ( true,  true,  false, true  );
        ( true,  false, true,  false );
        ( true,  false, false, true  );
        ( false, true,  true,  false );
        ( false, true,  false, false );
        ( false, false, true,  true  );
        ( false, false, false, true  ) ]
      in
      evaluate_rule expects RTCCSG.intersection_allowed_union);

    "Evaluating the rule for a CSG intersect operation" >::
    (fun test_ctxt ->
      let expects = [
        ( true,  true,  true,  true  );
        ( true,  true,  false, false );
        ( true,  false, true,  true  );
        ( true,  false, false, false );
        ( false, true,  true,  true  );
        ( false, true,  false, true  );
        ( false, false, true,  false );
        ( false, false, false, false ) ]
      in
      evaluate_rule expects RTCCSG.intersection_allowed_intersect);

    "Evaluating the rule for a CSG difference operation" >::
    (fun test_ctxt ->
      let expects = [
        ( true,  true,  true,  false );
        ( true,  true,  false, true  );
        ( true,  false, true,  false );
        ( true,  false, false, true  );
        ( false, true,  true,  true  );
        ( false, true,  false, true  );
        ( false, false, true,  false );
        ( false, false, false, false ) ]
      in
      evaluate_rule expects RTCCSG.intersection_allowed_difference);

    "Filtering a list of intersections" >::
    (fun test_ctxt ->
      let expects = [
        ( RTCCSG.union, 0, 3 );
        ( RTCCSG.intersection, 1, 2);
        ( RTCCSG.difference, 0, 1) ]
      in
      let s1 = RTCSphere.build () in
      let s2 = RTCCube.build () in
      let xs = [ x 1. s1; x 2. s2; x 3. s1; x 4. s2 ] in
      let rec loop = function
        | [] -> ()
        | (op, x0, x1) :: rest ->
          let c = op s1 s2 in
          let result = RTCCSG.filter_intersections c xs in
          assert_equal 2 (List.length result);
          assert_equal ~cmp:(==) (List.nth result 0) (List.nth xs x0);
          assert_equal ~cmp:(==) (List.nth result 1) (List.nth xs x1);
          loop rest
      in
      loop expects);

      "A ray misses a CSG object" >::
      (fun test_ctxt ->
        let c = RTCCSG.union (RTCSphere.build ()) (RTCCube.build ()) in
        let r = RTCRay.build (point 0. 2. (-5.)) (vector 0. 0. 1.) in
        let xs = c.local_intersect c r in
        assert_equal 0 (List.length xs));

      "A ray hits a CSG object" >::
      (fun test_ctxt ->
        let s1 = RTCSphere.build ()
        and s2 = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 0. 0. 0.5) in
        let c = RTCCSG.union s1 s2 in
        let r = RTCRay.build (point 0. 0. (-5.)) (vector 0. 0. 1.) in
        let xs = c.local_intersect c r in
        assert_equal 2 (List.length xs);
        assert_in_epsilon 4. (List.nth xs 0).t;
        assert_equal ~cmp:(==) s1 (List.nth xs 0).shape;
        assert_in_epsilon 6.5 (List.nth xs 1).t;
        assert_equal ~cmp:(==) s2 (List.nth xs 1).shape);
  ]
