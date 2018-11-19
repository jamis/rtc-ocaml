open OUnit2

let test_pattern () =
  let fn (point:RTCTuple.t) = RTCPattern.Solid (RTCColor.build point.x point.y point.z) in
  RTCPattern.build fn

let white_pattern = RTCPattern.Solid RTCColor.white
let black_pattern = RTCPattern.Solid RTCColor.black

let tests =
  "Patterns" >:::
  [
    "The default pattern transformation" >::
    (fun test_ctxt ->
      let pattern = test_pattern () in
      assert (RTCMatrix.equal pattern.transform RTCMatrix.identity));

    "Assigning a transformation" >::
    (fun test_ctxt ->
      let pattern' = test_pattern () in
      let tx = RTCTransform.translation 1. 2. 3. in
      let pattern = RTCPattern.transform pattern' tx in
      assert (RTCMatrix.equal pattern.transform tx);
      assert (RTCMatrix.equal pattern.inverse_transform (RTCMatrix.inverse tx)));

    "A pattern with an object transformation" >::
    (fun test_ctxt ->
      let shape = RTCShape.transform (RTCSphere.build ()) (RTCTransform.scaling 2. 2. 2.) in
      let pattern = test_pattern () in
      let expect = RTCColor.build 1. 1.5 2. in
      assert (RTCColor.equal expect (RTCPattern.at_object pattern (RTCShape.world_to_object shape) (RTCTuple.point 2. 3. 4.))));

    "A pattern with a pattern transformation" >::
    (fun test_ctxt ->
      let shape = RTCSphere.build () in
      let pattern' = test_pattern () in
      let pattern = RTCPattern.transform pattern' (RTCTransform.scaling 2. 2. 2.) in
      let expect = RTCColor.build 1. 1.5 2. in
      assert (RTCColor.equal expect (RTCPattern.at_object pattern (RTCShape.world_to_object shape) (RTCTuple.point 2. 3. 4.))));

    "A pattern with both an object and a pattern transformation" >::
    (fun test_ctxt ->
      let shape = RTCShape.transform (RTCSphere.build ()) (RTCTransform.scaling 2. 2. 2.) in
      let pattern' = test_pattern () in
      let pattern = RTCPattern.transform pattern' (RTCTransform.translation 0.5 1. 1.5) in
      let expect = RTCColor.build 0.75 0.5 0.25 in
      assert (RTCColor.equal expect (RTCPattern.at_object pattern (RTCShape.world_to_object shape) (RTCTuple.point 2.5 3. 3.5))));

    "A stripe pattern is constant in y" >::
    (fun test_ctxt ->
      let pattern = RTCPattern.stripe white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 1. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 2. 0.)));

    "A stripe pattern is constant in z" >::
    (fun test_ctxt ->
      let pattern = RTCPattern.stripe white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 1.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 2.)));

    "A stripe pattern alternates in x" >::
    (fun test_ctxt ->
      let pattern = RTCPattern.stripe white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0.9 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 1. 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point (-0.1) 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point (-1.) 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point (-1.1) 0. 0.)));

    "A gradient linearly interpolates between colors" >::
    ( fun test_ctxt ->
      let pattern = RTCPattern.gradient white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal (RTCPattern.Solid (RTCColor.build 0.75 0.75 0.75)) (pattern.fn (RTCTuple.point 0.25 0. 0.));
      assert_equal (RTCPattern.Solid (RTCColor.build 0.5 0.5 0.5)) (pattern.fn (RTCTuple.point 0.5 0. 0.));
      assert_equal (RTCPattern.Solid (RTCColor.build 0.25 0.25 0.25)) (pattern.fn (RTCTuple.point 0.75 0. 0.)));

    "A ring should extend in both x and z" >::
    ( fun test_ctxt ->
      let pattern = RTCPattern.ring white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 1. 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 0. 0. 1.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 0.708 0. 0.708)));

    "A ring should extend in both x and z" >::
    ( fun test_ctxt ->
      let pattern = RTCPattern.ring white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 1. 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 0. 0. 1.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 0.708 0. 0.708)));

    "Checkers should repeat in x" >::
    ( fun test_ctxt ->
      let pattern = RTCPattern.checkers white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0.99 0. 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 1.01 0. 0.)));

    "Checkers should repeat in y" >::
    ( fun test_ctxt ->
      let pattern = RTCPattern.checkers white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0.99 0.));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 0. 1.01 0.)));

    "Checkers should repeat in z" >::
    ( fun test_ctxt ->
      let pattern = RTCPattern.checkers white_pattern black_pattern in
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.));
      assert_equal white_pattern (pattern.fn (RTCTuple.point 0. 0. 0.99));
      assert_equal black_pattern (pattern.fn (RTCTuple.point 0. 0. 1.01)));
  ]
