open OUnit2

let tests =
  "Colors" >:::
  [
    "Colors are (red, green, blue) tuples" >::
    (fun test_ctxt ->
      let c = RTCColor.build (-.0.5) 0.4 1.7 in
      assert_equal (-.0.5) c.red;
      assert_equal 0.4 c.green;
      assert_equal 1.7 c.blue);

    "Adding colors" >::
    (fun test_ctxt ->
      let c1 = RTCColor.build 0.9 0.6 0.75 in
      let c2 = RTCColor.build 0.7 0.1 0.25 in
      assert (RTCColor.equal (RTCColor.build 1.6 0.7 1.0) (RTCColor.add c1 c2)));

    "Subtracting colors" >::
    (fun test_ctxt ->
      let c1 = RTCColor.build 0.9 0.6 0.75 in
      let c2 = RTCColor.build 0.7 0.1 0.25 in
      assert (RTCColor.equal (RTCColor.build 0.2 0.5 0.5) (RTCColor.subtract c1 c2)));

    "Multiplying a color by a scalar" >::
    (fun test_ctxt ->
      let c = RTCColor.build 0.2 0.3 0.4 in
      assert (RTCColor.equal (RTCColor.build 0.4 0.6 0.8) (RTCColor.mults c 2.)));

    "Multiplying colors" >::
    (fun test_ctxt ->
      let c1 = RTCColor.build 1. 0.2 0.4 in
      let c2 = RTCColor.build 0.9 1. 0.1 in
      assert (RTCColor.equal (RTCColor.build 0.9 0.2 0.04) (RTCColor.mult c1 c2)));
  ]
