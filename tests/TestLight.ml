open OUnit2

let tests =
  "Lights" >:::
  [
    "A point light has a position and intensity" >::
    (fun test_ctxt ->
      let intensity = RTCColor.build 1. 1. 1. in
      let position = RTCTuple.point 0. 0. 0. in
      let light = RTCLight.point position intensity in
      assert (RTCColor.equal intensity light.intensity);
      assert (RTCTuple.equal position light.position));
  ]
