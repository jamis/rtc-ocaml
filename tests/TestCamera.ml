open OUnit2

let half_pi = Float.pi /. 2.
let qtr_pi = Float.pi /. 4.

let assert_in_epsilon a b =
  let diff = abs_float (a -. b) in
  assert (diff < RTCConst.epsilon)

let tests =
  "Camera" >:::
  [
    "Constructing a camera" >::
    (fun test_ctxt ->
      let camera = RTCCamera.build 160 120 half_pi RTCMatrix.identity in
      assert_equal 160 camera.hsize;
      assert_equal 120 camera.vsize;
      assert_equal half_pi camera.field_of_view;
      assert (RTCMatrix.equal camera.transform RTCMatrix.identity));

    "The pixel size for a horizontal canvas" >::
    (fun test_ctxt ->
      let camera = RTCCamera.build 200 125 half_pi RTCMatrix.identity in
      assert_in_epsilon 0.01 camera.pixel_size);

    "The pixel size for a vertical canvas" >::
    (fun test_ctxt ->
      let camera = RTCCamera.build 125 200 half_pi RTCMatrix.identity in
      assert_in_epsilon 0.01 camera.pixel_size);

    "Constructing a ray through the center of the canvas" >::
    (fun test_ctxt ->
      let camera = RTCCamera.build 201 101 half_pi RTCMatrix.identity in
      let r = RTCCamera.ray_for_pixel camera 100 50 in
      assert (RTCTuple.equal r.origin (RTCTuple.point 0. 0. 0.));
      assert (RTCTuple.equal r.direction (RTCTuple.vector 0. 0. (-1.))));

    "Constructing a ray through a corner of the canvas" >::
    (fun test_ctxt ->
      let camera = RTCCamera.build 201 101 half_pi RTCMatrix.identity in
      let r = RTCCamera.ray_for_pixel camera 0 0 in
      assert (RTCTuple.equal r.origin (RTCTuple.point 0. 0. 0.));
      assert (RTCTuple.equal r.direction (RTCTuple.vector 0.66519 0.33259 (-0.66851))));

    "Constructing a ray when the camera is transformed" >::
    (fun test_ctxt ->
      let tx = RTCMatrix.mult (RTCTransform.rotation_y qtr_pi) (RTCTransform.translation 0. (-2.) 5.) in
      let camera = RTCCamera.build 201 101 half_pi tx in
      let r = RTCCamera.ray_for_pixel camera 100 50 in
      assert (RTCTuple.equal r.origin (RTCTuple.point 0. 2. (-5.)));
      assert (RTCTuple.equal r.direction (RTCTuple.vector (sqrt(2.)/.2.) 0. (-.sqrt(2.)/.2.))));

    "Rendering a world with a camera" >::
    (fun test_ctxt ->
      let w = TestWorld.default_world () in
      let from_p = RTCTuple.point 0. 0. (-5.) in
      let to_p = RTCTuple.point 0. 0. 0. in
      let up_v = RTCTuple.vector 0. 1. 0. in
      let c = RTCCamera.build 11 11 half_pi (RTCTransform.view from_p to_p up_v) in
      let image = RTCCamera.render c w in
      let expect = RTCColor.build 0.38066 0.47583 0.2855 in
      let actual = RTCCanvas.pixel_at image 5 5 in
      assert (RTCColor.equal expect actual));
  ]
