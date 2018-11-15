open OUnit2

let tests =
  "Rays" >:::
  [
    "Creating and querying a ray" >::
    (fun test_ctxt ->
      let origin = RTCTuple.point 1. 2. 3. in
      let direction = RTCTuple.vector 4. 5. 6. in
      let ray = RTCRay.build origin direction in
      assert (RTCTuple.equal origin ray.origin);
      assert (RTCTuple.equal direction ray.direction));

    "Computing a point from a distance" >::
    (fun test_ctxt ->
      let ray = RTCRay.build (RTCTuple.point 2. 3. 4.) (RTCTuple.vector 1. 0. 0.) in
      assert (RTCTuple.equal (RTCTuple.point 2. 3. 4.) (RTCRay.position ray 0.));
      assert (RTCTuple.equal (RTCTuple.point 3. 3. 4.) (RTCRay.position ray 1.));
      assert (RTCTuple.equal (RTCTuple.point 1. 3. 4.) (RTCRay.position ray (-1.)));
      assert (RTCTuple.equal (RTCTuple.point 4.5 3. 4.) (RTCRay.position ray 2.5)));

    "Translating a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 1. 2. 3.) (RTCTuple.vector 0. 1. 0.) in
      let m = RTCTransform.translation 3. 4. 5. in
      let r2 = RTCRay.transform r m in
      assert (RTCTuple.equal r2.origin (RTCTuple.point 4. 6. 8.));
      assert (RTCTuple.equal r2.direction (RTCTuple.vector 0. 1. 0.)));

    "Scaling a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 1. 2. 3.) (RTCTuple.vector 0. 1. 0.) in
      let m = RTCTransform.scaling 2. 3. 4. in
      let r2 = RTCRay.transform r m in
      assert (RTCTuple.equal r2.origin (RTCTuple.point 2. 6. 12.));
      assert (RTCTuple.equal r2.direction (RTCTuple.vector 0. 3. 0.)));
  ]
