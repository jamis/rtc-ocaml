open OUnit2

let tests =
  "Transformations" >:::
  [
    "Multiplying by a translation matrix" >::
    (fun test_ctxt ->
      let transform = RTCTransform.translation 5. (-3.) 2. in
      let p = RTCTuple.point (-3.) 4. 5. in
      let expect = RTCTuple.point 2. 1. 7. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "Multiplying by the inverse of a translation matrix" >::
    (fun test_ctxt ->
      let transform = RTCTransform.translation 5. (-3.) 2. in
      let inv = RTCMatrix.inverse transform in
      let p = RTCTuple.point (-3.) 4. 5. in
      let expect = RTCTuple.point (-8.) 7. 3. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult inv p)));

    "Translation does not affect vectors" >::
    (fun test_ctxt ->
      let transform = RTCTransform.translation 5. (-3.) 2. in
      let v = RTCTuple.vector (-3.) 4. 5. in
      assert (RTCTuple.equal v (RTCMatrix.tmult transform v)));

    "A scaling matrix applied to a point" >::
    (fun test_ctxt ->
      let transform = RTCTransform.scaling 2. 3. 4. in
      let p = RTCTuple.point (-4.) 6. 8. in
      let expect = RTCTuple.point (-8.) 18. 32. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "A scaling matrix applied to a vector" >::
    (fun test_ctxt ->
      let transform = RTCTransform.scaling 2. 3. 4. in
      let v = RTCTuple.vector (-4.) 6. 8. in
      let expect = RTCTuple.vector (-8.) 18. 32. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform v)));

    "Multiplying by the inverse of a scaling matrix" >::
    (fun test_ctxt ->
      let transform = RTCTransform.scaling 2. 3. 4. in
      let inv = RTCMatrix.inverse transform in
      let v = RTCTuple.vector (-4.) 6. 8. in
      let expect = RTCTuple.vector (-2.) 2. 2. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult inv v)));

    "Reflection is scaling by a negative value" >::
    (fun test_ctxt ->
      let transform = RTCTransform.scaling (-1.) 1. 1. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point (-2.) 3. 4. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "Rotating a point around the x axis" >::
    (fun test_ctxt ->
      let p = RTCTuple.point 0. 1. 0. in
      let half_quarter = RTCTransform.rotation_x (Float.pi /. 4.) in
      let full_quarter = RTCTransform.rotation_x (Float.pi /. 2.) in
      let expect_half = RTCTuple.point 0. (sqrt(2.) /. 2.) (sqrt(2.) /. 2.) in
      let expect_full = RTCTuple.point 0. 0. 1. in
      assert (RTCTuple.equal expect_half (RTCMatrix.tmult half_quarter p));
      assert (RTCTuple.equal expect_full (RTCMatrix.tmult full_quarter p)));

    "The inverse of an x-rotation rotates in the opposite direction" >::
    (fun test_ctxt ->
      let p = RTCTuple.point 0. 1. 0. in
      let half_quarter = RTCTransform.rotation_x (Float.pi /. 4.) in
      let inv = RTCMatrix.inverse half_quarter in
      let expect = RTCTuple.point 0. (sqrt(2.) /. 2.) (-.sqrt(2.) /. 2.) in
      assert (RTCTuple.equal expect (RTCMatrix.tmult inv p)));

    "Rotating a point around the y axis" >::
    (fun test_ctxt ->
      let p = RTCTuple.point 0. 0. 1. in
      let half_quarter = RTCTransform.rotation_y (Float.pi /. 4.) in
      let full_quarter = RTCTransform.rotation_y (Float.pi /. 2.) in
      let expect_half = RTCTuple.point (sqrt(2.) /. 2.) 0. (sqrt(2.) /. 2.) in
      let expect_full = RTCTuple.point 1. 0. 0. in
      assert (RTCTuple.equal expect_half (RTCMatrix.tmult half_quarter p));
      assert (RTCTuple.equal expect_full (RTCMatrix.tmult full_quarter p)));

    "Rotating a point around the z axis" >::
    (fun test_ctxt ->
      let p = RTCTuple.point 0. 1. 0. in
      let half_quarter = RTCTransform.rotation_z (Float.pi /. 4.) in
      let full_quarter = RTCTransform.rotation_z (Float.pi /. 2.) in
      let expect_half = RTCTuple.point (-.sqrt(2.) /. 2.) (sqrt(2.) /. 2.) 0. in
      let expect_full = RTCTuple.point (-1.) 0. 0. in
      assert (RTCTuple.equal expect_half (RTCMatrix.tmult half_quarter p));
      assert (RTCTuple.equal expect_full (RTCMatrix.tmult full_quarter p)));

    "A shearing transformation moves x in proportion to y" >::
    (fun test_ctxt ->
      let transform = RTCTransform.shearing 1. 0. 0. 0. 0. 0. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point 5. 3. 4. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "A shearing transformation moves x in proportion to z" >::
    (fun test_ctxt ->
      let transform = RTCTransform.shearing 0. 1. 0. 0. 0. 0. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point 6. 3. 4. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "A shearing transformation moves y in proportion to x" >::
    (fun test_ctxt ->
      let transform = RTCTransform.shearing 0. 0. 1. 0. 0. 0. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point 2. 5. 4. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "A shearing transformation moves y in proportion to z" >::
    (fun test_ctxt ->
      let transform = RTCTransform.shearing 0. 0. 0. 1. 0. 0. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point 2. 7. 4. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "A shearing transformation moves z in proportion to x" >::
    (fun test_ctxt ->
      let transform = RTCTransform.shearing 0. 0. 0. 0. 1. 0. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point 2. 3. 6. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));

    "A shearing transformation moves z in proportion to y" >::
    (fun test_ctxt ->
      let transform = RTCTransform.shearing 0. 0. 0. 0. 0. 1. in
      let p = RTCTuple.point 2. 3. 4. in
      let expect = RTCTuple.point 2. 3. 7. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult transform p)));
  ]
