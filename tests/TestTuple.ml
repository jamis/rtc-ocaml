open OUnit2

let tests =
  "Tuples, Points, Vectors" >:::
  [
    "A tuple with w=1.0 is a point" >::
    (fun test_ctxt ->
      let a = RTCTuple.build 4.3 (-4.2) 3.1 1.0 in
      assert_equal 4.3 a.x;
      assert_equal (-4.2) a.y;
      assert_equal 3.1 a.z;
      assert_equal 1.0 a.w;
      assert_bool "shouldn't be vector" (not (RTCTuple.is_vector a));
      assert_bool "should be point" (RTCTuple.is_point a));

    "A tuple with w=0.0 is a vector" >::
    (fun test_ctxt ->
      let a = RTCTuple.build 4.3 (-4.2) 3.1 0.0 in
      assert_equal 4.3 a.x;
      assert_equal (-4.2) a.y;
      assert_equal 3.1 a.z;
      assert_equal 0.0 a.w;
      assert_bool "should be vector" (RTCTuple.is_vector a);
      assert_bool "shouldn't be point" (not (RTCTuple.is_point a)));

    "point() creates tuples with w=1" >::
    (fun test_ctxt ->
      let p = RTCTuple.point 4.0 (-4.0) 3.0 in
      assert_equal p (RTCTuple.build 4.0 (-4.0) 3.0 1.0));

    "vector() creates tuples with w=0" >::
    (fun test_ctxt ->
      let p = RTCTuple.vector 4.0 (-4.0) 3.0 in
      assert_equal p (RTCTuple.build 4.0 (-4.0) 3.0 0.0));

    "Adding two tuples" >::
    (fun test_ctxt ->
      let a1 = RTCTuple.build 3. (-2.) 5. 1. in
      let a2 = RTCTuple.build (-2.) 3. 1. 0. in
      assert_equal (RTCTuple.add a1 a2) (RTCTuple.build 1. 1. 6. 1.));

    "Subtracting two points" >::
    (fun test_ctxt ->
      let p1 = RTCTuple.point 3. 2. 1. in
      let p2 = RTCTuple.point 5. 6. 7. in
      assert_equal (RTCTuple.subtract p1 p2) (RTCTuple.vector (-2.) (-4.) (-6.)));

    "Subtracting a vector from a point" >::
    (fun test_ctxt ->
      let p = RTCTuple.point 3. 2. 1. in
      let v = RTCTuple.vector 5. 6. 7. in
      assert_equal (RTCTuple.subtract p v) (RTCTuple.point (-2.) (-4.) (-6.)));

    "Subtracting two vectors" >::
    (fun test_ctxt ->
      let v1 = RTCTuple.vector 3. 2. 1. in
      let v2 = RTCTuple.vector 5. 6. 7. in
      assert_equal (RTCTuple.subtract v1 v2) (RTCTuple.vector (-2.) (-4.) (-6.)));

    "Subtracting a vector from the zero vector" >::
    (fun test_ctxt ->
      let zero = RTCTuple.vector 0. 0. 0. in
      let v = RTCTuple.vector 1. (-2.) 3. in
      assert_equal (RTCTuple.subtract zero v) (RTCTuple.vector (-1.) 2. (-3.)));

    "Negating a tuple" >::
    (fun test_ctxt ->
      let a = RTCTuple.build 1. (-2.) 3. (-4.) in
      assert_equal (RTCTuple.neg a) (RTCTuple.build (-1.) 2. (-3.) 4.));

    "Multiplying a tuple by a scalar" >::
    (fun test_ctxt ->
      let a = RTCTuple.build 1. (-2.) 3. (-4.) in
      assert_equal (RTCTuple.mults a 3.5) (RTCTuple.build 3.5 (-7.) 10.5 (-14.)));

    "Multiplying a tuple by a fraction" >::
    (fun test_ctxt ->
      let a = RTCTuple.build 1. (-2.) 3. (-4.) in
      assert_equal (RTCTuple.mults a 0.5) (RTCTuple.build 0.5 (-1.) 1.5 (-2.)));

    "Dividing a tuple by a scalar" >::
    (fun test_ctxt ->
      let a = RTCTuple.build 1. (-2.) 3. (-4.) in
      assert_equal (RTCTuple.divs a 2.) (RTCTuple.build 0.5 (-1.) 1.5 (-2.)));

    "Computing the magnitude of vector(1, 0, 0)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 1. 0. 0. in
      assert_equal 1. (RTCTuple.mag vec));

    "Computing the magnitude of vector(0, 1, 0)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 0. 1. 0. in
      assert_equal 1. (RTCTuple.mag vec));

    "Computing the magnitude of vector(0, 0, 1)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 0. 0. 1. in
      assert_equal 1. (RTCTuple.mag vec));

    "Computing the magnitude of vector(1, 2, 3)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 1. 2. 3. in
      assert_equal (sqrt 14.) (RTCTuple.mag vec));

    "Computing the magnitude of vector(-1, -2, -3)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector (-.1.) (-.2.) (-.3.) in
      assert_equal (sqrt 14.) (RTCTuple.mag vec));

    "Normalizing vector(4, 0, 0) gives (1, 0, 0)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 4. 0. 0. in
      assert (RTCTuple.equal (RTCTuple.norm vec) (RTCTuple.vector 1. 0. 0.)));

    "Normalizing vector(4, 0, 0) gives (1, 2, 3)" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 1. 2. 3. in
      assert (RTCTuple.equal (RTCTuple.norm vec) (RTCTuple.vector 0.26726 0.53452 0.80178)));

    "The magnitude of a normalized vector" >::
    (fun test_ctxt ->
      let vec = RTCTuple.vector 1. 2. 3. in
      let norm = RTCTuple.norm vec in
      assert_equal 1. (RTCTuple.mag norm));

    "The dot product of two tuples" >::
    (fun test_ctxt ->
      let a = RTCTuple.vector 1. 2. 3. in
      let b = RTCTuple.vector 2. 3. 4. in
      assert_equal 20. (RTCTuple.dot a b));

    "The cross product of two vectors" >::
    (fun test_ctxt ->
      let a = RTCTuple.vector 1. 2. 3. in
      let b = RTCTuple.vector 2. 3. 4. in
      assert (RTCTuple.equal (RTCTuple.cross a b) (RTCTuple.vector (-.1.) 2. (-.1.)));
      assert (RTCTuple.equal (RTCTuple.cross b a) (RTCTuple.vector 1. (-.2.) 1.)));

    "Reflecting a vector approaching at 45°" >::
    (fun test_ctxt ->
      let v = RTCTuple.vector 1. (-1.) 0. in
      let n = RTCTuple.vector 0. 1. 0. in
      let r = RTCTuple.reflect v n in
      assert (RTCTuple.equal r (RTCTuple.vector 1. 1. 0.)));

    "Reflecting a vector off a slanted surface" >::
    (fun test_ctxt ->
      let v = RTCTuple.vector 0. (-1.) 0. in
      let n = RTCTuple.vector (sqrt(2.)/.2.) (sqrt(2.)/.2.) 0. in
      let r = RTCTuple.reflect v n in
      assert (RTCTuple.equal r (RTCTuple.vector 1. 0. 0.)));
  ]
