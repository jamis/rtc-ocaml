open OUnit2

let tests =
  "Matrix" >:::
  [
    "Constructing and inspecting a 4x4 matrix" >::
    (fun test_ctxt ->
      let m = [| [|  1.;   2.;   3.;   4.  |];
                 [|  5.5;  6.5;  7.5;  8.5 |];
                 [|  9.;  10.;  11.;  12.  |];
                 [| 13.5; 14.5; 15.5; 16.5 |] |] in
      assert_equal 1. m.(0).(0);
      assert_equal 4. m.(0).(3);
      assert_equal 5.5 m.(1).(0);
      assert_equal 7.5 m.(1).(2);
      assert_equal 11. m.(2).(2);
      assert_equal 13.5 m.(3).(0);
      assert_equal 15.5 m.(3).(2));

    "Constructing and inspecting a 2x2 matrix" >::
    (fun test_ctxt ->
      let m = [| [| -.3.;   5. |];
                 [|   1.; -.2. |] |] in
      assert_equal (-.3.) m.(0).(0);
      assert_equal 5. m.(0).(1);
      assert_equal 1. m.(1).(0);
      assert_equal (-.2.) m.(1).(1));

    "Constructing and inspecting a 3x3 matrix" >::
    (fun test_ctxt ->
      let m = [| [| -.3.;   5.;   0. |];
                 [|   1.; -.2.; -.7. |];
                 [|   0.;   1.;   1. |] |] in
      assert_equal (-.3.) m.(0).(0);
      assert_equal (-.2.) m.(1).(1);
      assert_equal 1. m.(2).(2));

    "Matrix equality with identical matrices" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 2.; 3.; 4. |];
                 [| 5.; 6.; 7.; 8. |];
                 [| 9.; 8.; 7.; 6.000001 |];
                 [| 5.; 4.; 3.; 2. |] |] in
      let b = [| [| 1.; 2.; 3.; 4. |];
                 [| 5.; 6.; 7.; 8. |];
                 [| 9.; 8.; 7.; 6.000002 |];
                 [| 5.; 4.; 3.; 2. |] |] in
      assert (RTCMatrix.equal a b));

    "Matrix equality with different matrices" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 2.; 3.; 4. |];
                 [| 5.; 6.; 7.; 8. |];
                 [| 9.; 8.; 7.; 6. |];
                 [| 5.; 4.; 3.; 2. |] |] in
      let b = [| [| 2.; 3.; 4.; 5. |];
                 [| 6.; 7.; 8.; 9. |];
                 [| 8.; 7.; 6.; 5. |];
                 [| 4.; 3.; 2.; 1. |] |] in
      assert (not (RTCMatrix.equal a b)));

    "Multiplying two matrices" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 2.; 3.; 4. |];
                 [| 5.; 6.; 7.; 8. |];
                 [| 9.; 8.; 7.; 6. |];
                 [| 5.; 4.; 3.; 2. |] |] in
      let b = [| [| (-2.); 1.; 2.; 3. |];
                 [| 3.; 2.; 1.; (-1.) |];
                 [| 4.; 3.; 6.; 5. |];
                 [| 1.; 2.; 7.; 8. |] |] in
      let expect = [| [| 20.; 22.; 50.; 48. |];
                      [| 44.; 54.; 114.; 108. |];
                      [| 40.; 58.; 110.; 102. |];
                      [| 16.; 26.; 46.; 42. |] |] in
      assert (RTCMatrix.equal expect (RTCMatrix.mult a b)));

    "A matrix multiplied by a tuple" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 2.; 3.; 4. |];
                 [| 2.; 4.; 4.; 2. |];
                 [| 8.; 6.; 4.; 1. |];
                 [| 0.; 0.; 0.; 1. |] |] in
      let b = RTCTuple.build 1. 2. 3. 1. in
      let expect = RTCTuple.build 18. 24. 33. 1. in
      assert (RTCTuple.equal expect (RTCMatrix.tmult a b)));

    "Multiplying a matrix by the identity matrix" >::
    (fun test_ctxt ->
      let a = [| [| 0.; 1.; 2.; 4. |];
                 [| 1.; 2.; 4.; 8. |];
                 [| 2.; 4.; 8.; 16. |];
                 [| 4.; 8.; 16.; 32. |] |] in
      assert (RTCMatrix.equal a (RTCMatrix.mult a RTCMatrix.identity)));

    "Transposing a matrix" >::
    (fun test_ctxt ->
      let a = [| [| 0.; 9.; 3.; 0. |];
                 [| 9.; 8.; 0.; 8. |];
                 [| 1.; 8.; 5.; 3. |];
                 [| 0.; 0.; 5.; 8. |] |] in
      let b = [| [| 0.; 9.; 1.; 0. |];
                 [| 9.; 8.; 8.; 0. |];
                 [| 3.; 0.; 5.; 5. |];
                 [| 0.; 8.; 3.; 8. |] |] in
      assert (RTCMatrix.equal b (RTCMatrix.transpose a)));

    "Transposing the identity matrix" >::
    (fun test_ctxt ->
      assert (RTCMatrix.equal RTCMatrix.identity (RTCMatrix.transpose RTCMatrix.identity)));

    "Calculating the determinant of a 2x2 matrix" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 5. |];
                 [| (-3.); 2. |] |] in
      assert_equal 17. (RTCMatrix.determinant a));

    "A submatrix of a 3x3 matrix is a 2x2 matrix" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 5.; 0. |];
                 [| (-3.); 2.; 7. |];
                 [| 0.; 6.; (-3.) |] |] in
      let expect = [| [| (-3.); 2. |];
                      [| 0.; 6. |] |] in
      assert (RTCMatrix.equal expect (RTCMatrix.submatrix a 0 2)));

    "A submatrix of a 4x4 matrix is a 3x3 matrix" >::
    (fun test_ctxt ->
      let a = [| [| (-6.); 1.; 1.; 6. |];
                 [| (-8.); 5.; 8.; 6. |];
                 [| (-1.); 0.; 8.; 2. |];
                 [| (-7.); 1.; (-1.); 1. |] |] in
      let expect = [| [| (-6.); 1.; 6. |];
                      [| (-8.); 8.; 6. |];
                      [| (-7.); (-1.); 1. |] |] in
      assert (RTCMatrix.equal expect (RTCMatrix.submatrix a 2 1)));

    "Calculating a minor of a 3x3 matrix" >::
    (fun test_ctxt ->
      let a = [| [| 3.; 5.; 0. |];
                 [| 2.; -1.; -7. |];
                 [| 6.; -1.; 5. |] |] in
      let b = RTCMatrix.submatrix a 1 0 in
      assert_equal 25.0 (RTCMatrix.determinant b);
      assert_equal 25.0 (RTCMatrix.minor a 1 0));

    "Calculating a cofactor of a 3x3 matrix" >::
    (fun test_ctxt ->
      let a = [| [| 3.; 5.; 0. |];
                 [| 2.; -1.; -7. |];
                 [| 6.; -1.; 5. |] |] in
      assert_equal (-12.) (RTCMatrix.minor a 0 0);
      assert_equal (-12.) (RTCMatrix.cofactor a 0 0);
      assert_equal 25. (RTCMatrix.minor a 1 0);
      assert_equal (-25.) (RTCMatrix.cofactor a 1 0));

    "Calculating the determinant of a 3x3 matrix" >::
    (fun test_ctxt ->
      let a = [| [| 1.; 2.; 6. |];
                 [| -5.; 8.; -4. |];
                 [| 2.; 6.; 4. |] |] in
      assert_equal 56. (RTCMatrix.cofactor a 0 0);
      assert_equal 12. (RTCMatrix.cofactor a 0 1);
      assert_equal (-46.) (RTCMatrix.cofactor a 0 2);
      assert_equal (-196.) (RTCMatrix.determinant a));

    "Calculating the determinant of a 4x4 matrix" >::
    (fun test_ctxt ->
      let a = [| [| -2.; -8.; 3.; 5. |];
                 [| -3.; 1.; 7.; 3. |];
                 [| 1.; 2.; -9.; 6. |];
                 [| -6.; 7.; 7.; -9. |] |] in
      assert_equal 690. (RTCMatrix.cofactor a 0 0);
      assert_equal 447. (RTCMatrix.cofactor a 0 1);
      assert_equal 210. (RTCMatrix.cofactor a 0 2);
      assert_equal 51. (RTCMatrix.cofactor a 0 3);
      assert_equal (-4071.) (RTCMatrix.determinant a));

    "Testing an invertible matrix for invertibility" >::
    (fun test_ctxt ->
      let a = [| [| 6.; 4.; 4.; 4. |];
                 [| 5.; 5.; 7.; 6. |];
                 [| 4.; -9.; 3.; -7. |];
                 [| 9.; 1.; 7.; -6. |] |] in
      assert_equal (-2120.) (RTCMatrix.determinant a);
      assert (RTCMatrix.invertible a));

    "Testing a non-invertible matrix for invertibility" >::
    (fun test_ctxt ->
      let a = [| [| -4.; 2.; -2.; -3. |];
                 [| 9.; 6.; 2.; 6. |];
                 [| 0.; -5.; 1.; -5. |];
                 [| 0.; 0.; 0.; 0. |] |] in
      assert_equal 0. (RTCMatrix.determinant a);
      assert (not (RTCMatrix.invertible a)));

    "Calculating the inverse of a matrix" >::
    (fun test_ctxt ->
      let a = [| [| -5.; 2.; 6.; -8. |];
                 [| 1.; -5.; 1.; 8. |];
                 [| 7.; 7.; -6.; -7. |];
                 [| 1.; -3.; 7.; 4. |] |] in
      let b = RTCMatrix.inverse a in
      let expect = [| [|  0.21805;  0.45113;  0.24060; -0.04511 |];
                      [| -0.80827; -1.45677; -0.44361;  0.52068 |];
                      [| -0.07895; -0.22368; -0.05263;  0.19737 |];
                      [| -0.52256; -0.81391; -0.30075;  0.30639 |] |] in
      assert_equal 532. (RTCMatrix.determinant a);
      assert_equal (-160.) (RTCMatrix.cofactor a 2 3);
      assert_equal (-160. /. 532.) b.(3).(2);
      assert_equal 105. (RTCMatrix.cofactor a 3 2);
      assert_equal (105. /. 532.) b.(2).(3);
      assert (RTCMatrix.equal b expect));

    "Calculating the inverse of another matrix" >::
    (fun test_ctxt ->
      let a = [| [| 8.; -5.; 9.; 2. |];
                 [| 7.; 5.; 6.; 1. |];
                 [| -6.; 0.; 9.; 6. |];
                 [| -3.; 0.; -9.; -4. |] |] in
      let expect = [| [| -0.15385; -0.15385; -0.28205; -0.53846 |];
                      [| -0.07692;  0.12308;  0.02564;  0.03077 |];
                      [|  0.35897;  0.35897;  0.43590;  0.92308 |];
                      [| -0.69231; -0.69231; -0.76923; -1.92308 |] |] in
      assert (RTCMatrix.equal expect (RTCMatrix.inverse a)));

    "Calculating the inverse of a third matrix" >::
    (fun test_ctxt ->
      let a = [| [| 9.; 3.; 0.; 9. |];
                 [| -5.; -2.; -6.; -3. |];
                 [| -4.; 9.; 6.; 4. |];
                 [| -7.; 6.; 6.; 2. |] |] in
      let expect = [| [| -0.04074; -0.07778;  0.14444; -0.22222 |];
                      [| -0.07778;  0.03333;  0.36667; -0.33333 |];
                      [| -0.02901; -0.14630; -0.10926;  0.12963 |];
                      [|  0.17778;  0.06667; -0.26667;  0.33333 |] |] in
      assert (RTCMatrix.equal expect (RTCMatrix.inverse a)));

    "Multiplying a product by its inverse" >::
    (fun test_ctxt ->
      let a = [| [| 3.; -9.; 7.; 3. |];
                 [| 3.; -8.; 2.; -9. |];
                 [| -4.; 4.; 4.; 1. |];
                 [| -6.; 5.; -1.; 1. |] |] in
      let b = [| [| 8.; 2.; 2.; 2. |];
                 [| 3.; -1.; 7.; 0. |];
                 [| 7.; 0.; 5.; 4. |];
                 [| 6.; -2.; 0.; 5. |] |] in
      let c = RTCMatrix.mult a b in
      assert (RTCMatrix.equal a (RTCMatrix.mult c (RTCMatrix.inverse b))));
  ]
