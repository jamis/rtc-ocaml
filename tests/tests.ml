open OUnit2

let () =
  run_test_tt_main
    ~exit
    ("RTC">:::
     [
       TestTuple.tests;
       TestColor.tests;
       TestCanvas.tests;
       TestMatrix.tests;
       TestTransform.tests;
       TestRay.tests;
       TestIntersection.tests;
       TestLight.tests;
       TestMaterial.tests;
       TestPattern.tests;
       TestWorld.tests;
       TestCamera.tests;
       TestShape.tests;
       TestSphere.tests;
       TestPlane.tests;
     ])
