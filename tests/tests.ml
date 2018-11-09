open OUnit2

let () =
  run_test_tt_main
    ~exit
    ("RTC">:::
     [
       TestTuple.tests;
       TestColor.tests;
       TestCanvas.tests;
     ])
