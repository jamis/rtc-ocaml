open OUnit2

let assert_in_epsilon a b =
  let diff = abs_float (a -. b) in
  assert (diff < RTCConst.epsilon)

let assert_tuple_equal a b =
  assert (RTCTuple.equal a b)
