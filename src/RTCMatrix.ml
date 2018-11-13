let identity = [| [| 1.; 0.; 0.; 0. |];
                  [| 0.; 1.; 0.; 0. |];
                  [| 0.; 0.; 1.; 0. |];
                  [| 0.; 0.; 0.; 1. |] |]

let equal a b =
  let diff v1 v2 = abs_float (v1 -. v2) in
  let zero x = x < RTCConst.epsilon in
  let row_collapse row_a row_b = Array.for_all zero (Array.map2 diff row_a row_b) in
  Array.for_all (fun x -> x) (Array.map2 row_collapse a b)

let mult a b =
  let compute row col = Array.fold_left (+.) 0. (Array.mapi (fun i _ -> a.(row).(i) *. b.(i).(col)) a) in
  Array.mapi (fun row _ -> Array.mapi (fun col _ -> compute row col) b) a

let tmult a (b:RTCTuple.tuple) =
  let x = a.(0).(0) *. b.x +. a.(0).(1) *. b.y +. a.(0).(2) *. b.z +. a.(0).(3) *. b.w in
  let y = a.(1).(0) *. b.x +. a.(1).(1) *. b.y +. a.(1).(2) *. b.z +. a.(1).(3) *. b.w in
  let z = a.(2).(0) *. b.x +. a.(2).(1) *. b.y +. a.(2).(2) *. b.z +. a.(2).(3) *. b.w in
  let w = a.(3).(0) *. b.x +. a.(3).(1) *. b.y +. a.(3).(2) *. b.z +. a.(3).(3) *. b.w in
  RTCTuple.build x y z w

let transpose a =
  Array.mapi (fun row _ -> Array.mapi (fun col _ -> a.(col).(row)) a) a

let submatrix m row col =
  let dim = (Array.length m) - 1 in
  let result = Array.make_matrix dim dim 0. in
  for y = 0 to dim do
    if y <> row then
      let y' = if y < row then y else y-1 in
      for x = 0 to dim do
        if x <> col then
          let x' = if x < col then x else x-1 in
          result.(y').(x') <- m.(y).(x)
      done
  done;
  result

let rec determinant = function
    | [| [| a; b |]; [| c; d |] |] -> a *. d -. b *. c
    | m -> Array.fold_left (+.) 0. (Array.mapi (fun i _ -> m.(0).(i) *. (cofactor m 0 i)) m)
and cofactor m row col =
  let el = minor m row col in
  match (row + col) with
  | n when n mod 2 = 0 -> el
  | _ -> -.el
and minor m row col = determinant (submatrix m row col)

let invertible m = (abs_float (determinant m)) >= RTCConst.epsilon

exception NotInvertible of string

let inverse m =
  let det = determinant m in
  if (abs_float det) < RTCConst.epsilon then
    raise (NotInvertible "matrix is not invertible")
  else
    let calc row col = (cofactor m row col) /. det in
    (* "calc x y" instead of "calc y x" performs the transpose *)
    Array.mapi (fun y row -> Array.mapi (fun x _ -> calc x y) row) m
