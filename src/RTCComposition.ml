type transform =
  | Translate of float * float * float
  | Scale of float * float * float
  | RotateX of float
  | RotateY of float
  | RotateZ of float

let compose tforms =
  let rec aux acc = function
    | [] -> acc
    | Translate (dx, dy, dz) :: tforms -> aux (RTCMatrix.mult (RTCTransform.translation dx dy dz) acc) tforms
    | Scale (sx, sy, sz) :: tforms -> aux (RTCMatrix.mult (RTCTransform.scaling sx sy sz) acc) tforms
    | RotateX theta :: tforms -> aux (RTCMatrix.mult (RTCTransform.rotation_x theta) acc) tforms
    | RotateY theta :: tforms -> aux (RTCMatrix.mult (RTCTransform.rotation_y theta) acc) tforms
    | RotateZ theta :: tforms -> aux (RTCMatrix.mult (RTCTransform.rotation_z theta) acc) tforms
  in
  aux RTCMatrix.identity tforms
