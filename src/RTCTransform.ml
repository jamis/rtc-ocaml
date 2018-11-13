let translation x y z = [| [| 1.; 0.; 0.; x  |];
                           [| 0.; 1.; 0.; y  |];
                           [| 0.; 0.; 1.; z  |];
                           [| 0.; 0.; 0.; 1. |] |]

let scaling x y z = [| [| x;  0.; 0.; 0. |];
                       [| 0.; y;  0.; 0. |];
                       [| 0.; 0.; z;  0. |];
                       [| 0.; 0.; 0.; 1. |] |]

let rotation_x r =
  let cosr = cos(r) in
  let sinr = sin(r) in
  [| [| 1.; 0.; 0.; 0. |];
     [| 0.; cosr; -.sinr; 0. |];
     [| 0.; sinr; cosr; 0. |];
     [| 0.; 0.; 0.; 1. |] |]

let rotation_y r =
  let cosr = cos(r) in
  let sinr = sin(r) in
  [| [| cosr; 0.; sinr; 0. |];
     [| 0.; 1.; 0.; 0. |];
     [| -.sinr; 0.; cosr; 0. |];
     [| 0.; 0.; 0.; 1. |] |]

let rotation_z r =
  let cosr = cos(r) in
  let sinr = sin(r) in
  [| [| cosr; -.sinr; 0.; 0. |];
     [| sinr; cosr; 0.; 0. |];
     [| 0.; 0.; 1.; 0. |];
     [| 0.; 0.; 0.; 1. |] |]

let shearing xy xz yx yz zx zy =
  [| [| 1.; xy; xz; 0. |];
     [| yx; 1.; yz; 0. |];
     [| zx; zy; 1.; 0. |];
     [| 0.; 0.; 0.; 1. |] |]
