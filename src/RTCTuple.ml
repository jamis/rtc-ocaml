type tuple = { x : float; y : float; z : float; w : float }

let build x y z w = { x; y; z; w; }

let point x y z = build x y z 1.0
let vector x y z = build x y z 0.0

let is_vector a = a.w = 0.0
let is_point a = a.w = 1.0

let equal a b = let xdiff = abs_float(a.x -. b.x) in
                let ydiff = abs_float(a.y -. b.y) in
                let zdiff = abs_float(a.z -. b.z) in
                let wdiff = abs_float(a.w -. b.w) in
                xdiff < RTCConst.epsilon && ydiff < RTCConst.epsilon &&
                zdiff < RTCConst.epsilon && wdiff < RTCConst.epsilon

let add a b = build (a.x +. b.x) (a.y +. b.y) (a.z +. b.z) (a.w +. b.w)
let subtract a b = build (a.x -. b.x) (a.y -. b.y) (a.z -. b.z) (a.w -. b.w)
let neg a = { x = -. a.x; y = -. a.y; z = -. a.z; w = -. a.w }
let mults a scalar = { x = (a.x *. scalar); y = (a.y *. scalar); z = (a.z *. scalar); w = (a.w *. scalar) }
let divs a scalar = { x = (a.x /. scalar); y = (a.y /. scalar); z = (a.z /. scalar); w = (a.w /. scalar) }
let mag vec = sqrt (vec.x ** 2. +. vec.y ** 2. +. vec.z ** 2. +. vec.w ** 2.)
let norm vec = divs vec (mag vec)

let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z

let cross a b = vector (a.y *. b.z -. a.z *. b.y)
                       (a.z *. b.x -. a.x *. b.z)
                       (a.x *. b.y -. a.y *. b.x)
