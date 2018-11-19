type t = { fn: RTCTuple.t -> texture_t;
           transform: RTCMatrix.t;
           inverse_transform: RTCMatrix.t }
and texture_t =
  | Solid of RTCColor.t
  | Pattern of t

let build fn = { fn; transform=RTCMatrix.identity; inverse_transform=RTCMatrix.identity }

let rec eval texture opoint = match texture with
  | Solid color -> color
  | Pattern pattern ->
    let ppoint = RTCMatrix.tmult pattern.inverse_transform opoint in
    eval (pattern.fn ppoint) ppoint

let solid (a:RTCColor.t) = Solid a

let stripe (a:texture_t) (b:texture_t) =
  let fn (point:RTCTuple.t) =
    let disc = (int_of_float (floor point.x)) mod 2 in
    if disc = 0 then a else b
  in
  build fn

let gradient (a:texture_t) (b:texture_t) =
  let fn (point:RTCTuple.t) =
    let ac = eval a point in
    let bc = eval b point in
    let distance = RTCColor.subtract bc ac in
    let fraction = point.x -. (floor point.x) in
    Solid (RTCColor.add ac (RTCColor.mults distance fraction))
  in
  build fn

let ring (a:texture_t) (b:texture_t) =
  let fn (point:RTCTuple.t) =
    let distance = floor (sqrt (point.x ** 2. +. point.z ** 2.)) in
    let disc = (int_of_float distance) mod 2 in
    if disc = 0 then a else b
  in
  build fn

let checkers (a:texture_t) (b:texture_t) =
  let fn (point:RTCTuple.t) =
    let distance = (floor point.x) +. floor(point.y) +. floor(point.z) in
    let disc = (int_of_float distance) mod 2 in
    if disc = 0 then a else b
  in
  build fn

let at_object (pattern:t) (transform:RTCTuple.t -> RTCTuple.t) (point:RTCTuple.t) =
  let opoint = transform point in
  eval (Pattern pattern) opoint

let transform (pattern:t) (m:RTCMatrix.t) =
  { pattern with transform=m; inverse_transform=(RTCMatrix.inverse m) }
