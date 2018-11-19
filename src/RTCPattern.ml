type t = { fn: RTCTuple.t -> RTCColor.t;
           transform: RTCMatrix.t;
           inverse_transform: RTCMatrix.t }

let build fn = { fn; transform=RTCMatrix.identity; inverse_transform=RTCMatrix.identity }

let stripe (a:RTCColor.t) (b:RTCColor.t) =
  let fn (point:RTCTuple.t) =
    let disc = (int_of_float (floor point.x)) mod 2 in
    if disc = 0 then a else b
  in
  build fn

let gradient (a:RTCColor.t) (b:RTCColor.t) =
  let fn (point:RTCTuple.t) =
    let distance = RTCColor.subtract b a in
    let fraction = point.x -. (floor point.x) in
    RTCColor.add a (RTCColor.mults distance fraction)
  in
  build fn

let ring (a:RTCColor.t) (b:RTCColor.t) =
  let fn (point:RTCTuple.t) =
    let distance = floor (sqrt (point.x ** 2. +. point.z ** 2.)) in
    let disc = (int_of_float distance) mod 2 in
    if disc = 0 then a else b
  in
  build fn

let checkers (a:RTCColor.t) (b:RTCColor.t) =
  let fn (point:RTCTuple.t) =
    let distance = (floor point.x) +. floor(point.y) +. floor(point.z) in
    let disc = (int_of_float distance) mod 2 in
    if disc = 0 then a else b
  in
  build fn

let at_object (pattern:t) (transform:RTCTuple.t -> RTCTuple.t) (point:RTCTuple.t) =
  let opoint = transform point in
  let ppoint = RTCMatrix.tmult pattern.inverse_transform opoint in
  pattern.fn ppoint

let transform (pattern:t) (m:RTCMatrix.t) =
  { pattern with transform=m; inverse_transform=(RTCMatrix.inverse m) }
