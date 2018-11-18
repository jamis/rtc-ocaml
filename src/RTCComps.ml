type t = { t : float;
           shape : RTCShape.t;
           point : RTCTuple.t;
           eyev : RTCTuple.t;
           normalv : RTCTuple.t;
           inside : bool }

let prepare (i:RTCShape.t RTCIntersection.t) r =
  let point = RTCRay.position r i.t in
  let eyev = RTCTuple.neg r.direction in
  let normalv = RTCShape.normal_at i.shape point in
  let inside = (RTCTuple.dot normalv eyev) < 0. in
  let normalv' = if inside then RTCTuple.neg normalv else normalv in
  let point' = RTCTuple.add_mults point normalv' RTCConst.epsilon in
  { t=i.t; shape=i.shape; point=point'; eyev; normalv=normalv'; inside }
