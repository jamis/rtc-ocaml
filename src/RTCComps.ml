type comps = { t : float;
               shape : RTCShape.shape;
               point : RTCTuple.tuple;
               eyev : RTCTuple.tuple;
               normalv : RTCTuple.tuple;
               inside : bool }

let prepare (i:RTCIntersection.intersection) r =
  let point = RTCRay.position r i.t in
  let eyev = RTCTuple.neg r.direction in
  let normalv = RTCSphere.normal_at i.shape point in
  let inside = (RTCTuple.dot normalv eyev) < 0. in
  let normalv' = if inside then RTCTuple.neg normalv else normalv in
  let point' = RTCTuple.add_mults point normalv' RTCConst.epsilon in
  { t=i.t; shape=i.shape; point=point'; eyev; normalv=normalv'; inside }
