let build () =
  let local_intersect shape ?(trail=[]) (r:RTCRay.t) =
    if (abs_float r.direction.y) < RTCConst.epsilon then
      []
    else let t = -.r.origin.y /. r.direction.y in
      [ RTCIntersection.build t shape trail ]
  in
  let local_normal_at ?(hit=None) shape (point:RTCTuple.t) = RTCTuple.vector 0. 1. 0. in
  RTCShape.build Plane local_intersect local_normal_at
