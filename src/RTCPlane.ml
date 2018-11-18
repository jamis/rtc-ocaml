let build () =
  let local_intersect shape (r:RTCRay.t) =
    if (abs_float r.direction.y) < RTCConst.epsilon then
      []
    else let t = -.r.origin.y /. r.direction.y in
      [ RTCIntersection.build t shape ]
  in
  let local_normal_at shape (point:RTCTuple.t) = RTCTuple.vector 0. 1. 0. in
  RTCShape.build Plane local_intersect local_normal_at
