let build () =
  let check_axis origin direction =
    let t1 = ((-1.) -. origin) /. direction in
    let t2 = (1. -. origin) /. direction in
    if t1 > t2 then (t2, t1) else (t1, t2)
  in
  let local_intersect shape ?(trail=[]) (r:RTCRay.t) =
    let (xtmin, xtmax) = check_axis r.origin.x r.direction.x in
    let (ytmin, ytmax) = check_axis r.origin.y r.direction.y in
    let (ztmin, ztmax) = check_axis r.origin.z r.direction.z in
    let tmin = max (max xtmin ytmin) ztmin in
    let tmax = min (min xtmax ytmax) ztmax in
    if tmin > tmax then
      []
    else
      [ RTCIntersection.build tmin shape trail;
        RTCIntersection.build tmax shape trail ]
  in
  let local_normal_at shape (point:RTCTuple.t) =
    let x = abs_float point.x in
    let y = abs_float point.y in
    let z = abs_float point.z in
    let maxc = max (max x y) z in
    if maxc = x then RTCTuple.vector point.x 0. 0.
    else if maxc = y then RTCTuple.vector 0. point.y 0.
    else RTCTuple.vector 0. 0. point.z
  in
  RTCShape.build Cube local_intersect local_normal_at
