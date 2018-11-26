let build () =
  let local_intersect shape ?(trail=[]) (r:RTCRay.t) =
    let sphere_to_ray = RTCTuple.subtract r.origin (RTCTuple.point 0. 0. 0.) in
    let a = RTCTuple.dot r.direction r.direction in
    let b = 2. *. (RTCTuple.dot r.direction sphere_to_ray) in
    let c = (RTCTuple.dot sphere_to_ray sphere_to_ray) -. 1. in
    let disc = b *. b -. 4. *. a *. c in
    if disc < 0. then RTCIntersection.list []
    else let t1 = (-.b -. sqrt(disc)) /. (2. *. a) in
         let t2 = (-.b +. sqrt(disc)) /. (2. *. a) in
         let i1 = RTCIntersection.build t1 shape trail in
         let i2 = RTCIntersection.build t2 shape trail in
         RTCIntersection.list [i1; i2]
  in
  let local_normal_at ?(hit=None) shape (point:RTCTuple.t) = RTCTuple.vector point.x point.y point.z in
  RTCShape.build Sphere local_intersect local_normal_at
