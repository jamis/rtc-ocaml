class shape =
  object (self)
    inherit RTCShape.shape
  end

let intersect sphere (ray : RTCRay.ray) =
  let ray2 = RTCRay.transform ray sphere#inverse_transform in
  let sphere_to_ray = RTCTuple.subtract ray2.origin (RTCTuple.point 0. 0. 0.) in
  let a = RTCTuple.dot ray2.direction ray2.direction in
  let b = 2. *. (RTCTuple.dot ray2.direction sphere_to_ray) in
  let c = (RTCTuple.dot sphere_to_ray sphere_to_ray) -. 1. in
  let disc = b *. b -. 4. *. a *. c in
  if disc < 0. then RTCIntersection.list []
  else let t1 = (-.b -. sqrt(disc)) /. (2. *. a) in
       let t2 = (-.b +. sqrt(disc)) /. (2. *. a) in
       RTCIntersection.list [(RTCIntersection.build t1 sphere); (RTCIntersection.build t2 sphere)]
