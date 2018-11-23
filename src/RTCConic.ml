let parameters_of (shape:RTCShape.t) = match shape.shape with
  | Cylinder (minimum, maximum, closed) -> (minimum, maximum, closed)
  | Cone (minimum, maximum, closed) -> (minimum, maximum, closed)
  | _ -> failwith "expected a cylinder or cone"

let intersect_caps trail shape min max closed (ray:RTCRay.t) radius_at xs =
  let check_cap t radius =
    let x = ray.origin.x +. t *. ray.direction.x
    and z = ray.origin.z +. t *. ray.direction.z in
    (x ** 2. +. z ** 2.) <= radius ** 2.
  in
  if (not closed) || ((abs_float ray.direction.y) < RTCConst.epsilon) then
    xs
  else
    let xs' =
      let t = (min -. ray.origin.y) /. ray.direction.y in
      if check_cap t (radius_at min) then (RTCIntersection.build t shape trail) :: xs else xs
    in
    let t = (max -. ray.origin.y) /. ray.direction.y in
    if check_cap t (radius_at max) then (RTCIntersection.build t shape trail) :: xs' else xs'

let intersect (shape:RTCShape.t) ?(trail=[]) (r:RTCRay.t) afn bfn cfn radius_at =
  let (minimum, maximum, closed) = parameters_of shape in
  let a = afn r in
  let b = bfn r in
  let xs =
    if (abs_float a) < RTCConst.epsilon then
      if (abs_float b) < RTCConst.epsilon then
        []
      else
        let c = cfn r in
        let t = (-.c) /. (2. *. b) in
        [ RTCIntersection.build t shape trail ]
    else
      let c = cfn r in
      let disc = b ** 2. -. 4. *. a *. c in
      if disc < 0. then
        []
      else
        let root = sqrt disc in
        let t0 = (-.b -. root) /. (2. *. a)
        and t1 = (-.b +. root) /. (2. *. a) in
        let xs =
          let y = r.origin.y +. t1 *. r.direction.y in
          if minimum < y && y < maximum then [ RTCIntersection.build t1 shape trail ] else []
        in
        let y = r.origin.y +. t0 *. r.direction.y in
        if minimum < y && y < maximum then
          (RTCIntersection.build t0 shape trail) :: xs
        else xs
  in
  RTCIntersection.list (intersect_caps trail shape minimum maximum closed r radius_at xs)

let normal_at shape (point:RTCTuple.t) nfn =
  let (minimum, maximum, _) = parameters_of shape in
  let dist = point.x ** 2. +. point.z ** 2. in
  if dist < 1. && point.y >= maximum -. RTCConst.epsilon then
    RTCTuple.vector 0. 1. 0.
  else if dist < 1. && point.y <= minimum +. RTCConst.epsilon then
    RTCTuple.vector 0. (-1.) 0.
  else
    nfn point
