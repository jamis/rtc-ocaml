let parameters_of (shape:RTCShape.t) = match shape.shape with
  | Cylinder (minimum, maximum, closed) -> (minimum, maximum, closed)
  | _ -> failwith "expected a cylinder"

let intersect_caps shape min max closed (ray:RTCRay.t) xs =
  let check_cap t =
    let x = ray.origin.x +. t *. ray.direction.x
    and z = ray.origin.z +. t *. ray.direction.z in
    (x ** 2. +. z ** 2.) <= 1.
  in
  if (not closed) || ((abs_float ray.direction.y) < RTCConst.epsilon) then
    xs
  else
    let xs' =
      let t = (min -. ray.origin.y) /. ray.direction.y in
      if check_cap t then (RTCIntersection.build t shape) :: xs else xs
    in
    let t = (max -. ray.origin.y) /. ray.direction.y in
    if check_cap t then (RTCIntersection.build t shape) :: xs' else xs'

let build ?(minimum=Float.neg_infinity) ?(maximum=Float.infinity) ?(closed=false) () =
  let local_intersect (shape:RTCShape.t) (r:RTCRay.t) =
    let (minimum, maximum, closed) = parameters_of shape in
    let a = r.direction.x ** 2. +. r.direction.z ** 2. in
    let xs =
      if a >= RTCConst.epsilon then
        let b = 2. *. r.origin.x *. r.direction.x +.
                2. *. r.origin.z *. r.direction.z
        and c = r.origin.x ** 2. +. r.origin.z ** 2. -. 1. in
        let disc = b ** 2. -. 4. *. a *. c in
        if disc < 0. then
          []
        else
          let root = sqrt disc in
          let t0 = (-.b -. root) /. (2. *. a)
          and t1 = (-.b +. root) /. (2. *. a) in
          let xs =
            let y = r.origin.y +. t1 *. r.direction.y in
            if minimum < y && y < maximum then [ RTCIntersection.build t1 shape ] else []
          in
          let y = r.origin.y +. t0 *. r.direction.y in
          if minimum < y && y < maximum then
            (RTCIntersection.build t0 shape) :: xs
          else xs
      else []
    in
    RTCIntersection.list (intersect_caps shape minimum maximum closed r xs)
  in
  let local_normal_at shape (point:RTCTuple.t) =
    let (minimum, maximum, _) = parameters_of shape in
    let dist = point.x ** 2. +. point.z ** 2. in
    if dist < 1. && point.y >= maximum -. RTCConst.epsilon then
      RTCTuple.vector 0. 1. 0.
    else if dist < 1. && point.y <= minimum +. RTCConst.epsilon then
      RTCTuple.vector 0. (-1.) 0.
    else
      RTCTuple.vector point.x 0. point.z
  in
  RTCShape.build (Cylinder (minimum, maximum, closed)) local_intersect local_normal_at
