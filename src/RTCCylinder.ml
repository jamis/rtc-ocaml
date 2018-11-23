let build ?(minimum=Float.neg_infinity) ?(maximum=Float.infinity) ?(closed=false) () =
  let local_intersect (shape:RTCShape.t) ?(trail=[]) (r:RTCRay.t) =
    let afn (r:RTCRay.t) = r.direction.x ** 2. +. r.direction.z ** 2. in
    let bfn (r:RTCRay.t) = 2. *. r.origin.x *. r.direction.x +.
                           2. *. r.origin.z *. r.direction.z
    in
    let cfn (r:RTCRay.t) = r.origin.x ** 2. +. r.origin.z ** 2. -. 1. in
    RTCConic.intersect shape ~trail:trail r afn bfn cfn (fun _ -> 1.)
  in
  let local_normal_at shape (point:RTCTuple.t) =
    let nfn (point:RTCTuple.t) = RTCTuple.vector point.x 0. point.z in
    RTCConic.normal_at shape point nfn
  in
  RTCShape.build (Cylinder (minimum, maximum, closed)) local_intersect local_normal_at
