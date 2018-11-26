let data (tri:RTCShape.t) =
  match tri.shape with
  | Triangle data -> data
  | _ -> failwith "expected a triangle"

let precompute p1 p2 p3 =
  let e1 = RTCTuple.subtract p2 p1 in
  let e2 = RTCTuple.subtract p3 p1 in
  let normal = RTCTuple.norm (RTCTuple.cross e2 e1) in
  (e1, e2, normal)

let local_intersect shape ?(trail=[]) (r:RTCRay.t) =
  let tri = data shape in
  let dir_cross_e2 = RTCTuple.cross r.direction tri.e2 in
  let det = RTCTuple.dot tri.e1 dir_cross_e2 in
  if (abs_float det) < RTCConst.epsilon then
    []
  else
    let f = 1. /. det in
    let p1_to_origin = RTCTuple.subtract r.origin tri.p1 in
    let u = f *. (RTCTuple.dot p1_to_origin dir_cross_e2) in
    if u < 0. || u > 1. then []
    else
      let origin_cross_e1 = RTCTuple.cross p1_to_origin tri.e1 in
      let v = f *. (RTCTuple.dot r.direction origin_cross_e1) in
      if v < 0. || (u +. v) > 1. then []
      else
        let t = f *. (RTCTuple.dot tri.e2 origin_cross_e1) in
        [ RTCIntersection.build ~u:u ~v:v t shape trail ]

let build p1 p2 p3 =
  let (e1, e2, normal) = precompute p1 p2 p3 in
  let data : RTCShape.tri_data = { p1; p2; p3; n1=normal; n2=normal; n3=normal; smooth=false; e1; e2; normal } in
  let local_normal_at ?(hit=None) shape (point:RTCTuple.t) = normal in
  RTCShape.build (Triangle data) local_intersect local_normal_at

let smooth p1 p2 p3 n1 n2 n3 =
  let (e1, e2, normal) = precompute p1 p2 p3 in
  let data : RTCShape.tri_data = { p1; p2; p3; n1; n2; n3; smooth=true; e1; e2; normal } in
  let local_normal_at ?(hit=None) shape (point:RTCTuple.t) =
    let (h:RTCShape.t RTCIntersection.t) = match hit with
      | None -> failwith "smooth triangles require hit information"
      | Some i -> i
    in
    let n2scaled = RTCTuple.mults data.n2 h.u
    and n3scaled = RTCTuple.mults data.n3 h.v
    and n1scaled = RTCTuple.mults data.n1 (1. -. h.u -. h.v) in
    RTCTuple.add n2scaled (RTCTuple.add n3scaled n1scaled)
  in
  RTCShape.build (Triangle data) local_intersect local_normal_at
