type t = { t : float;
           shape : RTCShape.t;
           trail : RTCShape.t list;
           point : RTCTuple.t;
           under_point : RTCTuple.t;
           eyev : RTCTuple.t;
           normalv : RTCTuple.t;
           reflectv : RTCTuple.t;
           inside : bool;
           n1 : float;
           n2 : float; }

let prepare (i:RTCShape.t RTCIntersection.t) r (xs:RTCShape.t RTCIntersection.xslist) =
  let point = RTCRay.position r i.t in
  let eyev = RTCTuple.neg r.direction in
  let normalv = RTCShape.normal_at ~hit:(Some i) i.shape i.trail point in
  let inside = (RTCTuple.dot normalv eyev) < 0. in
  let normalv' = if inside then RTCTuple.neg normalv else normalv in
  let reflectv = RTCTuple.reflect r.direction normalv' in
  let point' = RTCTuple.add_mults point normalv' RTCConst.epsilon in
  let under_point = RTCTuple.add_mults point normalv' (-.RTCConst.epsilon) in
  let append_or_remove shape containers =
    let rec aux acc remaining found = match remaining with
      | [] ->
        let result = List.rev acc in
        if found then result else shape :: result
      | (el:RTCShape.t) :: els ->
        if el == shape then aux acc els true else aux (el :: acc) els found
    in
    aux [] containers false
  in
  let rec n1n2 (containers:RTCShape.t list) n1 n2 xlist = match xlist with
    | [] -> (n1, n2)
    | (x:RTCShape.t RTCIntersection.t) :: xlist ->
      let n1' = if x == i then match containers with
        | [] -> 1.
        | s :: _ -> (RTCShape.material s).refractive_index
        else n1
      in
      let containers' = append_or_remove x.shape containers in
      if x == i then match containers' with
        | [] -> (n1', 1.)
        | s :: _ -> (n1', (RTCShape.material s).refractive_index)
        else n1n2 containers' n1' n2 xlist
  in
  let (n1, n2) = n1n2 [] 1. 1. xs in
  { t=i.t; shape=i.shape; trail=i.trail;
    point=point'; under_point;
    eyev; normalv=normalv'; reflectv;
    inside; n1; n2 }

let schlick (comps:t) =
  let reflectance cos_i =
    let r0 = ((comps.n1 -. comps.n2) /. (comps.n1 +. comps.n2)) ** 2. in
    r0 +. (1. -. r0) *. (1. -. cos_i) ** 5.
  in
  let cos_i = RTCTuple.dot comps.eyev comps.normalv in
  (* total internal reflection if n1 > n2 *)
  if comps.n1 > comps.n2 then
    let n = comps.n1 /. comps.n2 in
    let sin2_t = n ** 2. *. (1. -. cos_i ** 2.) in
    if sin2_t > 1. then
      1.
    else
      let cos_t = sqrt(1. -. sin2_t) in
      reflectance cos_t
  else
    reflectance cos_i

let material (comps:t) = RTCShape.material ~trail:comps.trail comps.shape
