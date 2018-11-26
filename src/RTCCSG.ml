let intersection_allowed_union lhit inl inr =
  (lhit && (not inr)) || ((not lhit) && (not inl))

let intersection_allowed_intersect lhit inl inr =
  (lhit && inr) || ((not lhit) && inl)

let intersection_allowed_difference lhit inl inr =
  (lhit && (not inr)) || ((not lhit) && inl)

let filter_intersections (shape:RTCShape.t) xs =
  let (left, right, rule) = match shape.shape with
    | Union (l, r, fn) -> (l, r, fn)
    | Intersect (l, r, fn) -> (l, r, fn)
    | Difference (l, r, fn) -> (l, r, fn)
    | _ -> failwith "expected a CSG operate to filter_intersections"
  in
  let rec is_included (child:RTCShape.t) (parent:RTCShape.t) =
    match parent.shape with
    | Group children -> List.exists (is_included child) children
    | Union (l, r, _) -> (is_included child l) || (is_included child r)
    | Intersect (l, r, _) -> (is_included child l) || (is_included child r)
    | Difference (l, r, _) -> (is_included child l) || (is_included child r)
    | _ -> parent == child
  in
  let rec loop inl inr acc = function
    | [] -> List.rev acc
    | (i:RTCShape.t RTCIntersection.t) :: xs' ->
      let lhit = is_included i.shape left in
      let acc' = if rule lhit inl inr then i :: acc else acc in
      let (inl', inr') = if lhit then (not inl, inr) else (inl, not inr) in
      loop inl' inr' acc' xs'
  in
  loop false false [] xs

let build operation =
  let local_intersect (shape:RTCShape.t) ?(trail=[]) (r:RTCRay.t) =
    let (left, right) = match shape.shape with
      | Union (l, r, _) -> (l, r)
      | Intersect (l, r, _) -> (l, r)
      | Difference (l, r, _) -> (l, r)
      | _ -> failwith "expected CSG object in local_intersect"
    in
    let leftxs = RTCShape.intersect left ~trail:(shape :: trail) r in
    let rightxs = RTCShape.intersect right ~trail:(shape :: trail) r in
    let xs = RTCIntersection.list (List.rev_append leftxs rightxs) in
    filter_intersections shape xs
  in
  let local_normal_at ?(hit=None) shape (point:RTCTuple.t) = failwith "CSG has no normal" in
  RTCShape.build operation local_intersect local_normal_at

let union left right = build (Union (left, right, intersection_allowed_union))
let intersection left right = build (Intersect (left, right, intersection_allowed_intersect))
let difference left right = build (Difference (left, right, intersection_allowed_difference))
