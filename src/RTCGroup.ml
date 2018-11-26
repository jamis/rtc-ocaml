let get_children (g:RTCShape.t) =
  match g.shape with
  | Group cs -> cs
  | _ -> failwith "is_empty requires a Group argument"

let build ?(children=[]) () =
  let local_intersect shape ?(trail=[]) (r:RTCRay.t) =
    let rec loop xs = function
      | [] -> xs
      | child :: children ->
        let xs' = RTCShape.intersect child ~trail:(shape :: trail) r in
        loop (List.rev_append xs' xs) children
    in
    RTCIntersection.list (loop [] (get_children shape))
  in
  let local_normal_at ?(hit=None) shape (point:RTCTuple.t) = failwith "groups have no normal" in
  RTCShape.build (Group children) local_intersect local_normal_at

let is_empty (g:RTCShape.t) = (List.length (get_children g)) = 0
