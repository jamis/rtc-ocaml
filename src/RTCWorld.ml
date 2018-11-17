type world = { shapes : RTCSphere.shape list; lights : RTCLight.point_light list }
type x_t = RTCIntersection.intersection

let build ?(shapes=[]) ?(lights=[]) () = { shapes; lights }

let intersect (w:world) (r:RTCRay.ray) =
  let rec aux xs = function
    | [] -> xs
    | shape :: shapes ->
      let xs' = RTCSphere.intersect shape r in
      match xs' with
      | [] -> aux xs shapes
      | _  -> aux (List.merge (fun (a:x_t) (b:x_t) -> compare a.t b.t) xs xs') shapes
  in
  aux [] w.shapes

let shade_hit (w:world) (c:RTCComps.comps) =
  let rec collect acc = function
    | [] -> acc
    | light :: lights ->
      let result = RTCMaterial.lighting c.shape#material light c.point c.eyev c.normalv in
      collect (RTCColor.add acc result) lights
  in
  collect (RTCColor.black) w.lights

let color_at (w:world) (r:RTCRay.ray) =
  let xs = intersect w r in
  match RTCIntersection.hit xs with
  | None -> RTCColor.black
  | Some hit ->
    let comps = RTCComps.prepare hit r in
    shade_hit w comps
