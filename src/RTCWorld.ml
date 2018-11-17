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

let is_shadowed (w:world) (light:RTCLight.point_light) (point:RTCTuple.tuple) =
  let v = RTCTuple.subtract light.position point in
  let distance = RTCTuple.mag v in
  let direction = RTCTuple.norm v in
  let r = RTCRay.build point direction in
  let xs = intersect w r in
  match RTCIntersection.hit xs with
  | Some hit when hit.t < distance -> true
  | _ -> false

let shade_hit (w:world) (c:RTCComps.comps) =
  let rec collect acc = function
    | [] -> acc
    | light :: lights ->
      let shadowed = is_shadowed w light c.point in
      let result = RTCMaterial.lighting c.shape#material light c.point c.eyev c.normalv shadowed in
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
