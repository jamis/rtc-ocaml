type t = { shapes : RTCShape.t list; lights : RTCLight.point_light list }
type x_t = RTCShape.t RTCIntersection.t

let build ?(shapes=[]) ?(lights=[]) () = { shapes; lights }

let intersect (w:t) (r:RTCRay.t) =
  let rec aux xs = function
    | [] -> xs
    | shape :: shapes ->
      let xs' = RTCShape.intersect shape r in
      match xs' with
      | [] -> aux xs shapes
      | _  -> aux (List.merge (fun (a:x_t) (b:x_t) -> compare a.t b.t) xs xs') shapes
  in
  aux [] w.shapes

let is_shadowed (w:t) (light:RTCLight.point_light) (point:RTCTuple.t) =
  let v = RTCTuple.subtract light.position point in
  let distance = RTCTuple.mag v in
  let direction = RTCTuple.norm v in
  let r = RTCRay.build point direction in
  let xs = intersect w r in
  let allow (x:RTCShape.t RTCIntersection.t) = x.shape.shadow in
  match RTCIntersection.hit ~allow:allow xs with
  | Some hit when hit.t < distance -> true
  | _ -> false

let rec shade_hit (w:t) (c:RTCComps.t) remaining =
  let rec collect acc = function
    | [] -> acc
    | light :: lights ->
      let shadowed = is_shadowed w light c.point in
      let transform = RTCShape.world_to_object c.shape in
      let result = RTCMaterial.lighting c.shape.material transform light c.point c.eyev c.normalv shadowed in
      collect (RTCColor.add acc result) lights
  in
  let surface = collect (RTCColor.black) w.lights in
  let reflected = reflected_color w c remaining in
  let transmitted = refracted_color w c remaining in
  let material = c.shape.material in
  if material.reflective > 0. && material.transparency > 0. then
    let reflectance = RTCComps.schlick c in
    let reflected' = RTCColor.mults reflected reflectance in
    let transmitted' = RTCColor.mults transmitted (1. -. reflectance) in
    RTCColor.add (RTCColor.add surface reflected') transmitted'
  else
    RTCColor.add (RTCColor.add surface reflected) transmitted

and color_at (w:t) (r:RTCRay.t) remaining =
  let xs = intersect w r in
  match RTCIntersection.hit xs with
  | None -> RTCColor.black
  | Some hit ->
    let comps = RTCComps.prepare hit r xs in
    shade_hit w comps remaining

and reflected_color (w:t) (c:RTCComps.t) remaining =
  if (abs_float c.shape.material.reflective) < RTCConst.epsilon || remaining < 1 then
    RTCColor.black
  else
    let reflect_ray = RTCRay.build c.point c.reflectv in
    let color = color_at w reflect_ray (remaining - 1) in
    RTCColor.mults color c.shape.material.reflective

and refracted_color (w:t) (c:RTCComps.t) remaining =
  if (abs_float c.shape.material.transparency) < RTCConst.epsilon || remaining < 1 then
    RTCColor.black
  else
    let n_ratio = c.n1 /. c.n2 in
    let cos_i = RTCTuple.dot c.eyev c.normalv in
    let sin2_t = n_ratio ** 2. *. (1. -. cos_i ** 2.) in
    if sin2_t > 1. then
      RTCColor.black
    else
      let cos_t = sqrt(1. -. sin2_t) in
      let direction =
        let normal_scaled = RTCTuple.mults c.normalv (n_ratio *. cos_i -. cos_t) in
        let eye_scaled = RTCTuple.mults c.eyev n_ratio in
        RTCTuple.subtract normal_scaled eye_scaled
      in
      let refract_ray = RTCRay.build c.under_point direction in
      let color = color_at w refract_ray (remaining - 1) in
      RTCColor.mults color c.shape.material.transparency
