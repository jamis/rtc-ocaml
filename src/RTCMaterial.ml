type t = { color : RTCColor.t;
           pattern : RTCPattern.t option;
           ambient : float;
           diffuse : float;
           specular : float;
           shininess : float }

let build ?(color=RTCColor.build 1. 1. 1.)
          ?(pattern=None)
          ?(ambient=0.1)
          ?(diffuse=0.9)
          ?(specular=0.9)
          ?(shininess=200.0)
          () =
  { color; pattern; ambient; diffuse; specular; shininess }

let equal m1 m2 =
  (RTCColor.equal m1.color m2.color) &&
  (m1.ambient = m2.ambient) &&
  (m1.diffuse = m2.diffuse) &&
  (m1.specular = m2.specular) &&
  (m1.shininess = m2.shininess)

let lighting m transform (light : RTCLight.point_light) point eyev normalv shadowed =
  let color = match m.pattern with
    | None -> m.color
    | Some pattern -> RTCPattern.at_object pattern transform point
  in
  let effective_color = RTCColor.mult color light.intensity in
  let lightv = RTCTuple.norm (RTCTuple.subtract light.position point) in
  let ambient = RTCColor.mults effective_color m.ambient in
  if shadowed then
    ambient
  else
    let light_dot_normal = RTCTuple.dot lightv normalv in
    if light_dot_normal < 0. then
      ambient
    else
      let diffuse = RTCColor.mults effective_color (m.diffuse *. light_dot_normal) in
      let reflectv = RTCTuple.reflect (RTCTuple.neg lightv) normalv in
      let reflect_dot_eye = RTCTuple.dot reflectv eyev in
      if reflect_dot_eye <= 0. then
        RTCColor.add ambient diffuse
      else
        let factor = reflect_dot_eye ** m.shininess in
        let specular = RTCColor.mults light.intensity (m.specular *. factor) in
        RTCColor.add ambient (RTCColor.add diffuse specular)
