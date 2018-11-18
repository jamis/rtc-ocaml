let tick (grav, wind) (pos, vel) =
  let pos' = RTCTuple.add pos vel in
  let vel' = RTCTuple.add vel (RTCTuple.add grav wind) in
  (pos', vel')

let rec run (c:RTCCanvas.t) clr env ((pos:RTCTuple.t), _ as proj) =
  if pos.y <= 0. then c
  else let y = c.height - (int_of_float (pos.y +. 0.5)) in
       let x = int_of_float (pos.x +. 0.5) in
  begin
    Printf.printf "(x,y) = (%d,%d)\n" x y;
    if y >= 0 && y < c.height && x >= 0 && x < c.width then RTCCanvas.write_pixel c x y clr;
    run c clr env (tick env proj)
  end

let () =
  let grav = RTCTuple.vector 0. (-.0.1) 0. in
  let wind = RTCTuple.vector (-.0.01) 0. 0. in
  let pos  = RTCTuple.point 0. 1. 0. in
  let vel  = RTCTuple.mults (RTCTuple.norm (RTCTuple.vector 1. 1.8 0.)) 11.25 in
  let c    = RTCCanvas.build 900 550 in
  let clr  = RTCColor.build 1. 0.7 0.7 in
  let ppm  = RTCCanvas.to_ppm (run c clr (grav, wind) (pos, vel)) in
  let f    = open_out "trajectory.ppm" in
  output_string f ppm
