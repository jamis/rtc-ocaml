let tick (grav, wind) (pos, vel) =
  let pos' = RTCTuple.add pos vel in
  let vel' = RTCTuple.add vel (RTCTuple.add grav wind) in
  (pos', vel')

let rec loop env ((pos:RTCTuple.t), _ as proj) =
  if pos.y <= 0. then ()
  else begin
    Printf.printf "(%f, %f)\n" pos.x pos.y;
    loop env (tick env proj)
  end

let run () =
  let grav = RTCTuple.vector 0. (-.0.1) 0. in
  let wind = RTCTuple.vector (-.0.01) 0. 0. in
  let pos  = RTCTuple.point 0. 1. 0. in
  let vel  = RTCTuple.norm (RTCTuple.vector 1. 1. 0.) in
  loop (grav, wind) (pos, vel)
