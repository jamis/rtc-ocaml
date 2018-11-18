type t = { origin : RTCTuple.t; direction : RTCTuple.t }

let build origin direction = { origin; direction }

let position r t = RTCTuple.add_mults r.origin r.direction t

let transform r m =
  let origin = RTCMatrix.tmult m r.origin in
  let direction = RTCMatrix.tmult m r.direction in
  build origin direction
