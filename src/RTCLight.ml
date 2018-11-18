type point_light = { intensity : RTCColor.t; position : RTCTuple.t }

let point position intensity = { intensity; position }
