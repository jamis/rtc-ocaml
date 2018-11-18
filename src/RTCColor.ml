type t = { red : float; green : float; blue : float }

let build red green blue = { red; green; blue }

let black = { red = 0.; green = 0.; blue = 0. }
let white = { red = 1.; green = 1.; blue = 1. }

let equal a b = let red_diff = abs_float(a.red -. b.red) in
                let green_diff = abs_float(a.green -. b.green) in
                let blue_diff = abs_float(a.blue -. b.blue) in
                red_diff < RTCConst.epsilon &&
                green_diff < RTCConst.epsilon &&
                blue_diff < RTCConst.epsilon

let add a b = build (a.red +. b.red) (a.green +. b.green) (a.blue +. b.blue)
let subtract a b = build (a.red -. b.red) (a.green -. b.green) (a.blue -. b.blue)
let mults a scalar = build (a.red *. scalar) (a.green *. scalar) (a.blue *. scalar)
let mult a b = build (a.red *. b.red) (a.green *. b.green) (a.blue *. b.blue)
