let draw_clock width height =
  let cx = width / 2 in
  let cy = height / 2 in
  let clr = RTCColor.build 1. 1. 1. in
  let twelve = RTCTuple.point 0. ((float_of_int cy) *. 0.8) 0. in
  let hour_inc = 2. *. Float.pi /. 12. in
  let rec loop canvas = function
    | 12 -> canvas
    | hour ->
        let tx = RTCTransform.rotation_z (hour_inc *. (float_of_int hour)) in
        let point = RTCMatrix.tmult tx twelve in
        let x = cx + (int_of_float point.x) in
        let y = cy - (int_of_float point.y) in
        RTCCanvas.write_pixel canvas x y clr;
        loop canvas (hour+1)
  in
  loop (RTCCanvas.build width height) 0

let run () =
  let ppm = RTCCanvas.to_ppm (draw_clock 100 100) in
  let f = open_out "04-clock.ppm" in
  output_string f ppm;
  Printf.printf "wrote `04-clock.ppm'\n"
