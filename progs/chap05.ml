let draw_sphere sphere ray_z wall_z canvas_size =
  let ray_origin = RTCTuple.point 0. 0. ray_z in
  let wall_size = 2. *. (wall_z -. ray_z) /. (abs_float ray_z) +. 1. in
  let pixel_size = wall_size /. (float_of_int canvas_size) in
  let half = wall_size /. 2. in
  let color = RTCColor.build 1. 0. 0. in
  let draw_hit canvas x y = function
    | None -> ()
    | Some _ -> RTCCanvas.write_pixel canvas x y color
  in
  let rec render_pixel canvas y = function
    | x when x >= canvas_size -> render_row canvas (y+1)
    | x -> let world_y = half -. pixel_size *. (float_of_int y) in
           let world_x = (-.half) +. pixel_size *. (float_of_int x) in
           let position = RTCTuple.point world_x world_y wall_z in
           let direction = RTCTuple.norm (RTCTuple.subtract position ray_origin) in
           let r = RTCRay.build ray_origin direction in
           let xs = RTCShape.intersect sphere r in
           draw_hit canvas x y (RTCIntersection.hit xs);
           render_pixel canvas y (x+1)
  and render_row canvas = function
    | x when x >= canvas_size -> canvas
    | y -> render_pixel canvas y 0
  in
  render_row (RTCCanvas.build canvas_size canvas_size) 0

let run () =
  let sphere = RTCSphere.build () in
  let canvas = draw_sphere sphere (-5.) 10. 100 in
  let ppm = RTCCanvas.to_ppm canvas in
  let f = open_out "05-sphere.ppm" in
  output_string f ppm;
  Printf.printf "wrote `05-sphere.ppm'\n"
