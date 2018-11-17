type camera = { hsize : int; vsize : int;
                field_of_view : float;
                transform : float array array;
                inverse_transform : float array array;
                pixel_size : float;
                half_width : float;
                half_height : float }

let build hsize vsize field_of_view transform =
  let half_view = tan (field_of_view /. 2.) in
  let aspect = (float_of_int hsize) /. (float_of_int vsize) in
  let wide = hsize > vsize in
  let half_width = if wide then half_view else half_view *. aspect in
  let half_height = if wide then half_view /. aspect else half_view in
  let pixel_size = (half_width *. 2.) /. (float_of_int hsize) in
  { hsize; vsize; field_of_view;
    transform; inverse_transform=(RTCMatrix.inverse transform);
    pixel_size; half_width; half_height }

let ray_for_pixel c px py =
  let xoffset = ((float_of_int px) +. 0.5) *. c.pixel_size in
  let yoffset = ((float_of_int py) +. 0.5) *. c.pixel_size in
  let world_x = c.half_width -. xoffset in
  let world_y = c.half_height -. yoffset in
  let pixel = RTCMatrix.tmult c.inverse_transform (RTCTuple.point world_x world_y (-1.)) in
  let origin = RTCMatrix.tmult c.inverse_transform (RTCTuple.point 0. 0. 0.) in
  let direction = RTCTuple.norm (RTCTuple.subtract pixel origin) in
  RTCRay.build origin direction

let render c w =
  let rec render_yx canvas y = function
    | x when x >= c.hsize -> render_y canvas (y+1)
    | x -> let ray = ray_for_pixel c x y in
           let color = RTCWorld.color_at w ray in
           RTCCanvas.write_pixel canvas x y color;
           render_yx canvas y (x+1)
  and render_y canvas = function
    | y when y >= c.vsize -> canvas
    | y -> render_yx canvas y 0
  in
  render_y (RTCCanvas.build c.hsize c.vsize) 0
