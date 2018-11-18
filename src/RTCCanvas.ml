type t = { width : int; height : int;
           pixels : RTCColor.t array array }

let build width height = { width; height; pixels = Array.make_matrix height width RTCColor.black }

let write_pixel c x y color = c.pixels.(y).(x) <- color
let pixel_at c x y = c.pixels.(y).(x)

let to_ppm (c:t) =
  let buffer = Buffer.create 1024 in
  let rec add_rows y =
    let rec add_pixels llen x =
      let add_comp llen value =
        let comp = max 0 (min 255 (int_of_float (value *. 255. +. 0.5))) in
        let comps = Printf.sprintf "%d" comp in
        let space_len = if llen > 0 then 1 else 0 in
        let comps_len = String.length comps in
        let max_len = llen + space_len + comps_len in
        let final_len = if max_len > 70 then comps_len else max_len in
        if max_len > 70 then Buffer.add_char buffer '\n'
        else if llen > 0 then Buffer.add_char buffer ' ';
        Buffer.add_string buffer comps;
        final_len
      in
      if x >= c.width then ()
      else let pixel = c.pixels.(y).(x) in
           let rlen = add_comp llen pixel.red in
           let glen = add_comp rlen pixel.green in
           let blen = add_comp glen pixel.blue in
           add_pixels blen (x+1)
    in
    if y >= c.height then Buffer.contents buffer
    else begin
      add_pixels 0 0;
      Buffer.add_char buffer '\n';
      add_rows (y+1)
    end
  in
  Buffer.add_string buffer (Printf.sprintf "P3\n%d %d\n255\n" c.width c.height);
  add_rows 0
