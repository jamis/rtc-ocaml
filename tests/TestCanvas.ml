open OUnit2

let lines str = Array.of_list (String.split_on_char '\n' str)

let flood (canvas:RTCCanvas.canvas) color =
  let rec flood_row y =
    if y >= canvas.height then ()
    else begin
      Array.fill canvas.pixels.(y) 0 canvas.width color;
      flood_row (y+1);
    end
  in
  flood_row 0

let tests =
  "Canvas" >:::
  [
    "Creating a canvas" >::
    (fun test_ctxt ->
      let c = RTCCanvas.build 10 20 in
      let is_black = RTCColor.equal RTCColor.black in
      assert_equal 10 c.width;
      assert_equal 20 c.height;
      assert (Array.for_all (Array.for_all is_black) c.pixels));

    "Writing pixels to a canvas" >::
    (fun test_ctxt ->
      let c = RTCCanvas.build 10 20 in
      let red = RTCColor.build 1. 0. 0. in
      RTCCanvas.write_pixel c 2 3 red;
      assert (RTCColor.equal red (RTCCanvas.pixel_at c 2 3)));

    "Constructing the PPM header" >::
    (fun test_ctxt ->
      let c = RTCCanvas.build 5 3 in
      let ppm = lines (RTCCanvas.to_ppm c) in
      assert_equal "P3" ppm.(0);
      assert_equal "5 3" ppm.(1);
      assert_equal "255" ppm.(2));

    "Constructing the PPM pixel data" >::
    (fun test_ctxt ->
      let c = RTCCanvas.build 5 3 in
      RTCCanvas.write_pixel c 0 0 (RTCColor.build 1.5 0. 0.);
      RTCCanvas.write_pixel c 2 1 (RTCColor.build 0. 0.5 0.);
      RTCCanvas.write_pixel c 4 2 (RTCColor.build (-.0.5) 0. 1.);
      let ppm = lines (RTCCanvas.to_ppm c) in
      assert_equal "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0" ppm.(3);
      assert_equal "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0" ppm.(4);
      assert_equal "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255" ppm.(5));

    "Splitting long lines in PPM files" >::
    (fun test_ctxt ->
      let c = RTCCanvas.build 10 2 in
      flood c (RTCColor.build 1. 0.8 0.6);
      let raw_ppm = RTCCanvas.to_ppm c in
      let ppm = lines raw_ppm in
      assert_equal "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204" ppm.(3);
      assert_equal "153 255 204 153 255 204 153 255 204 153 255 204 153" ppm.(4);
      assert_equal "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204" ppm.(5);
      assert_equal "153 255 204 153 255 204 153 255 204 153 255 204 153" ppm.(6));

    "PPM files are terminated by a newline" >::
    (fun test_ctxt ->
      let c = RTCCanvas.build 5 3 in
      let raw_ppm = RTCCanvas.to_ppm c in
      assert_equal '\n' (String.get raw_ppm ((String.length raw_ppm) - 1)));
  ]
