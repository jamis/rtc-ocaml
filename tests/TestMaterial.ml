open OUnit2

let setup () = ( RTCMaterial.build (), RTCTuple.point 0. 0. 0.)

let with_setup name fn =
  let material = RTCMaterial.build () in
  let point = RTCTuple.point 0. 0. 0. in
  name >:: (fn material point)

let tests =
  "Materials" >:::
  [
    "The default material" >::
    (fun test_ctxt ->
      let m = RTCMaterial.build () in
      assert (RTCColor.equal m.color (RTCColor.build 1. 1. 1.));
      assert_equal m.ambient 0.1;
      assert_equal m.diffuse 0.9;
      assert_equal m.specular 0.9;
      assert_equal m.shininess 200.0);

    with_setup "Lighting with the eye between the light and the surface"
    (fun m position test_ctxt ->
      let eyev = RTCTuple.vector 0. 0. (-1.) in
      let normalv = RTCTuple.vector 0. 0. (-1.) in
      let light = RTCLight.point (RTCTuple.point 0. 0. (-10.)) (RTCColor.build 1. 1. 1.) in
      let result = RTCMaterial.lighting m light position eyev normalv false in
      assert (RTCColor.equal result (RTCColor.build 1.9 1.9 1.9)));

    with_setup "Lighting with the eye between light and surface, eye offset 45°"
    (fun m position test_ctxt ->
      let eyev = RTCTuple.vector 0. (sqrt(2.)/.2.) (-.sqrt(2.)/.2.) in
      let normalv = RTCTuple.vector 0. 0. (-1.) in
      let light = RTCLight.point (RTCTuple.point 0. 0. (-10.)) (RTCColor.build 1. 1. 1.) in
      let result = RTCMaterial.lighting m light position eyev normalv false in
      assert (RTCColor.equal result (RTCColor.build 1. 1. 1.)));

    with_setup "Lighting with eye opposite surface, light offset 45°"
    (fun m position test_ctxt ->
      let eyev = RTCTuple.vector 0. 0. (-1.) in
      let normalv = RTCTuple.vector 0. 0. (-1.) in
      let light = RTCLight.point (RTCTuple.point 0. 10. (-10.)) (RTCColor.build 1. 1. 1.) in
      let result = RTCMaterial.lighting m light position eyev normalv false in
      assert (RTCColor.equal result (RTCColor.build 0.7364 0.7364 0.7364)));

    with_setup "Lighting with eye in the path of the reflection vector"
    (fun m position test_ctxt ->
      let eyev = RTCTuple.vector 0. (-.sqrt(2.)/.2.) (-.sqrt(2.)/.2.) in
      let normalv = RTCTuple.vector 0. 0. (-1.) in
      let light = RTCLight.point (RTCTuple.point 0. 10. (-10.)) (RTCColor.build 1. 1. 1.) in
      let result = RTCMaterial.lighting m light position eyev normalv false in
      assert (RTCColor.equal result (RTCColor.build 1.6364 1.6364 1.6364)));

    with_setup "Lighting with the light behind the surface"
    (fun m position test_ctxt ->
      let eyev = RTCTuple.vector 0. 0. (-1.) in
      let normalv = RTCTuple.vector 0. 0. (-1.) in
      let light = RTCLight.point (RTCTuple.point 0. 0. 10.) (RTCColor.build 1. 1. 1.) in
      let result = RTCMaterial.lighting m light position eyev normalv false in
      assert (RTCColor.equal result (RTCColor.build 0.1 0.1 0.1)));

    with_setup "Lighting with the surface in shadow"
    (fun m position test_ctxt ->
      let eyev = RTCTuple.vector 0. 0. (-1.) in
      let normalv = RTCTuple.vector 0. 0. (-1.) in
      let light = RTCLight.point (RTCTuple.point 0. 0. (-10.)) (RTCColor.build 1. 1. 1.) in
      let result = RTCMaterial.lighting m light position eyev normalv true in
      assert (RTCColor.equal result (RTCColor.build 0.1 0.1 0.1)));
  ]
