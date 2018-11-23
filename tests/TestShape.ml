open OUnit2
open TestAssertions

let test_shape () =
  let intersect (shape:RTCShape.t) ?(trail=[]) r : RTCShape.t RTCIntersection.xslist =
    match shape.shape with
    | RTCShape.TestShape data -> data.ray <- Some r; []
    | _ -> failwith "shouldn't ever get here"
  in
  let normal_at (shape:RTCShape.t) (point:RTCTuple.t) = RTCTuple.vector point.x point.y point.z in
  RTCShape.build (TestShape {ray=None}) intersect normal_at

let tests =
  "Shapes" >:::
  [
    "The default transformation" >::
    (fun test_ctxt ->
      let s = test_shape () in
      assert (RTCMatrix.equal s.transform RTCMatrix.identity));

    "Assigning a transformation" >::
    (fun test_ctxt ->
      let s = test_shape () in
      let t = RTCTransform.translation 2. 3. 4. in
      let s' = RTCShape.transform s t in
      assert (RTCMatrix.equal s'.transform t));

    "The default material" >::
    (fun test_ctxt ->
      let s = test_shape () in
      assert_equal None s.material);

    "The querying the material" >::
    (fun test_ctxt ->
      let s = test_shape () in
      let m = RTCShape.material s in
      assert_equal ~cmp:RTCMaterial.equal m (RTCMaterial.build ()));

    "Assigning a material" >::
    (fun test_ctxt ->
      let s = test_shape () in
      let m = RTCMaterial.build ~ambient:1.0 () in
      let s' = RTCShape.texture s m in
      assert_equal ~cmp:RTCMaterial.equal m (RTCShape.material s'));

    "Intersecting a scaled shape with a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = test_shape () in
      let s' = RTCShape.transform s (RTCTransform.scaling 2. 2. 2.) in
      let _ = RTCShape.intersect s' r in
      match s'.shape with
      | TestShape {ray=Some saved_ray} ->
        assert (RTCTuple.equal saved_ray.origin (RTCTuple.point 0. 0. (-2.5)));
        assert (RTCTuple.equal saved_ray.direction (RTCTuple.vector 0. 0. 0.5))
      | _ -> failwith "should have saved the ray");

    "Intersecting a translated shape with a ray" >::
    (fun test_ctxt ->
      let r = RTCRay.build (RTCTuple.point 0. 0. (-5.)) (RTCTuple.vector 0. 0. 1.) in
      let s = test_shape () in
      let s' = RTCShape.transform s (RTCTransform.translation 5. 0. 0.) in
      let _ = RTCShape.intersect s' r in
      match s.shape with
      | TestShape {ray=Some saved_ray} ->
        assert (RTCTuple.equal saved_ray.origin (RTCTuple.point (-5.) 0. (-5.)));
        assert (RTCTuple.equal saved_ray.direction (RTCTuple.vector 0. 0. 1.))
      | _ -> failwith "should have saved the ray");

    "Computing the normal on a translated shape" >::
    (fun test_ctxt ->
      let s = RTCShape.transform (test_shape ()) (RTCTransform.translation 0. 1. 0.) in
      let n = RTCShape.normal_at s [] (RTCTuple.point 0. 1.70711 (-0.70711)) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 0.70711 (-0.70711))));

    "Computing the normal on a transformed shape" >::
    (fun test_ctxt ->
      let tx = RTCMatrix.mult (RTCTransform.scaling 1. 0.5 1.) (RTCTransform.rotation_z (Float.pi /. 5.)) in
      let s = RTCShape.transform (test_shape ()) tx in
      let n = RTCShape.normal_at s [] (RTCTuple.point 0. (sqrt(2.)/.2.) (-.sqrt(2.)/.2.)) in
      assert (RTCTuple.equal n (RTCTuple.vector 0. 0.97014 (-0.24254))));

    "Converting a point from world to object space" >::
    (fun test_ctxt ->
      let s = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 5. 0. 0.) in
      let g2 = RTCShape.transform (RTCGroup.build ~children:[s] ()) (RTCTransform.scaling 2. 2. 2.) in
      let g1 = RTCShape.transform (RTCGroup.build ~children:[g2] ()) (RTCTransform.rotation_y (Float.pi /. 2.)) in
      let p = RTCShape.world_to_object s [g2; g1] (RTCTuple.point (-2.) 0. (-10.)) in
      assert_tuple_equal p (RTCTuple.point 0. 0. (-1.)));

    "Converting a normal from object to world space" >::
    (fun test_ctxt ->
      let s = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 5. 0. 0.) in
      let g2 = RTCShape.transform (RTCGroup.build ~children:[s] ()) (RTCTransform.scaling 1. 2. 3.) in
      let g1 = RTCShape.transform (RTCGroup.build ~children:[g2] ()) (RTCTransform.rotation_y (Float.pi /. 2.)) in
      let r3d3 = sqrt(3.) /. 3. in
      let shape_p = RTCTuple.point r3d3 r3d3 r3d3 in
      let n = RTCShape.normal_to_world s [g2; g1] shape_p in
      assert_tuple_equal n (RTCTuple.vector 0.2857 0.4286 (-0.8571)));

    "Finding the normal on a child object" >::
    (fun test_ctxt ->
      let s = RTCShape.transform (RTCSphere.build ()) (RTCTransform.translation 5. 0. 0.) in
      let g2 = RTCShape.transform (RTCGroup.build ~children:[s] ()) (RTCTransform.scaling 1. 2. 3.) in
      let g1 = RTCShape.transform (RTCGroup.build ~children:[g2] ()) (RTCTransform.rotation_y (Float.pi /. 2.)) in
      let world_p = RTCTuple.point 1.7321 1.1547 (-5.5774) in
      let n = RTCShape.normal_at s [g2; g1] world_p in
      assert_tuple_equal n (RTCTuple.vector 0.2857 0.4286 (-0.8571)));
  ]
