open OUnit2
open TestAssertions

let point = RTCTuple.point
let vector = RTCTuple.vector

let triangles_obj = "
v -1 1 0
v -1 0 0
v 1 0 0
v 1 1 0

g FirstGroup
f 1 2 3
g SecondGroup
f 1 3 4
"

let stream_lines_of_string str =
  let lines = ref (String.split_on_char '\n' str) in
  let fetch_next_line _ =
    match !lines with
    | [] -> None
    | line :: remainder ->
      lines := remainder;
      Some line
  in
  Stream.from fetch_next_line

let tests =
  "OBJ Files" >:::
  [
    "Ignoring unrecognized lines" >::
    (fun test_ctxt ->
      let contents =
"There was a young lady named Bright
who traveled much faster than light.
She set out one day
in a relative way,
and came back the previous night."
      in
      let parser = RTCObjFile.parse (stream_lines_of_string contents) in
      assert_equal 5 parser.ignored_count);

    "Vertex records" >::
    (fun test_ctxt ->
      let contents =
"v -1 1 0
v -1.0000 0.5000 0.0000
v 1 0 0
v 1 1 0"
      in
      let parser = RTCObjFile.parse (stream_lines_of_string contents) in
      assert_tuple_equal parser.vertices.(1) (point (-1.) 1. 0.);
      assert_tuple_equal parser.vertices.(2) (point (-1.) 0.5 0.);
      assert_tuple_equal parser.vertices.(3) (point 1. 0. 0.);
      assert_tuple_equal parser.vertices.(4) (point 1. 1. 0.));

    "Parsing triangle faces" >::
    (fun test_ctxt ->
      let contents =
"v -1 1 0
v -1 0 0
v 1 0 0
v 1 1 0

f 1 2 3
f 1 3 4"
      in
      let parser = RTCObjFile.parse (stream_lines_of_string contents) in
      let g = parser.default_group in
      let children = RTCGroup.get_children g in
      let t1data = RTCTriangle.data (List.nth children 0) in
      let t2data = RTCTriangle.data (List.nth children 1) in
      assert_tuple_equal parser.vertices.(1) t1data.p1;
      assert_tuple_equal parser.vertices.(2) t1data.p2;
      assert_tuple_equal parser.vertices.(3) t1data.p3;
      assert_tuple_equal parser.vertices.(1) t2data.p1;
      assert_tuple_equal parser.vertices.(3) t2data.p2;
      assert_tuple_equal parser.vertices.(4) t2data.p3);

    "Triangulating polygons" >::
    (fun test_ctxt ->
      let contents =
"v -1 1 0
v -1 0 0
v 1 0 0
v 1 1 0
v 0 2 0

f 1 2 3 4 5"
      in
      let parser = RTCObjFile.parse (stream_lines_of_string contents) in
      let g = parser.default_group in
      let children = RTCGroup.get_children g in
      let t1 = RTCTriangle.data (List.nth children 0) in
      let t2 = RTCTriangle.data (List.nth children 1) in
      let t3 = RTCTriangle.data (List.nth children 2) in
      assert_tuple_equal parser.vertices.(1) t1.p1;
      assert_tuple_equal parser.vertices.(2) t1.p2;
      assert_tuple_equal parser.vertices.(3) t1.p3;
      assert_tuple_equal parser.vertices.(1) t2.p1;
      assert_tuple_equal parser.vertices.(3) t2.p2;
      assert_tuple_equal parser.vertices.(4) t2.p3;
      assert_tuple_equal parser.vertices.(1) t3.p1;
      assert_tuple_equal parser.vertices.(4) t3.p2;
      assert_tuple_equal parser.vertices.(5) t3.p3);

    "Triangles in groups" >::
    (fun test_ctxt ->
      let parser = RTCObjFile.parse (stream_lines_of_string triangles_obj) in
      let g1 = RTCObjFile.named_group parser "FirstGroup" in
      let g2 = RTCObjFile.named_group parser "SecondGroup" in
      let g1_children = RTCGroup.get_children g1 in
      let g2_children = RTCGroup.get_children g2 in
      let t1 = RTCTriangle.data (List.nth g1_children 0) in
      let t2 = RTCTriangle.data (List.nth g2_children 0) in
      assert_tuple_equal parser.vertices.(1) t1.p1;
      assert_tuple_equal parser.vertices.(2) t1.p2;
      assert_tuple_equal parser.vertices.(3) t1.p3;
      assert_tuple_equal parser.vertices.(1) t2.p1;
      assert_tuple_equal parser.vertices.(3) t2.p2;
      assert_tuple_equal parser.vertices.(4) t2.p3);

    "Converting an OBJ file to a group" >::
    (fun test_ctxt ->
      let parser = RTCObjFile.parse (stream_lines_of_string triangles_obj) in
      let g1 = RTCObjFile.named_group parser "FirstGroup" in
      let g2 = RTCObjFile.named_group parser "SecondGroup" in
      let g = RTCObjFile.to_group parser in
      let children = RTCGroup.get_children g in
      assert (List.mem g1 children);
      assert (List.mem g2 children));

    "Vertex normal records" >::
    (fun test_ctxt ->
      let contents = "
vn 0 0 1
vn 0.707 0 -0.707
vn 1 2 3"
      in
      let parser = RTCObjFile.parse (stream_lines_of_string contents) in
      assert_tuple_equal parser.normals.(1) (vector 0. 0. 1.);
      assert_tuple_equal parser.normals.(2) (vector 0.707 0. (-0.707));
      assert_tuple_equal parser.normals.(3) (vector 1. 2. 3.));

    "Faces with normals" >::
    (fun test_ctxt ->
      let contents = "
v 0 1 0
v -1 0 0
v 1 0 0

vn -1 0 0
vn 1 0 0
vn 0 1 0

f 1//3 2//1 3//2
f 1/0/3 2/102/1 3/14/2"
      in
      let parser = RTCObjFile.parse (stream_lines_of_string contents) in
      let children = RTCGroup.get_children parser.default_group in
      let t1 = RTCTriangle.data (List.nth children 0) in
      let t2 = RTCTriangle.data (List.nth children 1) in
      assert_tuple_equal parser.vertices.(1) t1.p1;
      assert_tuple_equal parser.vertices.(2) t1.p2;
      assert_tuple_equal parser.vertices.(3) t1.p3;
      assert_tuple_equal parser.normals.(3) t1.n1;
      assert_tuple_equal parser.normals.(1) t1.n2;
      assert_tuple_equal parser.normals.(2) t1.n3;
      assert_equal t1 t2);
  ]
