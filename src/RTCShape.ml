type test_shape_data = { mutable ray : RTCRay.t option }

type shape_t =
  | Sphere
  | Plane
  | Cube
  | Cylinder of float * float * bool (* minimum, maximum, closed *)
  | Cone of float * float * bool     (* minimum, maximum, closed *)
  | Group of t list (* child elements *)
  | TestShape of test_shape_data

and intersect_t = t -> ?trail:t list -> RTCRay.t -> t RTCIntersection.xslist
and normal_t = t -> RTCTuple.t -> RTCTuple.t
and t = { shape : shape_t;
          transform : RTCMatrix.t;
          inverse_transform : RTCMatrix.t;
          inverse_transpose_transform : RTCMatrix.t;
          material : RTCMaterial.t option;
          local_intersect : intersect_t;
          local_normal_at : normal_t;
          shadow : bool }

let build (shape : shape_t) (isect : intersect_t) (normal : normal_t)=
  { shape;
    transform=RTCMatrix.identity;
    inverse_transform=RTCMatrix.identity;
    inverse_transpose_transform=RTCMatrix.identity;
    material=None;
    local_intersect=isect;
    local_normal_at=normal;
    shadow=true }

let transform (shape : t) transform =
  let inverse_transform = RTCMatrix.inverse transform in
  let inverse_transpose_transform = RTCMatrix.transpose inverse_transform in
  { shape with transform; inverse_transform; inverse_transpose_transform }

let texture (shape : t) material = { shape with material=Some material }

(* implements "inherited" materials, by checking the shape first, and
   then each element of the trail for a valid material *)
let material ?(trail=[]) (shape : t) =
  let rec loop head tail =
    match head.material with
    | None ->
      ( match tail with
        | [] -> RTCMaterial.build ()
        | head' :: tail' -> loop head' tail' )
    | Some m -> m
  in
  loop shape trail

let intersect (shape : t) ?(trail=[]) (ray : RTCRay.t) =
  let ray2 = RTCRay.transform ray shape.inverse_transform in
  shape.local_intersect shape ~trail:trail ray2

let world_to_object (shape : t) (trail : t list) (wpoint : RTCTuple.t) =
  let rec loop point = function
    | [] -> point
    | parent :: parents ->
      RTCMatrix.tmult parent.inverse_transform (loop point parents)
  in
  loop wpoint (shape :: trail)

let normal_to_world (shape : t) (trail : t list) (normal : RTCTuple.t) =
  let rec loop normal = function
    | [] -> normal
    | shape :: shapes ->
      let n = RTCMatrix.tmult shape.inverse_transpose_transform normal in
      let n' = RTCTuple.norm (RTCTuple.vector n.x n.y n.z) in
      loop n' shapes
  in
  loop normal (shape :: trail)

let normal_at (shape : t) (trail : t list) (wpoint : RTCTuple.t) =
  let opoint = world_to_object shape trail wpoint in
  let onormal = shape.local_normal_at shape opoint in
  normal_to_world shape trail onormal
