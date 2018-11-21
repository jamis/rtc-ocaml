type test_shape_data = { mutable ray : RTCRay.t option }

type shape_t =
  | Sphere
  | Plane
  | Cube
  | TestShape of test_shape_data

type intersect_t = t -> RTCRay.t -> t RTCIntersection.xslist
and normal_t = t -> RTCTuple.t -> RTCTuple.t
and t = { shape : shape_t;
          transform : RTCMatrix.t;
          inverse_transform : RTCMatrix.t;
          inverse_transpose_transform : RTCMatrix.t;
          material : RTCMaterial.t;
          local_intersect : intersect_t;
          local_normal_at : normal_t }

let build (shape : shape_t) (isect : intersect_t) (normal : normal_t)=
  { shape;
    transform=RTCMatrix.identity;
    inverse_transform=RTCMatrix.identity;
    inverse_transpose_transform=RTCMatrix.identity;
    material=RTCMaterial.build ();
    local_intersect=isect;
    local_normal_at=normal }

let transform (shape : t) transform =
  let inverse_transform = RTCMatrix.inverse transform in
  let inverse_transpose_transform = RTCMatrix.transpose inverse_transform in
  { shape with transform; inverse_transform; inverse_transpose_transform }

let texture (shape : t) material = { shape with material }

let intersect (shape : t) (ray : RTCRay.t) =
  let ray2 = RTCRay.transform ray shape.inverse_transform in
  shape.local_intersect shape ray2

let normal_at (shape : t) (wpoint : RTCTuple.t) =
  let opoint = RTCMatrix.tmult shape.inverse_transform wpoint in
  let onormal = shape.local_normal_at shape opoint in
  let wnormal = RTCMatrix.tmult shape.inverse_transpose_transform onormal in
  RTCTuple.norm (RTCTuple.vector wnormal.x wnormal.y wnormal.z)

let world_to_object (shape : t) (wpoint : RTCTuple.t) =
  RTCMatrix.tmult shape.inverse_transform wpoint
