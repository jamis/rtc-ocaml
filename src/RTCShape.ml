class virtual shape =
  object (self)
    val mutable transform = RTCMatrix.identity
    val mutable inverse_transform = RTCMatrix.identity
    val mutable inverse_transpose_transform = RTCMatrix.identity
    val mutable material = RTCMaterial.build ()

    method transform = transform
    method inverse_transform = inverse_transform
    method inverse_transpose_transform = inverse_transpose_transform
    method material = material

    method set_transform m =
      transform <- m;
      inverse_transform <- RTCMatrix.inverse m;
      inverse_transpose_transform <- RTCMatrix.transpose inverse_transform

    method set_material m = material <- m
  end
