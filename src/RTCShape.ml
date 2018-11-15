class virtual shape =
  object (self)
    val mutable transform = RTCMatrix.identity
    val mutable inverse_transform = RTCMatrix.identity

    method transform = transform
    method inverse_transform = inverse_transform

    method set_transform m =
      transform <- m;
      inverse_transform <- RTCMatrix.inverse m
  end
