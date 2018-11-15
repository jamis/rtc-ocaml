type intersection = { t : float; shape : RTCShape.shape }
type xslist = intersection list

let build t shape = { t; shape }
let list (xs : xslist) = List.sort compare xs

let hit (xs : xslist) = List.find_opt (fun x -> x.t >= 0.) xs
