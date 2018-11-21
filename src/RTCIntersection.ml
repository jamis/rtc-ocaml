type 'a t = { t : float; shape : 'a }
type 'a xslist = 'a t list

let build t shape = { t; shape }
let list (xs : 'a xslist) = List.sort compare xs

let hit ?(allow=fun x -> true) (xs : 'a xslist) =
  let finder x = x.t >= 0. && (allow x) in
  List.find_opt finder xs
