type 'a t = { t : float; shape : 'a }
type 'a xslist = 'a t list

let build t shape = { t; shape }
let list (xs : 'a xslist) = List.sort compare xs

let hit (xs : 'a xslist) = List.find_opt (fun x -> x.t >= 0.) xs
