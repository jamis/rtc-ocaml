type 'a t = { t : float; shape : 'a; trail : 'a list; u : float; v : float }
type 'a xslist = 'a t list

let build ?(u=0.) ?(v=0.) t shape trail = { t; shape; trail; u; v }
let list (xs : 'a xslist) = List.sort (fun a b -> (compare a.t b.t)) xs

let hit ?(allow=fun x -> true) (xs : 'a xslist) =
  let finder x = x.t >= 0. && (allow x) in
  List.find_opt finder xs
