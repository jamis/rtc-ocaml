(* Routines for parsing (a subset of) Wavefront OBJ files *)

type t = { ignored_count : int;
           vertices : RTCTuple.t array;
           normals : RTCTuple.t array;
           default_group: RTCShape.t;
           groups: (string, RTCShape.t) Hashtbl.t }

type acc_t = { ignored_count : int;
               vertices : RTCTuple.t list;
               normals : RTCTuple.t list;
               faces : RTCShape.t list;
               finalv : RTCTuple.t array;
               finaln : RTCTuple.t array;
               current_group : string;
               groups: (string, RTCShape.t) Hashtbl.t }

let next_opt (stream:string Stream.t) =
  match (Stream.peek stream) with
  | None -> None
  | _ -> Some (Stream.next stream)

let parse (stream:string Stream.t) =
  (* compiles the current list of vertices into an array, and appends it
   * to the finalv array, which is used for vertex lookups by index *)
  let compile (acc:acc_t) =
    let vertices = [] and normals = [] in
    let finalv = match acc.vertices with
      | [] -> acc.finalv
      | v  -> Array.append acc.finalv (Array.of_list (List.rev v))
    in
    let finaln = match acc.normals with
      | [] -> acc.finaln
      | v -> Array.append acc.finaln (Array.of_list (List.rev v))
    in
    { acc with vertices; normals; finalv; finaln }
  in

  (* tokenizes the given line *)
  let scan_line line =
    let rec scan stream acc =
      let finalize_list = List.rev in
      match (Scanf.bscanf stream " %s " (fun el -> el :: acc)) with
        | exception Scanf.Scan_failure _ -> finalize_list acc
        | exception End_of_file -> finalize_list acc
        | "" :: remainder -> finalize_list remainder
        | items -> scan stream items
    in
    scan (Scanf.Scanning.from_string line) []
  in

  (* increments the ignored_count value *)
  let ignore_line r = { r with ignored_count=(r.ignored_count + 1) } in

  (* parses the arguments of a vertex command, adding a point to the vertices list *)
  let vertex_line (acc:acc_t) = function
    | [x; y; z] ->
      let pt = RTCTuple.point x y z in
      { acc with vertices=(pt :: acc.vertices) }
    | _ -> failwith "expected v to describe an x,y,z point"
  in

  (* parses the arguments of a vertex normal command, adding a vector to the
   * normals list *)
  let normal_line (acc:acc_t) = function
    | [x; y; z] ->
      let pt = RTCTuple.vector x y z in
      { acc with normals=(pt :: acc.normals) }
    | _ -> failwith "expected v to describe an x,y,z vector"
  in

  (* parses the arguments of a face command, adding one or more faces to
   * the faces list *)
  let face_line (acc:acc_t) args =
    let add_tri i1 i2 i3 (acc:acc_t) =
      let tri = match (i1, i2, i3) with
        | ((vi1, _, Some ni1), (vi2, _, Some ni2), (vi3, _, Some ni3)) ->
          let p1 = acc.finalv.(vi1)
          and p2 = acc.finalv.(vi2)
          and p3 = acc.finalv.(vi3) in
          let n1 = acc.finaln.(ni1)
          and n2 = acc.finaln.(ni2)
          and n3 = acc.finaln.(ni3) in
          RTCTriangle.smooth p1 p2 p3 n1 n2 n3
        | ((vi1, _, _), (vi2, _, _), (vi3, _, _)) ->
          let p1 = acc.finalv.(vi1)
          and p2 = acc.finalv.(vi2)
          and p3 = acc.finalv.(vi3) in
          RTCTriangle.build p1 p2 p3
      in
      { acc with faces=(tri :: acc.faces) }
    in
    let rec loop (acc:acc_t) = function
      | i1 :: i2 :: i3 :: [] ->
        add_tri i1 i2 i3 acc
      | i1 :: i2 :: i3 :: remainder ->
        loop (add_tri i1 i2 i3 acc) (i1 :: i3 :: remainder)
      | _ -> failwith "too few points"
    in
    loop (compile acc) args
  in

  (* finalizes the currently-aggregating group by creating a new group with
   * the current list of faces, and adding it to the groups hash table *)
  let finalize_current_group acc = match acc.faces with
    | [] -> acc
    | faces ->
      let reversed = List.rev faces in
      if Hashtbl.mem acc.groups acc.current_group then
        let group = Hashtbl.find acc.groups acc.current_group in
        let children = RTCGroup.get_children group in
        let combined = (List.rev children) @ reversed in
        let group' = RTCGroup.build ~children:combined () in
        Hashtbl.replace acc.groups acc.current_group group'
      else (
        let group = RTCGroup.build ~children:reversed () in
        Hashtbl.add acc.groups acc.current_group group
      );
      { acc with faces=[] }
  in

  (* parses the arguments of a group command. Ignores all but the first
   * group name. *)
  let start_group acc = function
    | [] -> failwith "expected a group name"
    | name :: _ -> { (finalize_current_group acc) with current_group=name }
  in

  (* parses a face argument in the form of "i" or "i//i" or "i/i/i" and
   * returns a triple (i, option, option) *)
  let parse_face_arg arg =
    match String.split_on_char '/' arg with
    | v :: _ :: "" :: _ -> ( int_of_string v, None, None )
    | v :: _ :: n :: _ -> ( int_of_string v, None, Some (int_of_string n) )
    | v :: _ -> ( int_of_string v, None, None )
    | _ -> failwith "invalid face specification"
  in

  (* parses a single line *)
  let parse_line acc line =
    match scan_line line with
      | [] -> acc
      | "v" :: args -> vertex_line acc (List.map float_of_string args)
      | "vn" :: args -> normal_line acc (List.map float_of_string args)
      | "f" :: args -> face_line acc (List.map parse_face_arg args)
      | "g" :: args -> start_group acc args
      | _ -> ignore_line acc
  in

  (* finalizes the parse by constructing a `t` return value from the
   * accumulator *)
  let finalize acc =
    let acc' = finalize_current_group acc in
    let default_group = match Hashtbl.find_opt acc'.groups "" with
      | None -> RTCGroup.build ()
      | Some group -> group
    in
    { ignored_count=acc'.ignored_count;
      vertices=acc'.finalv;
      normals=acc'.finaln;
      default_group;
      groups=acc'.groups }
  in

  (* loops over every line in the given stream *)
  let rec loop acc = function
    | None ->
      finalize (compile acc)
    | Some line ->
      loop (parse_line acc line) (next_opt stream)
  in

  loop { ignored_count=0;
         vertices=[];
         normals=[];
         faces=[];
         finalv=[| RTCTuple.origin |];
         finaln=[| RTCTuple.origin |];
         current_group="";
         groups=(Hashtbl.create 20) }
       (next_opt stream)

let parse_file filename =
  let stream_lines_of_channel channel =
    Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)
  in
  let channel = open_in filename in
  parse (stream_lines_of_channel channel)

let named_group (obj:t) name = Hashtbl.find obj.groups name

let to_group (obj:t) =
  match List.of_seq (Hashtbl.to_seq_values obj.groups) with
  | [] -> failwith "obj file contains no geometry"
  | single :: [] -> single
  | groups -> RTCGroup.build ~children:groups ()
