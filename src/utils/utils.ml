
let warning fmt pos msg =
  let pos = List.fold_left (
    fun acc pos ->
    Format.asprintf "%s %s" acc (Position.string_of_pos pos)
  ) "" pos in
  Format.fprintf fmt "Warning:%s\t%s\n" pos msg

let error fmt pos msg =
  let pos = List.fold_left (
    fun acc pos ->
    Format.asprintf "%s %s" acc (Position.string_of_pos pos)
  ) "" pos in
  Format.fprintf fmt "Error:%s\t%s\n" pos msg

let log_disabled = -1
let log_full = 10
let log_level = ref log_disabled
let log ?(level=0) a =
  if level <= !log_level then Format.fprintf Format.std_formatter a
  else Format.ifprintf Format.std_formatter a

let option_chain fs e =
  List.fold_left (fun acc f -> match acc with None -> None | Some e -> f e) (Some e) fs
  
let identity x = x
let filter_options x = List.filter_map identity x

let rec split3 lst =
  match lst with
  | [] -> ([],[],[])
  | (a,b,c)::lst ->
    let (ar,br,cr) = split3 lst in
    (a::ar,b::br,c::cr)
let rec split4 lst =
  match lst with
  | [] -> ([],[],[],[])
  | (a,b,c,d)::lst ->
    let (ar,br,cr,dr) = split4 lst in
    (a::ar,b::br,c::cr,d::dr)

let memoize f input_transform ht =
  let rec aux input =
    let htbl_key = input_transform input in
    try Hashtbl.find ht htbl_key
    with Not_found ->
    (
      let res = f aux input in
      Hashtbl.replace ht htbl_key res ;
      res
    )
  in aux

let do_not_memoize f =
  let rec aux input =
    f aux input
  in aux

let rec regroup_equiv equiv lst =
  let extract_eq elt = List.partition (equiv elt) in
  match lst with
  | [] -> []
  | e::lst ->
    let (es, lst) = extract_eq e lst in
    (e::es)::(regroup_equiv equiv lst)

let remove_duplicates equiv lst =
  regroup_equiv equiv lst |> List.map List.hd

let keep_only_minimal leq lst =
  let remove_greater elt = List.filter (fun e -> leq elt e |> not) in
  let rec aux explored lst =
    match lst with
    | [] -> List.rev explored
    | e::lst ->
      let explored = remove_greater e explored in
      let lst = remove_greater e lst in
      aux (e::explored) lst
  in aux [] lst

let pp_long_list pp_elt fmt lst =
  Format.fprintf fmt "[@,@[<v 1>" ;
  List.iter (fun elt -> Format.fprintf fmt " %a ;@ " pp_elt elt) lst ;
  Format.fprintf fmt "@]]"

let pp_list pp_elt fmt lst =
  Format.fprintf fmt "[ " ;
  List.iter (fun elt -> Format.fprintf fmt "%a ; " pp_elt elt) lst ;
  Format.fprintf fmt "]"

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let trd3 (_,_,c) = c

let pairs s1 s2 =
  let rec aux s1 s2 =
    match s1 with
    | [] -> []
    | a1::s1 ->
      let pairs = aux s1 s2 in
      (List.map (fun a2 -> (a1,a2)) s2) @ pairs
  in
  aux s1 s2

let add_others lst =
  let rec aux treated lst =
    match lst with
    | [] -> []
    | a::lst ->
      let others = treated@lst in
      (a,others)::(aux (treated@[a]) lst)
  in
  aux [] lst

let find_among_others pred lst =
  lst |> add_others |> List.find_opt (fun (a,o) -> pred a o)

let rec insert x lst =
  match lst with
  | [] -> [[x]]
  | h::t ->
    (x::lst) :: (List.map (fun el -> h::el) (insert x t))

let rec perm lst =
  match lst with
  | [] -> [[]]
  | h::t ->
    List.flatten (List.map (insert h) (perm t))
