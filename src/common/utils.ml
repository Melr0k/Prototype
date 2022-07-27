(* ✨ COLORS 💫 *)

let cli_off = ref false

type cli_opt =
  | Normal       (*  0 *)
  | Bold         (*  1 *)
  | Underline    (*  4 *)
  | Black        (* 30 *)
  | Red          (* 31 *)
  | Green        (* 32 *)
  | Yellow       (* 33 *)
  | Blue         (* 34 *)
  | Purple       (* 35 *)
  | Cyan         (* 36 *)
  | White        (* 37 *)
  | BgBlack      (* 40 *)
  | BgRed        (* 41 *)
  | BgGreen      (* 42 *)
  | BgYellow     (* 43 *)
  | BgBlue       (* 44 *)
  | BgPurple     (* 45 *)
  | BgCyan       (* 46 *)
  | BgWhite      (* 47 *)

let cli_to_code = function
  | Normal       -> ";0"
  | Bold         -> ";1"
  | Underline    -> ";4"
  | Black        -> ";30"
  | Red          -> ";31"
  | Green        -> ";32"
  | Yellow       -> ";33"
  | Blue         -> ";34"
  | Purple       -> ";35"
  | Cyan         -> ";36"
  | White        -> ";37"
  | BgBlack      -> ";40"
  | BgRed        -> ";41"
  | BgGreen      -> ";42"
  | BgYellow     -> ";43"
  | BgBlue       -> ";44"
  | BgPurple     -> ";45"
  | BgCyan       -> ";46"
  | BgWhite      -> ";47"

let print_cc c = "\o033[" ^ c ^ "m"
let print_co c = cli_to_code c |> print_cc
let print_col cl = List.fold_left (fun ac c -> ac ^ cli_to_code c) "" cl
                   |> print_cc

let cli_reset = print_co Normal

let colorify_l col ?(reset=Some Normal) str =
  if !cli_off then str
  else (col |> print_col) ^ str
       ^ (match reset with None -> "" | Some c -> print_co c)

let colorify col ?(reset=Some Normal) str = colorify_l [Bold;col] ~reset str

(* Utils *)

let print_type t =
    Format.printf "%s\n" (Cduce.string_of_type t)
    (*Format.printf "%s\n" (Cduce.string_of_type (Cduce.domain t))*)

let warning pos msg =
  let pos = List.fold_left (
    fun acc pos ->
    Format.asprintf "%s %s" acc (Position.string_of_pos pos)
  ) "" pos in
  Format.printf "%s:%s\t%s%s\n"
    ("Warning" |> colorify_l [Yellow] ~reset:None) pos msg cli_reset

let error pos msg =
  let pos = List.fold_left (
    fun acc pos ->
    Format.asprintf "%s %s" acc (Position.string_of_pos pos)
  ) "" pos in
  Format.printf "%s:%s\t%s%s\n"
    ("Error" |> colorify Red ~reset:None) pos msg cli_reset

let log_enabled = ref false
let log a =
  if !log_enabled then Format.fprintf Format.std_formatter a
  else Format.ifprintf Format.std_formatter a


let option_map f = function None -> None | Some e -> Some (f e)
let option_chain fs e =
  List.fold_left
    (fun acc f -> match acc with None -> None | Some e -> f e) (Some e) fs

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

let rec remove_duplicates equiv lst =
  let remove elt lst = List.filter (fun e -> equiv elt e |> not) lst in
  match lst with
  | [] -> []
  | e::lst -> e::(remove e lst |> remove_duplicates equiv)

let pp_long_list pp_elt fmt lst =
  Format.fprintf fmt "[@,@[<v 1>" ;
  List.iter (fun elt -> Format.fprintf fmt " %a ;@ " pp_elt elt) lst ;
  Format.fprintf fmt "@]]"

let pp_list pp_elt fmt lst =
  Format.fprintf fmt "[ " ;
  List.iter (fun elt -> Format.fprintf fmt "%a ; " pp_elt elt) lst ;
  Format.fprintf fmt "]"

let assert_with b msg =
  if not b then failwith msg
