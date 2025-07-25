let const = 5

let alloc = ref 5

let read = !alloc

let assign = alloc := true

let f0 = fun x -> read (* pure *)

let f1 = fun x -> succ x (* pure *)

let f2 = fun x -> (x + x)

let f3 = fun x -> !x

let f4 = fun (x:Ref) -> x (* pure *)

let f5 = fun f -> if (f 5) is Int
                  then (f 5)
                  else 2
(*
  no_se : ((5 -> Int & 'a) -> Int & 'a)
          & ((5 -> 'b) -> 2 | Int & 'b)

   se    :  ((5 -> 'a) -> 2 | 'a)
            & ((5 -> Int & 'b) -> Int & 'b)
            & ((5 -> Any \ Int) -> 2)
*)

let f6 = fun x -> if (f2 (succ x)) is 42
                  then (f2 (succ x))
                  else true

let add = <Int -> Int -> Int>:2pure

let x = let y = !alloc in if y is Int then y else 5
let int_read = if read is Int then read else 5

let partial_add1 = add int_read
let partial_add2 = add 5

let result1 = partial_add1 3
let result1 = partial_add2 3

let f7 = fun x ->
  if x is Unit then "Unit"
  else if x is Nil then "Nil"
  else if x is String then "String"
  else x

let f8 = f7 8

let fixpoint = fun f ->
  let delta = fun x ->
     f ( fun  v -> ( x x v ))
  in delta delta

let concat_stub concat x y =
   (x, concat x y)

let concat : ['a*] -> ['b*] -> ['a* ; 'b*] = fixpoint concat_stub

let my_ref = ref 5

let unit_ = my_ref := true

let value = !my_ref

let mix = fun x -> (ref 2, !my_ref)

(* let res = if (my_ref.set) is Nil -> Any then nil else true *)
