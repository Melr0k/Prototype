let a = ref 5

let b = fun x -> ref x

let c = !a

let d = a := true

let e = if a is Ref then 1 else "ha"

let err = !a := false

let aa = ref a

let dederef = !(!aa)

let f = fun x -> !x

let g = fun x -> if !x is Int then 2 else "he"

let h = fun x -> if !x is Ref then 3 else "hi"

let i = fun x -> !(f x)

let j = if a is Any then 4 else "ho"

let k = if !a is Any then 5 else "hu"

let l = if !a is Ref then !a else "han"


let x = ref 5

let ff = fun x ->
  let un = if !x is Int
           then x := "hi"
           else x := 5
  in !x

let k = fun f -> if f x is Int then f x else 3

let kk = (k ff)

let m = if !a is Int then !a else "b"

let xx = 5

(* let h = fun arg -> let t = arg.t in
                   let u = arg.u in
                   let q = 3 in
                   let o = !x in
                   let i = !x in
                   let b = 2 in
                   i *)

(*let n = if !x is Ref then if !(!x) is Int then !(!x) else 5 else 9*)

let x = ref 5

let k = fun f -> fun x -> if f x is Int
                          then f x
                         else 7

let f =
  fun args ->
  let g = args.a in
  if g {a = ()} is Int
  then g {a = ()}
  else ()

let neg = <Int->Int>

let g =
  fun args ->
  let n = args.a in
  let r = ref (neg 1) in
  if !x is Bool
  then let u = r := !x in
       ()
  else ()

let i = if f {a = g} is Int
        then f {a = g}
        else 0

let u = x := 0

let rr = ref (ref 9)

let dr = if !rr is Ref
         then if !(!rr) is Ref
              then if !(!(!rr)) is Int
                   then !(!(!rr))
                   else ()
              else ()
         else ()

let no = fun x -> 5
