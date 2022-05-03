let f_name = ref "F"
let b_name = ref "x"

let new_name r =
  r := !r ^ "'";
  !r
;;

let new_b () = new_name b_name

type term =
  | Var of string
  | Bound of string
  | Const of string
  | App of term * term
  | Abs of string * term

let new_v () = Var (new_name f_name)

let b_1 t =
  match t with
  | Bound s -> s
  | _ -> invalid_arg "Expected bound variable"
;;

let%test "b_1 gives back name of bound term" =
  let x = Bound "x" in
  b_1 x = "x"
;;

let%test "b_1 throws exception when not bound term" =
  let x = Var "x" in
  try b_1 x = "x" with
  | Invalid_argument _ -> true
;;

let strip t =
  let rec aux (t, l) =
    match t, l with
    | App (s, t), l -> aux (s, t :: l)
    | p -> p
  in
  aux (t, [])
;;

let%test "strip works on an application" =
  let term = App (App (Var "x", Var "z"), Var "y") in
  let h, l = strip term in
  h = Var "x" && l = [ Var "z"; Var "y" ]
;;

let%test "strip works on non application" =
  let term = Abs ("x", App (Var "x", Var "z")) in
  let h, l = strip term in
  h = term && l = []
;;

let abs xs t = List.fold_right (fun a b -> Abs (a, b)) (List.map b_1 xs) t
let hnf xs f ss = abs xs (List.fold_left (fun a b -> App (a, b)) f ss)
