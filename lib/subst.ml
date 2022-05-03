open Terms

let rec occ f s t =
  match t with
  | Var g ->
    f = g
    ||
    (match List.assoc_opt g s with
    | Some s' -> occ f s s'
    | None -> false)
  | App (s', t) -> occ f s s' || occ f s t
  | Abs (_, s') -> occ f s s'
  | _ -> false
;;

let%test _ =
  let t = Var "h" in
  let f = "h" in
  let l = [] in
  occ f l t
;;

let%test _ =
  let t = Var "h" in
  let f = "g" in
  let s = [("h", Var "g")] in
  occ f s t
;;

let%test _ =
  let t = App(Var "h", Var "j") in
  let f = "j" in
  let s = [] in
  occ f s t
;;

let%test _ =
  let t = Abs("h", Var "j") in
  let f = "j" in
  let s = [] in
  occ f s t
;;

let%test _ =
  let t = Abs("h", Var "j") in
  let f = "k" in
  let s = [] in
  not (occ f s t)
;;

let rec subst y x term =
  let rec sub t =
    match t with
    | Bound z as b -> if z = x then Bound y else b
    | App (s1, s2) -> App (sub s1, sub s2)
    | Abs (z, s) as t ->
      if z = x
      then t
      else if z <> y
      then Abs (z, sub s)
      else (
        let z' = new_b () in
        Abs (z', sub (subst z' z s)))
    | s -> s
  in
  sub term
;;

let rec red t l =
  match t, l with
  | Abs (x, s), y :: ys -> red (subst (b_1 y) x s) ys
  | s, y :: ys -> red (App (s, y)) ys
  | s, [] -> s
;;

let rec devar s t =
  match strip t with
  | Var f, ys ->
    (match List.assoc_opt f s with
    | Some t -> devar s (red t ys)
    | None -> t)
  | _ -> t
;;
