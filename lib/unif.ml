open Terms
open Subst

exception Unif

let intersection l1 l2 = List.filter (fun x -> List.mem x l2) l1

let rec proj w s s' =
  match strip (devar s s') with
  | Abs (x, t), _ -> proj (x :: w) s t
  | Const _, ss -> List.fold_left (proj w) s ss
  | Bound x, ss -> if List.mem x w then List.fold_left (proj w) s ss else raise Unif
  | Var f, ss ->
    (f, hnf ss (new_v ()) (intersection ss (List.map (fun x -> Bound x) w))) :: s
  | _ -> raise Unif
;;

let rec eqs l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> if x = y then x :: eqs xs ys else eqs xs ys
  | _, _ -> raise Unif
;;

let flexflex1 f ym zn s = (f, hnf ym (new_v ()) (eqs ym zn)) :: s

let flexflex2 f ym g zn s =
  let xk = intersection ym zn
  and h = new_v () in
  (f, hnf ym h xk) :: (g, hnf zn h xk) :: s
;;

let flexflex f ym g zn s = if f = g then flexflex1 f ym zn s else flexflex2 f ym g zn s

let flexrigid f ym t s =
  if occ f s t
  then raise Unif
  else proj (List.map (fun x -> b_1 x) ym) ((f, abs ym t) :: s) t
;;

let rec unif s (s', t) =
  match devar s s', devar s t with
  | Abs (x, s'), Abs (y, t) -> unif s (s', if x = y then t else subst x y t)
  | Abs (x, s'), t -> unif s (s', App (t, Bound x))
  | s', Abs (x, t) -> unif s (App (s', Bound x), t)
  | s', t -> cases s (s', t)

and cases s (s', t) =
  match strip s', strip t with
  | (Var f, ym), (Var g, zn) -> flexflex f ym g zn s
  | (Var f, ym), _ -> flexrigid f ym t s
  | _, (Var f, ym) -> flexrigid f ym s' s
  | (a, sm), (b, tn) -> rigidrigid a sm b tn s

and rigidrigid a ss b ts s =
  if a <> b then raise Unif else List.fold_left unif s (List.combine ss ts)
;;
