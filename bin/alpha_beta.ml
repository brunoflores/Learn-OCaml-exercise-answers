open Learnocaml.Alpha_beta

let () =
  let t =
    TNonLeaf
      (List.to_seq
         [
           (1, TNonLeaf (List.to_seq [ (2, TLeaf 5); (3, TLeaf 10) ]));
           (4, TLeaf (-8));
         ])
  in
  let sense = Even in
  Format.printf "%d\n" @@ eval sense t;
  Format.printf "%d\n" @@ eval (opposite sense) t;
  Format.printf "%d\n" @@ nval t;
  assert (eval sense t = nval t);
  assert (ntval t = nval t);
  Format.printf "%d\n" @@ bval bottom top t;
  (* pruning *)
  let alpha = bottom in
  let beta = 6 in
  Format.printf "%d\n" @@ bval alpha beta t;
  assert (equivalent alpha beta (bval alpha beta t) (ntval t));
  Format.printf "assured win: %b\n" @@ assured_win t;
  (* *)
  let t =
    TNonLeaf
      (List.to_seq
         [
           (1, TNonLeaf (List.to_seq [ (2, TLeaf (-1)); (3, TLeaf (-1)) ]));
           (4, TLeaf 1);
         ])
  in
  Format.printf "%d\n" @@ nval t;
  Format.printf "assured win: %b\n" @@ assured_win t

let play (t : tree) =
  let rec find t =
    let rec step_offspring move (offspring : offspring) =
      match offspring () with
      | Nil -> None
      | Cons ((v, tree), xs) ->
          if v = move then find tree else step_offspring move xs
    in
    let step move t =
      match t with
      | TLeaf v -> Some t
      | TNonLeaf offspring -> step_offspring move offspring
    in
    let best_move = bmove bottom top t in
    match best_move with
    | None ->
        Format.printf "%s\n" "END";
        None
    | Some move ->
        Format.printf "\tplaying: %i\n" move;
        step move t
  in
  Format.printf "%s\n" "START";
  find t

let () =
  let t =
    TNonLeaf
      (List.to_seq
         [
           ( 1,
             TNonLeaf
               (List.to_seq
                  [
                    ( 2,
                      TNonLeaf
                        (List.to_seq
                           [
                             ( 5,
                               TNonLeaf
                                 (List.to_seq [ (6, TLeaf 12); (7, TLeaf 9) ])
                             );
                           ]) );
                    (3, TLeaf 10);
                  ]) );
           (4, TLeaf (-4));
         ])
  in
  ignore @@ play t
