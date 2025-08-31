open Seq

(* -------------------------------------------------------------------------- *)

(* A value is an integer value that reflects the current player view (or
   evaluation) of the current game configuration. In a zero-sum game, the
   opponent's view is the opposite of the current player's view, and the
   value 0 means that both players are even; in particular, in a situation
   where no more moves can be played, the value 0 represents a draw. *)

type value = int

(* All of the values that we consider lie in the range [bottom] to [top],
   inclusive. Note that [bottom] is the opposite of [top]. *)

let bottom, top = (-max_int, max_int)

(* We assume that, in every game configuration, a finite number of moves
   are permitted, and that each of these moves can be identified with an
   integer code. (A more realistic implementation would be parametric in
   the type [move].) *)

type move = int

(* A game tree is either a leaf or not a leaf. A leaf [TLeaf v] means that the
   game is over and that its value (in the eyes of the current player) is [v].
   A nonleaf [TNonLeaf mts] means that the game is not over. In that case, at
   least one move is permitted. The sequence [mts] is then a nonempty sequence
   of pairs of a permitted move [m] and the subtree that corresponds to this
   move. This sequence covers all of the permitted moves. *)

type tree = TLeaf of value | TNonLeaf of offspring
and offspring = (move * tree) Seq.t

(* -------------------------------------------------------------------------- *)

(* [index] turns a list of things into a list of numbered things. *)

let rec index i (xs : 'a list) : (int * 'a) list =
  match xs with [] -> [] | x :: xs -> (i, x) :: index (i + 1) xs

let index xs = index 0 xs

(* The following functions offer facilities for building game trees. They may
   appear in the messages produced by the automated grading system. *)

let leaf v = TLeaf v
let nonleaf (ts : tree list) : tree = TNonLeaf (List.to_seq (index ts))
(* [index] is used to generate arbitrary distinct move numbers *)

(* -------------------------------------------------------------------------- *)

(* A notion of sense is used in the reference evaluator, that is, the first
   function that we write in order to compute the value of a game tree. *)

type sense = Even | Odd

let opposite sense : sense = match sense with Even -> Odd | Odd -> Even

let interpret sense (v : value) : value =
  match sense with Even -> v | Odd -> -v

let join sense : value -> value -> value =
  match sense with Even -> max | Odd -> min

let unit sense : value = interpret sense bottom

(* -------------------------------------------------------------------------- *)

(* The following notion of equivalence up to an [alpha, beta] window can be
   used to specify the expected behavior of the Alpha-Beta algorithm. *)

let equivalent alpha beta v1 v2 =
  assert (alpha < beta);
  (v1 <= alpha && v2 <= alpha) || v1 = v2 || (beta <= v1 && beta <= v2)

(* -------------------------------------------------------------------------- *)

(* The size of a tree. *)

let rec size (t : tree) : int =
  match t with
  | TLeaf _ -> 1
  | TNonLeaf offspring -> 1 + size_offspring offspring

and size_offspring (offspring : offspring) : int =
  match offspring () with
  | Nil -> 0
  | Cons ((_, tree), xs) -> size tree + size_offspring xs

(* -------------------------------------------------------------------------- *)

(* The height of a tree. *)

let rec height (t : tree) : int =
  match t with
  | TLeaf _ -> 1
  | TNonLeaf offspring -> 1 + height_offspring offspring

and height_offspring (offspring : offspring) : int =
  match offspring () with
  | Nil -> 0
  | Cons ((_, tree), xs) -> max (height tree) (height_offspring xs)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, with a sense parameter: Minimax. *)

let rec eval (sense : sense) (t : tree) : value =
  match t with
  | TLeaf v -> interpret sense v
  | TNonLeaf offspring -> eval_offspring sense offspring

and eval_offspring (sense : sense) (offspring : offspring) : value =
  match offspring () with
  | Nil -> unit sense
  | Cons ((_, tree), xs) ->
      join sense (eval (opposite sense) tree) (eval_offspring sense xs)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, without a sense parameter: Negamax. *)

let rec nval (t : tree) : value =
  match t with TLeaf v -> v | TNonLeaf offspring -> nval_offspring offspring

and nval_offspring (offspring : offspring) =
  match offspring () with
  | Nil -> bottom
  | Cons ((_, tree), xs) -> max (-1 * nval tree) (nval_offspring xs)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, in Negamax style, and looping over children in
   a tail-recursive manner. *)

let rec ntval (t : tree) : value =
  match t with
  | TLeaf v -> v
  | TNonLeaf offspring -> ntval_offspring 0 offspring

and ntval_offspring (running_max : value) (offspring : offspring) : value =
  match offspring () with
  | Nil -> running_max
  | Cons ((_, tree), xs) ->
      let val' = -1 * nval tree in
      let max' = max val' running_max in
      ntval_offspring max' xs

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, using the Alpha-Beta algorithm. *)

let rec bval (alpha : value) (beta : value) (t : tree) : value =
  assert (alpha < beta);
  match t with
  | TLeaf v -> v
  | TNonLeaf offspring -> bval_offspring alpha beta offspring

and bval_offspring (alpha : value) (beta : value) (offspring : offspring) :
    value =
  assert (alpha < beta);
  match offspring () with
  | Nil -> alpha
  | Cons ((_, tree), xs) ->
      let our_score = -1 * bval alpha beta tree in
      (* alpha' at least our_score *)
      let alpha' = max our_score alpha in
      if alpha' >= beta then
        (* prune *)
        alpha'
      else bval_offspring alpha' beta xs

(* -------------------------------------------------------------------------- *)

(* In a game tree where every leaf carries the value -1 (loss), 0 (draw),
   or +1 (win), determining whether the first player is assured to win. *)

let assured_win (t : tree) : bool = bval (-1) 1 t = 1

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree using Alpha-Beta and returning the best move. *)

let rec bmove_offspring alpha beta (candidate : move option)
    (offspring : offspring) : move option =
  assert (alpha < beta);
  match offspring () with
  | Nil -> None
  | Cons ((move, tree), xs) ->
      let score = bval alpha beta tree in
      let optimal = score = beta in
      (* Format.printf "\t%i %i %b %i %i\n" move score optimal alpha beta; *)
      if optimal then Some move else bmove_offspring alpha beta (Some move) xs

let bmove alpha beta (t : tree) : move option =
  assert (alpha < beta);
  let optimal = bval alpha beta t in
  (* Format.printf "\toptimal: %i\n" optimal; *)
  match t with
  | TLeaf _ -> None
  | TNonLeaf offspring -> bmove_offspring alpha (-optimal) None offspring
