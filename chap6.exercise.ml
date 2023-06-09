(*1)Exercise: spec game [★★★]
Pair up with another programmer and play the specification game with them. Take turns being the specifier and the devious programmer. Here are some suggested functions you could use:
num_vowels : string -> int
is_sorted : 'a list -> bool
sort : 'a list -> 'a list
max : 'a list -> 'a
is_prime : int -> bool
is_palindrome : string -> bool
second_largest : int list -> int
depth : 'a tree -> int*)

module type S = sig
  val num_vowels : string -> int
  val is_sorted : 'a list -> bool
  val sort :'a list -> 'a list
  val max : 'a list -> 'a
  val is_prime : int -> bool
  
end

(*2)Let’s create a data abstraction for single-variable integer polynomials of the form
cnxn+…+c1x+c0.
Let’s assume that the polynomials are dense, meaning that they contain very few coefficients that are zero. Here is an incomplete interface for polynomials:*)

module type poly = sig
  type t
  val eval : int -> t -> int
  val from_coefficients : int list -> t
end

module type Poly = sig
 type t
val eval : int -> t -> int
 val zero : t
val int :t -> t
val variable : t
val add : t -> t -> t
val subtract : t -> t -> t
val multiply : t -> t -> t

val power : t -> int -> t
val degree : t -> int
val coefficient : t -> int -> int
val is_zero : t -> bool
end

(*3
Implement your specification of Poly. As part of your implementation, you will need to choose a representation type t. Hint: recalling that our polynomials are dense might guide you in choosing a representation type that makes for an easier implementation.*)
 
type poly = int list

let rec degree p =
  match p with
  | [] -> -1  (* The polynomial is zero *)
  | [c] -> 0  (* The polynomial is a constant *)
  | _::rest -> 1 + degree rest  (* Recursively check the rest of the list *)

let rec add_poly p1 p2 =
  match p1, p2 with
  | [], [] -> []  (* Both polynomials are zero *)
  | [], p | p, [] -> p  (* One polynomial is zero, return the other *)
  | c1::rest1, c2::rest2 -> (c1 + c2) :: add_poly rest1 rest2

let rec multiply_poly p1 p2 =
  match p1 with
  | [] -> []
  | c::rest -> add_poly (multiply_scalar c p2) (0 :: multiply_poly rest p2)

and multiply_scalar c p =
  match p with
  | [] -> []
  | c'::rest -> (c * c') :: multiply_scalar c rest

(*
   (*4*)
To specify and implement a data abstraction for interval arithmetic in OCaml, we can define a module that encapsulates the interval operations and provides an abstraction over the interval representation. Here's an example implementation:*)
module Interval : sig
  type t
  

  val create : float -> float -> t
 

  val add : t -> t -> t
 

  val subtract : t -> t -> t
  

  val multiply : t -> t -> t
  

  val divide : t -> t -> t option
  

  val to_string : t -> string
  

  val rep_ok : t -> bool
  
end 
= 
struct
  type t = float * float

  let create a b = (a, b)

  let add (a1, b1) (a2, b2) = (a1 +. a2, b1 +. b2)

  let subtract (a1, b1) (a2, b2) = (a1 -. b2, b1 -. a2)

  let multiply (a1, b1) (a2, b2) =
    let products = [a1 *. a2; a1 *. b2; b1 *. a2; b1 *. b2] in
    (List.fold_left min infinity products, List.fold_left max neg_infinity products)

  let divide (a1, b1) (a2, b2) =
    if a2 <= 0.0 && b2 >= 0.0 then None
    else
      let reciprocals = [a2 /. a1; a2 /. b1; b2 /. a1; b2 /. b1] in
      Some (List.fold_left min infinity reciprocals, List.fold_left max neg_infinity reciprocals)

  let to_string (a, b) = Printf.sprintf "[%f, %f]" a b

  let rep_ok (a, b) = a <= b
end
(*5.Implement a map (aka dictionary) data structure with abstract type ('k, 'v) t. As the representation type, use 'k -> 'v. That is, a map is represented as an OCaml function from keys to values. Document the AF. You do not need an RI. Your solution will make heavy use of higher-order functions. Provide at least these values and operations: empty, mem, find, add, remove.
*)
module Map : sig
  type ('k, 'v) t
 

  val empty : ('k, 'v) t


  val mem : 'k -> ('k, 'v) t -> bool
  

  val find : 'k -> ('k, 'v) t -> 'v option
 

  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t


  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
 
end = struct
  type ('k, 'v) t = 'k -> 'v option

  let empty = fun _ -> None

  let mem key map = match map key with
    | Some _ -> true
    | None -> false

  let find key map = map key

  let add key value map = fun k -> if k = key then Some value else map k

  let remove key map = fun k -> if k = key then None else map k
end

(*6.
 Go back to the implementation of sets with lists in the previous chapter. Based on the specification comments of Set, write an OUnit test suite for ListSet that does black-box testing of all its operations.  *)
  open OUnit2

(* Helper functions *)
let set_to_list set =
  List.sort compare (Set.elements set)

(* Test suite *)
let test_suite =
  "ListSet" >::: [
    (* Initialization *)
    "empty_set" >:: (fun _ ->
      let set = ListSet.empty in
      assert_equal [] (set_to_list set)
    );

    (* Adding elements *)
    "add_element" >:: (fun _ ->
      let set = ListSet.add 1 ListSet.empty in
      assert_equal [1] (set_to_list set)
    );

    (* Removing elements *)
    "remove_element" >:: (fun _ ->
      let set = ListSet.remove 2 (ListSet.add 1 (ListSet.singleton 2)) in
      assert_equal [1] (set_to_list set)
    );

    (* Checking membership *)
    "member_true" >:: (fun _ ->
      let set = ListSet.singleton 1 in
      assert_bool "1 is a member" (ListSet.mem 1 set)
    );

    "member_false" >:: (fun _ ->
      let set = ListSet.singleton 1 in
      assert_bool "2 is not a member" (not (ListSet.mem 2 set))
    );

    (* Union *)
    "union" >:: (fun _ ->
      let set1 = ListSet.of_list [1; 2; 3] in
      let set2 = ListSet.of_list [3; 4; 5] in
      let set = ListSet.union set1 set2 in
      assert_equal [1; 2; 3; 4; 5] (set_to_list set)
    );

    (* Intersection *)
    "inter" >:: (fun _ ->
      let set1 = ListSet.of_list [1; 2; 3] in
      let set2 = ListSet.of_list [3; 4; 5] in
      let set = ListSet.inter set1 set2 in
      assert_equal [3] (set_to_list set)
    );

    (* Difference *)
    "diff" >:: (fun _ ->
      let set1 = ListSet.of_list [1; 2; 3] in
      let set2 = ListSet.of_list [3; 4; 5] in
      let set = ListSet.diff set1 set2 in
      assert_equal [1; 2] (set_to_list set)
    );

    (* Subset *)
    "subset_true" >:: (fun _ ->
      let set1 = ListSet.of_list [1; 2; 3] in
      let set2 = ListSet.of_list [1; 2; 3; 4; 5] in
      assert_bool "set1 is a subset of set2" (ListSet.subset set1 set2)
    );

    "subset_false" >:: (fun _ ->
      let set1 = ListSet.of_list [1; 2; 3] in
      let set2 = ListSet.of_list [4; 5] in
      assert_bool "set1 is not a subset of set2" (not (ListSet.subset set1 set2))
    );
  ]

(* Running the test suite *)
let () =
  run_test_tt_main test_suite


  (*question:qcheck odd divisor

Here is a buggy function:

(** [odd_divisor x] is an odd divisor of [x].
    Requires: [x >= 0]. *)
let odd_divisor x =
  if x < 3 then 1 else
    let rec search y =
      if y >= x then y  (* exceeded upper bound *)
      else if x mod y = 0 then y  (* found a divisor! *)
      else search (y + 2) (* skip evens *)
    in search 3
Write a QCheck test to determine whether the output of that function (on a positive integer, per its precondition; hint: there is an arbitrary that generates positive integers) is both odd and is a divisor of the input. You will discover that there is a bug in the function. What is the smallest integer that triggers that bug?*)

(*question:Prove that exp x (m + n) = exp x m * exp x n, where

let rec exp x n =
  if n = 0 then 1 else x * exp x (n - 1)
Proceed by induction on m.*)

 (*answer steps:
 Claim: exp x (m + n) = exp x m * exp x n

Proof: by induction on m.
P(m) = exp x (m + n) = exp x m * exp x n

Base case: m = 0
Show: exp x (0 + n) = exp x 0 * exp x n

  exp x (0 + n)
=   { evaluation }
  exp x n

  exp x 0 * exp x n
=   { evaluation }
  1 * exp x n
=   { evaluation }
  exp x n

Inductive case: m = k + 1
Show: exp x ((k + 1) + n) = exp x (k + 1) * exp x n
IH: exp x (k + n) = exp x k * exp x n

  exp x ((k + 1) + n)
=   { evaluation }
  x * exp x (k + n)
=   { IH }
  x * exp x k * exp x n

  exp x (k + 1) * exp x n
=   { evaluation }
  x * exp x k * exp x n

QED*)
(*Prove that forall n >= 1, fib n = fibi n (0, 1), where

let rec fib n =
  if n = 1 then 1
  else if n = 2 then 1
  else fib (n - 2) + fib (n - 1)

let rec fibi n (prev, curr) =
  if n = 1 then curr
  else fibi (n - 1) (curr, prev + curr)*)
(*question:Proceed by induction on n, rather than trying to apply the theorem about converting recursion into iteration.*)
(*Answer:Claim: forall n >= 1, fib n = fibi n (0, 1)
forall n >= 1, forall m >= 1, fib (n + m) = fibi n (fib m, fib (m + 1))
    Proof: by induction on n.

    Base case: n = 1
    Show: forall m >= 1, fib (1 + m) = fibi 1 (fib m, fib (m + 1))

        fibi 1 (fib m, fib (m + 1))
    =    { evaluation }
        fib (m + 1)
    =    { algebra }
        fib (1 + m)

    Inductive case: n = k + 1
    Show: forall m >= 1, fib ((k + 1) + m) = fibi (k + 1) (fib m, fib (m + 1))
    IH: forall m >= 1, fib (k + m) = fibi k (fib m, fib (m + 1))

                fib ((k + 1) + m)
            =      { algebra }
                fib (k + (m + 1))
            =      { IH with m := m + 1 }
                fibi k (fib (m + 1), fib (m + 2))
            =      { evaluation }
                fibi k (fib (m + 1), fib m + fib (m + 1))

                fibi (k + 1) (fib m, fib (m + 1))
            =      { evaluation }
                fibi k (fib (m + 1), fib m + fib (m + 1))

    QED*)
    (*question:Prove that expsq x n = exp x n, where

let rec expsq x n =
  if n = 0 then 1
  else if n = 1 then x
  else (if n mod 2 = 0 then 1 else x) * expsq (x * x) (n / 2)
Proceed by strong induction on n. Function expsq implements exponentiation by repeated squaring, which results in more efficient computation than exp.

*)
(*answer:Claim: forall n x, expsq x n  = exp x n

Proof: by induction on n.
P(n) = forall x, expsq x n  = exp x n

Base case 0:  n = 0
Show: forall x, expsq x 0  = exp x 0

  expsq x 0
=   { evaluation }
  1

  exp x 0
=   { evaluation }
  1

Base case 1:  n = 1
Show: forall x, expsq x 1  = exp x 1

  expsq x 1
=   { evaluation }
  x

  exp x 1
=   { evaluation }
  x * exp x 0
=   { evaluation and algebra }
  x

Inductive case for even n: n = 2k
Show: forall x, expsq x 2k  = exp x 2k
IH: forall x, forall j < 2k, expsq x j  = exp x j

  expsq x 2k
=   { evaluation }
  1 * expsq (x * x) (2k / 2)
=   { IH, instantiating its x as (x * x)
      and j as (2k / 2) }
  exp (x * x) k
=   { evaluation }
  (x * x) * exp (x * x) (k - 1)

  exp x 2k
=   { evaluation }
  x * exp x (2k - 1)
=   { evaluation }
  (x * x) exp x (2k - 2)
=   { IH, instantiating its x as x and j as (2k - 2) }
  (x * x) * expsq x (2k - 2)
=   { evaluation and algebra }
  (x * x) * expsq (x * x) (k - 1)
=   { IH, instantiating its x as (x * x) and j as (k - 1) }
  (x * x) * exp (x * x) (k - 1)

Inductive case for odd n: n = 2k + 1
Show: forall x, expsq x (2k + 1) = exp x (2k + 1)
IH: forall x, forall j < 2k, expsq x j  = exp x j

  expsq x (2k + 1)
=   { evaluation }
  x * expsq (x * x) ((2k + 1) / 2)
=   { by the properties of integer division }
  x * expsq (x * x) k

  exp x (2k + 1)
=   { evaluation }
  x * exp x 2k
=   { IH, instantiating its x as x, and j as 2k) }
  x * expsq x 2k
=   { evaluation and algebra }
  x * expsq (x * x) k

QED*)
(*Prove that forall n, mult n Z = Z by induction on n, where:

let rec mult a b =
  match a with
  | Z -> Z
  | S k -> plus b (mult k b)
*)
(*anwer:
   Exercise: mult

Claim: forall n, mult n Z = Z
Proof: by induction on n
P(n) = mult n Z = Z

Base case: n = Z
Show: mult Z Z = Z

  mult Z Z
=   { eval mult }
  Z

Inductive case: n = S k
Show: mult (S k) Z = Z
IH: mult k Z = Z

  mult (S k) Z
=   { eval mult }
  plus Z (mult k Z)
=   { IH }
  plus Z Z
=   { eval plus }
  Z

QED*)
(*question:Prove that forall lst, lst @ [] = lst by induction on lst.*)
(*answer:Claim:  forall lst, lst @ [] = lst
Proof: by induction on lst
P(lst) = lst @ [] = lst

Base case: lst = []
Show: [] @ [] = []

  [] @ []
=   { eval @ }
  []

Inductive case: lst = h :: t
Show: (h :: t) @ [] = h :: t
IH: t @ [] = t

  (h :: t) @ []
=   { eval @ }
  h :: (t @ [])
=   { IH }
  h :: t

QED*)
(*question:Prove that reverse distributes over append, i.e., that forall lst1 lst2, rev (lst1 @ lst2) = rev lst2 @ rev lst1, where:

let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [h]
(That is, of course, an inefficient implementation of rev.) You will need to choose which list to induct over. You will need the previous exercise as a lemma, as well as the associativity of append, which was proved in the notes above.

*)
(*answer:Claim: forall lst1 lst2, rev (lst1 @ lst2) = rev lst2 @ rev lst1
Proof: by induction on lst1
P(lst1) = forall lst2, rev (lst1 @ lst2) = rev lst2 @ rev lst1

Base case: lst1 = []
Show: forall lst2, rev ([] @ lst2) = rev lst2 @ rev []

  rev ([] @ lst2)
=   { eval @ }
  rev lst2

  rev lst2 @ rev []
=   { eval rev }
  rev lst2 @ []
=   { exercise 2 }
  rev lst2

Inductive case: lst1 = h :: t
Show: forall lst2, rev ((h :: t) @ lst2) = rev lst2 @ rev (h :: t)
IH: forall lst2, rev (t @ lst2) = rev lst2 @ rev t

  rev ((h :: t) @ lst2)
=   { eval @ }
  rev (h :: (t @ lst2))
=   { eval rev }
  rev (t @ lst2) @ [h]
=   { IH }
  (rev lst2 @ rev t) @ [h]

  rev lst2 @ rev (h :: t)
=   { eval rev }
  rev lst2 @ (rev t @ [h])
=   { associativity of @, proved in textbook }
  (rev lst2 @ rev t) @ [h]

QED*)
(*question:Prove that reverse is an involution, i.e., that forall lst, rev (rev lst) = lst. Proceed by induction on lst. You will need the previous exercise as a lemma.*)
(*answer:Claim: forall lst, rev (rev lst) = lst
Proof: by induction on lst
P(lst) = rev (rev lst) = lst

Base case: lst = []
Show: rev (rev []) = []

  rev (rev [])
=   { eval rev, twice }
  []

Inductive case: lst = h :: t
Show: rev (rev (h :: t)) = h :: t
IH: rev (rev t) = t

  rev (rev (h :: t))
=   { eval rev }
  rev (rev t @ [h])
=   { exercise 3 }
  rev [h] @ rev (rev t)
=   { IH }
  rev [h] @ t
=   { eval rev }
  [h] @ t
=   { eval @ }
  h :: t

QED
*)


(*question:Prove that forall t, size (reflect t) = size t by induction on t, where:

let rec size = function
  | Leaf -> 0
  | Node (l, v, r) -> 1 + size l + size r*)


(*answer:Claim: forall t, size (reflect t) = size t
Proof: by induction on t
P(t) = size (reflect t) = size t

Base case: t = Leaf
Show: size (reflect Leaf) = size Leaf

  size (reflect Leaf)
=   { eval reflect }
  size Leaf

Inductive case: t = Node (l, v, r)
Show: size (reflect (Node (l, v, r))) = size (Node (l, v, r))
IH1: size (reflect l) = size l
IH2: size (reflect r) = size r

  size (reflect (Node (l, v, r)))
=   { eval reflect }
  size (Node (reflect r, v, reflect l))
=   { eval size }
  1 + size (reflect r) + size (reflect l)
=   { IH1 and IH2 }
  1 + size r + size l

  size (Node (l, v, r))
=   { eval size }
  1 + size l + size r
=   { algebra }
  1 + size r + size l

QED*)
(*question:We proved that fold_left and fold_right yield the same results if their function argument is associative and commutative. But that doesn’t explain why these two implementations of concat yield the same results, because ( ^ ) is not commutative:

let concat_l lst = List.fold_left ( ^ ) "" lst
let concat_r lst = List.fold_right ( ^ ) lst ""
Formulate and prove a new theorem about when fold_left and fold_right yield the same results, under the relaxed assumption that their function argument is associative but not necessarily commutative. Hint: make a new assumption about the initial value of the accumulator.*)
(*claim:if fold_left and fold_right is associative and there exist exists an element 
   e such that f e x = x and f x e = x for all x in the input list
Proof:
Let f be the associative function and lst be the input list.

basecase:

For fold_left, the initial value of the accumulator is e, and fold_left f e [] = e.
For fold_right, the initial value of the accumulator is also e, and fold_right f [] e = e.

inductve case:

fold_left f e lst = fold_right f lst e holds for some non-empty list lst.
fold_left f e (x :: lst) is equivalent to fold_left f (f e x) lst
By associativity of f, we can rewrite it as fold_left f (f (f e x) (lst_head)) (lst_tail), where lst_head is the first element of lst and lst_tail is the remaining sublist.
f e x = x (as per the assumption), we have f (f e x) (lst_head) = f x (lst_head)
fold_left f e (x :: lst) is equivalent to fold_left f (f x (lst_head)) (lst_tail).
fold_right:

fold_right f (x :: lst) e is equivalent to f x (fold_right f lst e).
By the induction assumption, we can replace fold_right f lst e with fold_left f e lst.
Therefore, fold_right f (x :: lst) e is equivalent to f x (fold_left f e lst).
fold_left f e (x :: lst) = fold_right f (x :: lst) e if fold_left f e lst = fold_right f lst e.*)

(*question:In propositional logic, we have atomic propositions, negation, conjunction, disjunction, and implication. For example, raining /\ snowing /\ cold is a proposition stating that it is simultaneously raining and snowing and cold (a weather condition known as Ithacating).
Define an OCaml type to represent propositions. Then state the induction principle for that type.  *)
  
type prop = (* propositions *)
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop
  | Imp of prop * prop
  (*Induction principle for prop:

forall properties P,
  if forall x, P(Atom x)
  and forall q, P(q) implies P(Neg q)
  and forall q r, (P(q) and P(r)) implies P(Conj (q,r))
  and forall q r, (P(q) and P(r)) implies P(Disj (q,r))
  and forall q r, (P(q) and P(r)) implies P(Imp (q,r))
  then forall q, P(q)
*)
(*questions:A bag or multiset is like a blend of a list and a set: like a set, order does not matter; like a list, elements may occur more than once. The number of times an element occurs is its multiplicity. An element that does not occur in the bag has multiplicity 0. Here is an OCaml signature for bags:

module type Bag = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val mult : 'a -> 'a t -> int
  val remove : 'a -> 'a t -> 'a t
end
Categorize the operations in the Bag interface as generators, manipulators, or queries. Then design an equational specification for bags. 
For the remove operation, your specification should cause at most one occurrence of an element to be removed. 
That is, the multiplicity of that value should decrease by at most one.*)
(*answer:
Generators: `empty`, `insert`.
Manipulator: `remove`.
Queries: `is_empty`, `mult`.

Equations:

1.  is_empty empty = true
2.  is_empty (insert x b) = false
3.  mult x empty = 0
4a. mult y (insert x b) = 1 + mult y b              if x = y
4b. mult y (insert x b) = mult y b                  if x <> y
5.  remove x empty = empty
6a. remove y (insert x b) = b                       if x = y
6b. remove y (insert x b) = insert x (remove y b)   if x <> y
7.  insert x (insert y b) = insert y (insert x b)*)