(*chap5:1*)
 (* complex synonym*) 
 module type ComplexSig = sig 
   type t = float * float 
   val zero : t 
   val add : t -> t -> t 
 end;; 
  
  
 (*chap5:2*)
 (*complex encapsulation *) 
 module Complex : ComplexSig = struct 
   type t = float * float 
   let zero = (0., 0.) 
   let add (r1,i1) (r2,i2) = r1 +. r2, i1 +. i2 
 end;; 
  
 (*chap5:3*)
 (big list queue) 
 module ListQueue = struct 
   type 'a queue = 'a list 
  
   let empty = [] 
  
   let is_empty q = (q = []) 
  
   let enqueue x q = q @ [x] 
  
   let peek = function 
     | []   -> failwith "Empty" 
     | x::_ -> x 
  
   let dequeue = function 
     | []   -> failwith "Empty" 
     | _::q -> q 
 end 
  
 let fill_listqueue n = 
   let rec loop n q = 
     if n=0 then q 
     else loop (n-1) (ListQueue.enqueue n q) in 
   loop n ListQueue.empty;; 
  
  
   (*chap5:4*) 
   (big batched queu) 
   module BatchedQueue = struct 
     type 'a t = {outbox:'a list; inbox:'a list} 
    
     let empty = {outbox=[]; inbox=[]} 
    
     let is_empty = function 
       | {outbox=[]; inbox=[]} -> true 
       | _ -> false 
    
     let norm = function 
       | {outbox=[]; inbox} -> {outbox=List.rev inbox; inbox=[]} 
       | q -> q 
    
     let enqueue x q = norm {q with inbox=x::q.inbox} 
    
     let peek = function 
       | {outbox=[]; _} -> None 
       | {outbox=x::_; _} -> Some x 
    
     let dequeue = function 
       | {outbox=[]; _} -> None 
       | {outbox=_::xs; inbox} -> Some (norm {outbox=xs; inbox}) 
   end 
    
   let fill_BatchedQueue n = 
     let rec loop n q = 
       if n=0 
       then q 
       else loop (n-1) (BatchedQueue.enqueue n q) 
     in 
     loop n BatchedQueue.empty;; 
  
  
 (*chap5:6*)
 (binary search tree map) 
 module type Map = sig 
   type ('k, 'v) t 
   val empty  : ('k, 'v) t 
   val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t 
   val lookup  : 'k -> ('k,'v) t -> 'v 
 end 
  
 module BstMap : Map = struct 
   type 'a tree = 
     | Leaf 
     | Node of 'a * 'a tree * 'a tree 
  
   type ('k, 'v) t = ('k * 'v) tree 
  
   let empty = 
     Leaf 
  
   let rec insert k v = function 
     | Leaf -> Node((k, v), Leaf, Leaf) 
     | Node ((k',v'), l, r) -> 
       if (k = k') then Node ((k, v), l, r) 
       else if (k < k') then Node ((k',v'), insert k v l, r) 
       else Node ((k',v'), l, insert k v r) 
  
   let rec lookup k = function 
     | Leaf -> failwith "Not_found" 
     | Node ((k',v'), l, r) -> 
       if (k = k') then v' 
       else if (k < k') then lookup k l 
       else lookup k r 
 end;; 
  
  
 (*chap5:7*)
 (fraction) 
 module type Fraction = sig 
   (* A fraction is a rational number p/q, where q != 0.*) 
   type t 
  
   (* [make n d] is n/d. Requires d != 0. *) 
   val make : int -> int -> t 
  
   val numerator : t -> int 
   val denominator : t -> int 
   val to_string : t -> string 
   val to_float : t -> float 
  
   val add : t -> t -> t 
   val mul : t -> t -> t 
 end 
  
 module PairFraction = struct 
   type t = int * int 
   let make n d = 
     assert (d != 0); 
     (n,d) 
   let numerator (n,d) = n 
   let denominator (n,d) = d 
   let to_string (n,d) = 
     string_of_int n ^ " / " ^ string_of_int d 
   let to_float (n,d) = 
     float_of_int n /. float_of_int d 
   let add (n1,d1) (n2,d2) = 
     let d' = d1 * d2 in 
     (n1 * d2 + n2 * d1, d') 
   let mul (n1,d1) (n2,d2) = 
     (n1 * n2, d1 * d2) 
 end;; 
  
  
  
 (*chap5:8*) 
 (* fraction reduced*) 
  
 module Fraction : sig 
   type t 
   val make : int -> int -> t 
   val add : t -> t -> t 
   val mul : t -> t -> t 
 end = struct 
   type t = int * int  (* numerator, denominator *) 
  
   (* Helper function to calculate the greatest common divisor using Euclid's algorithm *) 
   let rec gcd a b = 
     if b = 0 then a 
     else gcd b (a mod b) 
  
   (* Helper function to reduce a fraction to its reduced form *) 
   let reduce (num, den) = 
     let gcd_val = gcd num den in 
     (num / gcd_val, den / gcd_val) 
  
   let make num den = 
     if den = 0 then failwith "Denominator cannot be zero" 
     else if den < 0 then reduce (-num, -den) 
     else reduce (num, den) 
  
   let add (num1, den1) (num2, den2) = 
     let num = (num1 * den2) + (num2 * den1) 
     and den = den1 * den2 in 
     make num den 
  
   let mul (num1, den1) (num2, den2) = 
     let num = num1 * num2 
     and den = den1 * den2 in 
     make num den 
 end;; 
  
  
 (*chap5:9*) 
 (use char map) 
 module CharMap = Map.Make(Char) 
 let map = CharMap.( 
     empty 
     |> add 'A' "Alpha" 
     |> add 'E' "Echo" 
     |> add 'S' "Sierra" 
     |> add 'V' "Victor" 
   ) 
 let echo = CharMap.find 'E' map (* "Echo" *) 
 let map' = CharMap.remove 'A' map 
 let a_exists = CharMap.mem 'A' map' (* false *) 
 let bindings = CharMap.bindings map';; (* [('E', "Echo"); ('S', "Sierra"); 
                                          ('V', "Victor")] *) 
  
  
 (*chap5:10*) 
 (bindings) 
 CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings);; 
  
 CharMap.(empty |> add 'y' 1 |> add 'x' 0 |> bindings);; 
  
 CharMap.(empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings);; 
  
 (*chap5:11*) 
 (*date order *) 
 type date = { 
   month : int; 
   day : int 
 } 
  
 module Date = struct 
   type t = date 
  
   let compare d1 d2 = 
     if d1.month = d2.month then d1.day - d2.day 
     else d1.month - d2.month 
 end;; 
  
 (*chap5:12*)
 (calendar) 
 module DateMap = Map.Make(Date) 
  
 type calendar = string DateMap.t 
  
 let my_calendar = 
   DateMap.(empty |> 
            add { month = 2; day = 7 } "e day" |> 
            add { month = 3; day = 14 } "pi day" |> 
            add { month = 6; day = 18 } "phi day" (* according to some *) |> 
            add { month = 10; day = 23 } "mole day" |> 
            add { month = 11; day = 23 } "fibonacci day" 
           );; 
  
  (*chap5:12*)
   (* print calendar*) 
   let print_calendar cal = 
     DateMap.iter 
       (fun date event -> Printf.printf "%d/%d: %s\n" date.month date.day event) 
       cal;; 
        
   (*chap5:15*)
   (is for) 
   let is_for m = 
     CharMap.mapi (fun key v -> Printf.sprintf "%c is for %s" key v) m;; 
  
  
 
 (*chap5:15*)
 (* first after*) 
  
 let thd (_,_,x) = x;; 
 let first_after date cal = 
   DateMap.(split date cal |> thd |> min_binding |> snd);; 
  
  
  (*chap5:16*)
 (sets) 
  
 module CaseInsensitiveStringSet = struct 
   module CaseInsensitiveString = struct 
     type t = string 
  
     let compare a b = String.compare (String.lowercase_ascii a) (String.lowercase_ascii b) 
     let equal a b = String.lowercase_ascii a = String.lowercase_ascii b 
     let hash = Hashtbl.hash 
  
     let to_string s = s 
   end 
  
   module S = Set.Make(CaseInsensitiveString) 
  
   type t = S.t 
  
   let empty = S.empty 
   let is_empty = S.is_empty 
   let mem = S.mem 
   let add s set = S.add (String.lowercase_ascii s) set 
   let remove s set = S.remove (String.lowercase_ascii s) set 
   let union set1 set2 = S.union set1 set2 
   let inter set1 set2 = S.inter set1 set2 
   let diff set1 set2 = S.diff set1 set2 
   let subset set1 set2 = S.subset set1 set2 
   let iter f set = S.iter f set 
   let fold f set acc = S.fold f set acc 
   let for_all p set = S.for_all p set 
   let exists p set = S.exists p set 
   let filter p set = S.filter p set 
   let cardinal set = S.cardinal set 
   let elements set = S.elements set 
   let min_elt set = S.min_elt set 
   let max_elt set = S.max_elt set 
   let choose set = S.choose set 
   let split s set = S.split (String.lowercase_ascii s) set 
   let find s set = S.find (String.lowercase_ascii s) set 
 end;; 
  
  
  (*chap5:17*)
 (ToString) 
 module type ToString = sig 
   type t 
   val to_string: t -> string 
 end;; 
  
  (*chap5:19*)
 (* Print*) 
 module Print (M : ToString) = struct 
   (* effects: print a string representation of [M.t] *) 
   let print v = print_string (M.to_string v) 
 end;; 
  
  (*chap5:20*)
 (Print Int) 
 module Int = struct 
   type t = int 
   let to_string = string_of_int 
 end 
  
 module PrintInt = Print(Int) 
 let _ = PrintInt.print 5;; 
  
  (*chap5:21*)
 (* Print String*) 
  
 module MyString = struct 
   type t = string 
   let to_string s = s 
 end 
  
 module PrintString = Print(MyString) 
 let _ = PrintString.print "Harambe";; 
  
  
  (*chap5:22*) 
 (sets) 
 module CisSet = Set.Make(struct 
     type t = string 
     let compare s1 s2 = 
       String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2) 
   end) 
  
 let _ = CisSet.(equal (of_list ["grr"; "argh"]) (of_list ["GRR"; "aRgh"]));; 
  
  (*chap5:23*) 
 (Print String reuse revisited) 
 module StringWithPrint = struct 
   include String 
   include Print(MyString) 
 end;; 
  
  
  
 (refactor arith) 
 module type PreRing = sig 
   type t 
   val zero  : t 
   val one   : t 
   val (+)   : t -> t -> t 
   val (~-)  : t -> t 
   val ( * ) : t -> t -> t 
   val to_string : t -> string 
 end 
  
 module type OfInt = sig 
   type t 
   val of_int : int -> t 
 end 
  
 module type Ring = sig 
   include PreRing 
   include OfInt with type t := t 
 end 
  
 module type PreField = sig 
   include PreRing 
   val (/) : t -> t -> t 
 end 
  
 module type Field = sig 
   include PreField 
   include OfInt with type t := t 
 end 
  
 module RingOfPreRing (R:PreRing) = (struct 
   include R 
   let of_int n = 
     let two = one + one in 
     (* [loop n b x] is [nb + x] *) 
     let rec loop n b x = 
       if n=0 then x 
       else loop Stdlib.(n/2) (b*two) 
           (if n mod 2 = 0 then x else x+b) 
     in 
     let m = loop (abs n) one zero in 
     if n<0 then -m else m 
 end : Ring with type t = R.t) 
  
 module FieldOfPreField (F:PreField) = (struct 
   module R : (OfInt with type t := F.t) = RingOfPreRing(F) 
   include F 
   include R 
 end : Field) 
  
 module IntPreRing = struct 
   type t = int 
   let zero = 0 
   let one = 1 
   let (+) = (+) 
   let (~-) = (~-) 
   let ( * ) = ( * ) 
   let to_string = string_of_int 
 end 
  
 module IntRing : Ring = RingOfPreRing(IntPreRing) 
  
 module IntPreField = struct 
   include IntPreRing 
   let (/) = (/) 
 end 
  
 module IntField : Field = FieldOfPreField(IntPreField) 
  
 module FloatPreRing = struct 
   type t = float 
   let zero = 0. 
   let one = 1. 
   let (+) = (+.) 
   let (~-) = (~-.) 
   let ( * ) = ( *. ) 
   let to_string = string_of_float 
 end 
  
 module FloatRing : Ring = RingOfPreRing(FloatPreRing) 
  
 module FloatPreField = struct 
   include FloatPreRing 
   let (/) = (/.) 
 end 
  
 module FloatField : Field = FieldOfPreField(FloatPreField) 
  
 module Fraction (F:Field) = struct 
   type t = F.t * F.t 
   let zero = (F.zero, F.one) 
   let one = (F.one, F.one) 
   let (+) (a,b) (c,d) = F.(a*d + c*b, b*d) 
   let (~-) (a,b) = F.(-a,b) 
   let ( * ) (a,b) (c,d) = F.(a*c, b*d) 
   let (/) (a,b) (c,d) = (a,b) * (d,c) 
   let to_string (a,b) = F.((to_string a) ^ "/" ^ (to_string b)) 
 end 
  
 module IntRational : Field = FieldOfPreField(Fraction(IntField)) 
  
 module FloatRational : Field = FieldOfPreField(Fraction(FloatField));;