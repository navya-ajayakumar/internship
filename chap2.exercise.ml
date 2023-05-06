(*chap2:values
What is the type and value of each of the following OCaml expressions?
1)7 * (1 + 2 + 3)
2)"CS " ^ string_of_int 3110*)

let a:int=7*(1+2+3);;
let b:string="cs" ^ string_of_int 3110;;

(*output:
   1)val a : int = 42
   2)val b : string = "cs3110"*)  

(*chap2:expressions
1)Write an expression that multiplies 42 by 10.
2)Write an expression that divides 3.14 by 2.0.
3)Write an expression that computes 4.2 raised to the seventh power.*)
let a=42*10;;
let b=3.14/.2.0;;
let c=4.2**7.0;;
(*output:
   1)val a : int = 420
   2)val b : float = 1.57
   3)val c : float = 23053.933324800004*)

(*chap2:equality
1)Write an expression that compares 42 to 42 using structural equality.
2)Write an expression that compares "hi" to "hi" using structural equality. What is the result?
3)Write an expression that compares "hi" to "hi" using physical equality. What is the result?*) 
let a=42=42;;  
let b="hi"="hi";;
let c="hi"=="hi";;
(*output:
1)val a : bool = true
2)val b : bool = true
3)val c : bool = false*)
(*chap2:assert
1)Enter assert true;; into utop and see what happens.
2)Enter assert false;; into utop and see what happens.
3)Write an expression that asserts 2110 is not (structurally) equal to 3110.*)
assert true;;
assert false;;
assert (not(2110=3110));;
(*output:
1) unit = ()
2) Assert_failure ("//toplevel//", 1, 0).
3) unit = ()*)
(*chap2:if
  Write an if expression that evaluates to 42 if 2 is greater than 1 and otherwise evaluates to 7.*)
 let a=if 2>1 then 42 else 7;;
  (*output:val a : int = 42*)
  (*chap2:fun
Using the increment function from above as a guide, define a function double that multiplies its input by 2.Turn those test cases into assertions.*)
let double a = 2 *a;;
let a=2 in(double a);;
let a=23 in(double a);;
let a=assert(double 2=4);;
let b=assert(double 2=5);;
let a=assert(double 23=46);;
let b=assert(double 23=89);;
(*output:
  val double : int -> int = <fun> 
  - : int = 4(if a=2)
  - : int = 8(if a=4)
  - : int = 23(if a=46)
    //by using assertions;
  o/p:1.1)val a : unit = ()
  2)Assert_failure ("//toplevel//", 1, 6).
  1.2)val a : unit = ()
  2)Assert_failure ("//toplevel//", 1, 6).*)
(*chap2:more fun
1)Define a function that computes the cube of a floating-point number. Test your function by applying it to a few inputs.
2)Define a function that computes the sign (1, 0, or -1) of an integer. Use a nested if expression. Test your function by applying it to a few inputs.
3)Define a function that computes the area of a circle given its radius. Test your function with assert.*)
let cube a=a*.a*.a;;
let a=9.67 in (cube a);;
let a=3.00 in (cube a);;
let a=assert(cube 9.67=904.231063);;
let a=assert(cube 9.67=390.3456);;
let a=assert(cube 3.0=27.00);;
let a=assert(cube 3.0=34.00);;

let intsign s=if s > 0 then 1 else if s < 0 then -1 else 0;;
let s=2 in(intsign s);;
let s= -4 in(intsign s);;
let s= 0 in(intsign s);;

let areacir r=3.14*.r**2.0;;
let r=2.0 in (  areacir r);;
let a=assert(areacir 2.0=12.56);;
let a=assert(areacir 2.0=11.35);;
 
(*output:
1)
 val cube : float -> float = <fun>
 1) - : float = 904.231063
 2)- : float = 27.
 1.1)--val a : unit = ()
 1.2)Assert_failure ("//toplevel//", 1, 6).
 2.1)val a : unit = ()
 2.2)Assert_failure ("//toplevel//", 1, 6).
2)
val intsign : int -> int = <fun>
- : int = 1(when s=2)
- : int = -1(when s=-4)
- : int = 0(when s=0)
3)
val areacir : float -> float = <fun>
- : float = 12.56
//checking with assert
1.1)val a : unit = ()
1.2)Assert_failure ("//toplevel//", 1, 6).*)
(*chap2:RMS
Define a function that computes the root mean square of two numbers—i.e., (((x2+y2)/2)1/2)Test your function with assert*)
let rms x y=sqrt(x *. x +. y *. y) /. 2.;;
let a = rms 1.00 2.00;;
let b=assert(a=1.1180339887498949);;
(*output:
val rms : float -> float -> float = <fun> 
val a : float = 1.1180339887498949 
val b : unit = ()*)
(*chap2:date fun
   Define a function that takes an integer d and string m as input and returns true just when d and m form a valid date*)
   let validity_check d m =
    if m = "Jan" || m = "Mar" || m = "May" || m = "Jul"|| m = "Aug" || m = "Oct" || m = "Dec"
    then 1 <= d && d <= 31
    else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
    then 1 <= d && d <= 30
    else if m = "Feb"
    then 1 <= d && d <=28 
  else   false;;
    let f=validity_check 23 "Feb";;
    let f=validity_check 31 "Feb";;
(*output:
   val validity_check : int -> string -> bool = <fun>
   val f : bool = true
   val f : bool = false*)



  (*chap2: fib
 Define a recursive function fib : int -> int, such that fib n is the nth number in the Fibonacci sequence, which is 1, 1, 2, 3, 5, 8, 13, … That is:
    fib 1 = 1, fib 2 = 1, and fib n = fib (n-1) + fib (n-2) for any n > 2.Test your function in the toplevel.*)
let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-1) + fib (n-2);;
  let a= fib 93 ;;
  (*output:
     val fib : int -> int = <fun>
     val a : int = 8*)

(*chap2:fib fast
 Create a function fib_fast that requires only a linear amount of work. What is the first value of n for which fib_fast n is negative, indicating that integer overflow occurred?*)
 let rec h n pp p = if n = 1 then p else h (n-1) p (pp+p)
let fib_fast n=h n 0 1;;
 let a=h 6 0 1;;  
let rec first_negative f n0 =let res = f n0 in if res < 0 then n0 else first_negative f (n0+1);;
 let a=  first_negative fib_fast 94;;
  (*output:
 1) val h : int -> int -> int -> int = <fun>
  val a : int = 8
  2)val first_negative : (int -> int) -> int -> int = <fun>
  val a : int = 1293530146158671551
  val a : int = 96*)
  
(*chap2:poly types 
 What is the type of each of the functions below? You can ask the toplevel to check your answers*)
let f x = if x then x else x;;
let g x y = if y then x else x;;
let h x y z = if x then y else z;;
let i x y z = if x then y else y;;

(*output:
   val f : bool -> bool = <fun>
   val f : bool -> bool = <fun>
   val h : bool -> 'a -> 'a -> 'a = <fun>
   val h : bool -> 'a -> 'a -> 'a = <fun>*)


(*chap2:divide 
Write a function divide : numerator:float -> denominator:float -> float. Apply your function.*)
  let divide x y = x /. y;;
  let a=divide 2.00 3.98;;
 (*output:val divide : float -> float -> float = <fun>
 val a : float = 0.50251256281407031*)

(*chap2:associativity
 Suppose that we have defined let add x y = x + y. Which of the following produces an integer, which produces a function, and which produces an error? Decide on an answer, then check your answer in the toplevel*)
 let add x y = x + y;;
 add 5 1;;
 add 5;;
(add 5) 1;;
add (5 1);;
(*output:
  val add : int -> int -> int = <fun> //function 
  - : int = 6//add 5 1;;
  - : int -> int = <fun>//add 5;;
  - : int = 6//(add 5) 1;;
  will produce an error//add (5 1);;*)

(*chap2:average 
  Define an infix operator +/. to compute the average of two floating-point numbers. For example,*)
let (+/.) a b =(a +. b) /. 2.;;
let d=1.0 +/. 2.0 ;;
let d=0. +/. 0.;;
(*output:
val ( +/. ) : float -> float -> float = <fun>
val d : float = 1.5
val d : float = 0.*)

(*chap2:hello world 
Type the following in utop and anaylse the out*)
print_endline "Hello world!";;
print_string "Hello world!";;
(*output:
1)Hello world!
- : unit = ()
2)Hello world!- : unit = ()*)






  


    
    
    

