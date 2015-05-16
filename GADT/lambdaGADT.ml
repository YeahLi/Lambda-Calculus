type _ term =
  | Var : string -> 'a term
  | Int : int -> int term
  | Bool : bool -> bool term	
  | Apply : ('a -> 'b) term * 'a term -> 'b term
  | Abs : ('a term -> 'b term) -> ('a -> 'b) term (* 'a term reprents the bound variable *)
  | Let_In : string * 'a term * ('a -> 'b) term -> 'b term
  | Minus : int term -> int term
  | Not : bool term -> bool term
  | Plus : int term * int term -> int term
  | Multiply : int term * int term -> int term
  | And : bool term * bool term -> bool term
  | Or : bool term * bool term -> bool term
  | Equal : int term * int term -> bool term
  | Less : int term * int term -> bool term
  | Less_Equal : int term * int term -> bool term
  | If_Then_Else : bool term * 'a term * 'a term -> 'a term
;;

let rec eval: type a. a term -> a term= function
  | Var t -> failwith "throws an error if it encounters a free variable"
  | Int t -> Int t
  | Bool t -> Bool t
  | Abs f -> Abs f
  | Apply (Var _, _) -> failwith "throws an error if it encounters a free variable"
  | Apply (Abs f,t2) -> eval (f (eval t2))
  | Apply (t1,t2) -> eval (Apply(eval(t1),eval(t2)))
  | Minus (Int i) -> eval (Int (-i))
  | Minus (t1) -> eval (Minus (eval t1))
  | Not (Bool b) -> eval (Bool (not b))
  | Not (t1) -> eval (Not (eval t1))
  | Plus (Int a, Int b) -> eval (Int (a+b))
  | Plus (t1,t2) -> eval (Plus ((eval t1),(eval t2)))
  | Multiply (Int a, Int b) -> eval (Int (a*b))
  | Multiply (t1,t2) -> eval (Multiply ((eval t1),(eval t2)))
  | And (Bool a, Bool b) -> eval (Bool (a&&b))
  | And (t1,t2) -> eval (And ((eval t1),(eval t2)))
  | Or (Bool a, Bool b) -> eval (Bool (a||b))
  | Or (t1,t2) -> eval (Or ((eval t1),(eval t2)))
  | Equal (Int a,Int b) -> eval (Bool (a=b))
  | Equal (t1,t2) -> eval (Equal ((eval t1),(eval t2)))
  | Less (Int a, Int b) -> eval (Bool (a<b))
  | Less (t1,t2) -> eval (Less ((eval t1),(eval t2)))
  | Less_Equal (Int a, Int b) -> eval (Bool (a<=b))
  | Less_Equal (t1,t2) -> eval (Less_Equal ((eval t1),(eval t2)))
  | If_Then_Else (Bool b, t2, t3) -> if b then (eval t2) else (eval t3)
  | If_Then_Else (t1,t2,t3) -> eval (If_Then_Else ((eval t1), t2, t3))
  | Let_In (x,t1,t2) -> eval (Apply(t2,t1))
;;
(*test*)
let exp1 = Apply(Abs (fun x -> x),Minus(Int 5));;
let result = eval exp1;;
let exp2 = Apply(Abs(fun x -> Abs(fun y -> Plus(y,x))),Int 8);;
let result = eval exp2;; 
let exp3 = Apply(Abs (fun x -> Abs (fun y -> And (x,y))),Less(Int 19,Int 8)) ;;
let result =eval exp3;;
let exp4 = Let_In ("x",Int 9, Abs(fun x -> Multiply(Int 5,x)));;
let result = eval exp4;;
let exp5 = If_Then_Else (Less(Int 5,Int 9),exp4,Int 0);;
let result =eval exp5;;
let exp6 = If_Then_Else (Less(Int 9,Int 5),exp4,Int 0);;
let result =eval exp6;;

let exp1 = Apply(Abs(fun x -> Abs(fun y -> Apply(y,x))), Abs(fun y ->y));;
let res = eval exp1;;

let exp2 = Apply(Abs(fun x -> Abs(fun y -> Apply(x,y))), Abs(fun x ->Apply(Abs(fun y->y),x)));;
let res = eval exp2;;

let exp3 = Apply(Abs(fun x -> Apply(x,Var "y")),Abs(fun x -> Apply(Var "y",x)));;
let res = eval exp3;;
(* result *)
(* 
val eval : 'a term -> 'a term = <fun>
val exp1 : int term = Apply (Abs <fun>, Minus (Int 5))
val result : int term = Int (-5)
val exp2 : (int -> int) term = Apply (Abs <fun>, Int 8)
val result : (int -> int) term = Abs <fun>
val exp3 : (bool -> bool) term = Apply (Abs <fun>, Less (Int 19, Int 8))
val result : (bool -> bool) term = Abs <fun>
val exp4 : int term = Let_In ("x", Int 9, Abs <fun>)
val result : int term = Int 45
val exp5 : int term =
  If_Then_Else (Less (Int 5, Int 9), Let_In ("x", Int 9, Abs <fun>), Int 0)
val result : int term = Int 45
val exp6 : int term =
  If_Then_Else (Less (Int 9, Int 5), Let_In ("x", Int 9, Abs <fun>), Int 0)
val result : int term = Int 0 
val exp : ((('a -> 'a) -> 'b) -> 'b) term = Apply (Abs <fun>, Abs <fun>)
- : ((('_a -> '_a) -> '_b) -> '_b) term = Abs <fun>
*)
