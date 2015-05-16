type op1 = Minus | Not;;
type op2 = Plus | Multiply | And | Or | Equal | Less | Less_Equal;; 
type term =
  | Var of string
  | Int of int 
  | Bool of bool
  | Apply of term * term
  | Abs of string * term (* string reprents the bound variable *)
  | Let_In of string * term * term
  | Op1 of op1 * term
  | Op2 of term * op2 * term
  | If_Then_Else of term * term * term
;;
(* check \y of e1 in FV(e2) *)
let rec conflict x = function
  (Int _ | Bool _) -> false
  | Var y -> y = x
  | Abs (y,e) -> y <> x && (conflict x e)
  | Apply (t1,t2) -> (conflict x t1) || (conflict x t2)
  | Op1 (op,t) -> conflict x t
  | Op2 (t1,op,t2) -> (conflict x t1) || (conflict x t2)
  | If_Then_Else (t1, t2, t3) -> (conflict x t1) || (conflict x t2) || (conflict x t3)
  | Let_In (y,t1,t2) -> y<>x && ((conflict x t1) || (conflict x t2))
;;
(* rename *)
let rec alpha x t = 
  let newName = x^x in
  match t with
  | Var s -> if s = x then Var newName else t
  | Abs (s,e)-> alpha x e
  | Apply (t1,t2) -> Apply ((alpha x t1),(alpha x t2))
  | Op1 (op,t1) -> Op1 (op,(alpha x t1))
  | Op2 (t1,op,t2) -> Op2 ((alpha x t1),op,(alpha x t2))
  | If_Then_Else (t1, t2, t3) -> If_Then_Else ((alpha x t1),(alpha x t2),(alpha x t3))
  | Let_In (y,t1,t2) -> 
  	if y = x then Let_In(y^y,(alpha x t1),(alpha y t2))
    else Let_In (y,(alpha x t1),(alpha x t2))
  | _ -> t
;;
(* use e2 to substitute \x.e1 [x->e2]e1*)
let rec subst x e2 e1 = 
  match e1 with
    (Int _|Bool _) -> e1
	(* [x->s]x = s,[x->s]y = y (y<>x) *)
	| Var y -> if y = x then e2 else begin
					if (conflict y e2) then Var (y^y) (* rename Var y*)
					else e1
				end 
	(* [x->s](\y.t1) = \y.[x->s]t1 if y<>x and y is not in FV(e2) *)
	(*[x->s](\y.t1) = \y.[x->s]t1 if y=x *)
	| Abs (y,t1) -> 
	  if y = x then Abs (y,(subst x e2 t1)) 
	  else begin
	  	if (conflict y e2) then let newExp = alpha y t1 in
	  	Abs ((y^y),(subst x e2 newExp)) (* rename y and y in t1 *)
	  	else Abs (y,(subst x e2 t1)) 
	  end
	| Apply(t1,t2) -> 
		let exp1 = subst x e2 t1 in
		let exp2 = subst x e2 t2 in
		Apply(exp1,exp2)
	| Op1(op,t) -> Op1(op,(subst x e2 t))
	| Op2(t1,op,t2) -> 
		let exp1 = subst x e2 t1 in
		let exp2 = subst x e2 t2 in
		Op2(exp1,op,exp2)	
	| Let_In (y, t1, t2) -> 
	    let exp1 = subst x e2 t1 in
	    if y = x then let newExp = alpha y t2 in Let_In (y^y, exp1, newExp) 
	    (* if y =x, newExp will not include x *)
	    else let exp2 = subst x e2 t2 in Let_In (y, exp1, exp2)
	|If_Then_Else (t1,t2,t3) ->
		let exp1 = subst x e2 t1 in
		let exp2 = subst x e2 t2 in
		let exp3 = subst x e2 t3 in
		If_Then_Else (exp1,exp2,exp3)
;;

let rec beta = function
	| Var t -> Var t
	| Int t -> Int t
	| Bool t -> Bool t
	| Abs (x,e) -> begin (* call-by-value *)
		match e with
		(Int _ | Bool _ | Var _)-> Abs (x,e)
		| t1 -> Abs (x, (beta t1))
	end
	| Apply (Abs (x,e), t) -> beta (subst x t e)
	| Apply (Var a,Var b) -> Apply (Var a, Var b)
	| Apply (Var _, _) -> raise (Invalid_argument "Invalid argument")
	| Apply (Int _, _) -> raise (Invalid_argument "Invalid argument")
	| Apply (Bool _, _) -> raise (Invalid_argument "Invalid argument") 
	| Apply (t1,t2)-> Apply ((beta t1),t2)
	| Op1 (op,t) -> begin
		let v = beta t in
		match (op,v) with
		| (Not,Bool b) -> Bool (not b)
		| (Minus,Int i) -> Int (-i)
		| (_,_) -> raise (Invalid_argument "Invalid argument")
	end	
	| Op2 (t1,op,t2) -> begin
		let v1 = beta t1 in
		let v2 = beta t2 in
		match (v1,op,v2) with
		| (Int a, Plus, Int b) -> Int (a+b)
		| (Int a, Multiply, Int b) -> Int (a*b)
		| (Bool a, And, Bool b) -> Bool (a && b)
		| (Bool a, Or, Bool b) -> Bool (a || b)
		| (Int a, Equal, Int b) -> Bool (a=b)
		| (Int a, Less, Int b) -> Bool (a<b)
		| (Int a, Less_Equal, Int b) -> Bool (a<=b)
		| (_,_,_) -> raise (Invalid_argument "Invalid argument")
	end
	| If_Then_Else (t1, t2, t3) -> begin
		let v = beta t1 in
		match v with
		| Bool b -> if b then (beta t2) else (beta t3)
		| _ -> raise (Invalid_argument "Invalid argument")
	end	
	| Let_In (x,t1,t2) -> beta (subst x t1 t2)
;;

let rec eval t =
	match  t with
	| Int _ | Bool _  -> t
	| Var _ -> failwith "throws an error if it encounters a free variable"
	| Abs (x,e) -> t
	| Apply (Var _, _) -> failwith "throws an error if it encounters a free variable"
	| _ -> eval (beta t)
;;
(*test*)
let exp1 = Apply(Abs ("x",Var "x"),Op1(Minus, Int 5));;
let result = eval exp1;;
let exp3 = Apply(Abs ("x", Abs ("y", Var "x")),Op2(Int 19,Less,Int 8)) ;;
let result =eval exp3;;
let exp4 = Let_In ("x",Int 9,Op2(Int 5,Multiply,Var "x"));;
let result = eval exp4;;
let exp5 = If_Then_Else (Op2(Int 5,Less,Int 9),exp4,Int 0);;
let result =eval exp5;;
let exp6 = If_Then_Else (Op2(Int 9,Less,Int 5),exp4,Int 0);;
let result =eval exp6;;
(*let exp2 = Apply(Abs ("x",Var "x"),Op2(Int 5, Plus ,Bool true));;
let result = eval exp2;; *)
let exp = Apply(Abs("a",Abs("b",Apply(Var "b",Var "a"))),Var "b");;
let res = eval exp;;	

let exp = Apply(Abs("x",Abs("y",Apply(Var "x", Var "y"))),Abs("x",Apply(Var "y",Var "x")));;
let res = eval exp;;

let exp = Apply(Abs("x",Apply(Var "x", Var "y")),Abs("x",Apply(Var "y",Var "x")));;
let res = eval exp;;(* res: Apply (Var "yy", Var "y") so error!!! *)