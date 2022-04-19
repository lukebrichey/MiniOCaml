(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let free_vars (exp : expr) : varidset =
  failwith "free_vars not implemented" ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num x -> string_of_int x
  | Bool b -> string_of_bool b   
  | Unop (_, e) -> "~-" ^ exp_to_concrete_string e                 
  | Binop (b, e1, e2) -> 
    let s1 = exp_to_concrete_string e1 in 
    let s2 = exp_to_concrete_string e2 in                     
    (match b with
    | Plus -> s1 ^ " + " ^ s2
    | Minus -> s1 ^ " - " ^ s2
    | Times -> s1 ^ " * " ^ s2
    | Equals -> s1 ^ " = " ^ s2
    | LessThan -> s1 ^ " <= " ^ s2)
  | Conditional (e1, e2, e3) ->  "if " ^ exp_to_concrete_string e1 ^
                                 " then " ^ exp_to_concrete_string e2 ^ 
                                 " else " ^ exp_to_concrete_string e3
  | Fun (v, e) -> v ^ exp_to_concrete_string e                
  | Let (v, e1, e2) -> "Let " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^ exp_to_concrete_string e2
  | Letrec (v, e1, e2) -> "Let rec " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^ exp_to_concrete_string e2
  | Raise -> "Exception"
  | Unassigned -> "Unassigned"
  | App (f, e) -> exp_to_concrete_string f ^ exp_to_concrete_string e ;;;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var("^ v ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"  
  | Unop (_, e) -> "Unop(Negate, " ^ exp_to_concrete_string e ^ ")"                
  | Binop (b, e1, e2) -> 
    (match b with
    | Plus -> "Binop(Plus, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | Minus -> "Binop(Minus, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | Times -> "Binop(Times, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | Equals -> "Binop(Equals, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | LessThan -> "Binop(LessThan, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")")
  | Conditional (e1, e2, e3) ->  "Conditional(" ^ exp_to_abstract_string e1 ^
                                 ", " ^ exp_to_abstract_string e2 ^ 
                                 ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"               
  | Let (v, e1, e2) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (v, e1, e2) -> "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Raise -> "Exception"
  | Unassigned -> "Unassigned"
  | App (f, e) -> "App(" ^ exp_to_abstract_string f ^ ", " ^ exp_to_abstract_string e ^ ")" ;;
