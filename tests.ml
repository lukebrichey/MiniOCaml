open Expr ;;
open Evaluation ;;
open CS51Utils ;;
open Absbook ;;

(* Test expressions *)
let varx = "x" ;;
let var0 = "var0" ;; 
let var1 = "var1" ;;
let var2 = "var2" ;;


(* Test nums *)
let num1 = Num 2 ;;
let num2 = Num 35 ;; 
let num3 = Num 17 ;;

(* Test bools *)
let bool_t = Bool true ;;
let bool_f = Bool false ;;  

(* Test unops *)
let unop = Unop (Negate, num1) ;; 
let broken_unop = Unop (Negate, bool_t) ;;

(* Test binops *)
let binop_ints = Binop (Plus, num1, num3) ;; 
let binop_bool1 = Binop (Equals, bool_t, bool_f) ;; 
let binop_comp_int = Binop (LessThan, num1, num2) ;; 
let binop_bool2 = Binop (LessThan, bool_t, bool_f) ;; 

(* Test conditionals *)
let cond_1 = Conditional (bool_t, num1, num2) ;; 
let cond_2 = Conditional (bool_f, num1, num2) ;;

(* Test funs *)
let fun1 = Fun (varx, Binop (Plus, Var (varx), num1)) ;; 
let fun2 = Fun (var0, Conditional (Var (var0), num2, num3)) ;; 
let fun3 = Fun (var1, Binop (Times, Var (var1), num1)) ;;

(* Test let statements *)
let let1 = Let ("f1", fun1, App (Var("f1"), num2)) ;; 
let let2 = Let ("f2", fun2, App (Var("f2"), bool_t)) ;; 


(* Test letrecs *)
let fact = 
    Letrec ("fact", Fun ("n", Conditional (Binop (Equals, Var ("n"), Num (0)), 
                                            Num (1),
                                           Binop (Times, Var ("n"), App (Var ("fact"), Binop (Minus, Var ("n"), Num (1)))))), 
            App (Var ("fact"), 
            Num (3))) ;;

(* Tests for exp_to_concrete string and exp_to_abstract string *)
let stage1_tests () = 
    (* exp_to_concrete_string tests *)
    unit_test (exp_to_concrete_string (Var (varx)) = "x") "exp_c Var";
    unit_test (exp_to_concrete_string num1 = "2") "exp_c Num";
    unit_test (exp_to_concrete_string bool_t = "true") "exp_c Bool";
    unit_test (exp_to_concrete_string unop = "~-2") "exp_c Unop";
    unit_test (exp_to_concrete_string binop_ints = "2 + 17") "exp_c Binop";
    unit_test (exp_to_concrete_string cond_1 = "if true then 2 else 35") "exp_c Cond";
    unit_test (exp_to_concrete_string fun1 = "fun x -> x + 2") "exp_c Fun";
    unit_test (exp_to_concrete_string let1 = 
                "Let f1 = fun x -> x + 2 in f1 35") 
    "exp_c Let + App" ;

    (* exp_to_abstract_string tests *)
    unit_test (exp_to_abstract_string fact = 
        "Letrec (fact, Fun (n, Conditional (Binop (Equals, Var (n), Num (0)), " ^
        "Num (1), Binop (Times, Var (n), App (Var (fact), Binop (Minus, Var (n), " ^
        "Num (1)))))), App (Var (fact), Num (3)))")
    "exp_a fact" ;;

let tests () =
  (* stage1 tests *)
  stage1_tests ();

  () ;;

let _ = tests () ;;