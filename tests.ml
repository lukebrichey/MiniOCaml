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
let binop_broken = Binop (Plus, bool_f, bool_t) ;;  

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

(* Test letrec *)
let fact = 
    Letrec ("fact", 
            Fun ("n", Conditional (Binop (Equals, Var ("n"), Num (0)), 
                                            Num (1),
                                          Binop (Times, 
                                                 Var ("n"), 
                                                 App (Var ("fact"), 
                                                     Binop (Minus, 
                                                            Var ("n"), 
                                                            Num (1)))))), 
            App (Var ("fact"), Num (3))) ;;



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
                "Let f1 = fun x -> x + 2 in (f1) 35") 
    "exp_c Let + App" ;

    (* exp_to_abstract_string tests *)
    unit_test (exp_to_abstract_string fact = 
        "Letrec (fact, Fun (n, Conditional (Binop (Equals, Var (n), Num (0)), " ^
        "Num (1), Binop (Times, Var (n), App (Var (fact), Binop (Minus, Var (n), " ^
        "Num (1)))))), App (Var (fact), Num (3)))")
    "exp_a fact" ;;

let stage2_tests () = 
    unit_test (free_vars (Num 2) = vars_of_list []) 
        "free_vars num";
    unit_test (free_vars (Var ("x")) = vars_of_list ["x"]) 
        "free_vars var x";
    unit_test (free_vars (Let ("f", Fun ("y", 
                                            Binop (Plus, Binop (Plus, Var ("x"), 
                                                                      Var ("z")), 
                                                         Var ("y"))), 
                                        App (Var ("f"), Var ("w")))) 
                = vars_of_list ["x"; "z"; "w"])
        "free_vars binop + fun + app + let";
    unit_test (free_vars (App (Var ("f"), Var ("x"))) = vars_of_list ["f"; "x"]) 
        "free_vars app";
    unit_test (free_vars (Let ("x", 
                              Binop (Plus, Var ("x"), Num (2)), 
                              Binop (Plus, Var ("x"), Var ("y"))))
                = vars_of_list ["y"; "x"])
        "free_vars let";
    unit_test (free_vars (Let ("x", Fun ("y", Var ("x")), Var ("x")))
                = vars_of_list ["x"])
        "free vars let + app";
    unit_test (free_vars (Letrec ("f", Fun ("x", Binop (Plus, 
                                                             Binop (Plus, Var ("x"), 
                                                             Var ("z")), 
                                                      App (Var ("f"), Var ("y")))), 
                                            App (Var ("f"), Var ("l"))))
                = vars_of_list ["y"; "z"; "l"]) 
        "free_vars let rec";
    unit_test (free_vars (Conditional (Var ("f"), Var ("x"), Var ("y"))) 
                = vars_of_list ["f"; "x"; "y"])
        "free_vars conditional";;

let stage3_tests () =
    (* new_varname tests *)
    unit_test (new_varname () = "var0") "new_varname first call";
    unit_test (new_varname () = "var1") "new_varname second call";

    (* subst tests *)
    unit_test (subst varx num1 num3 = Num (17)) "subst num (same as other literals)";
    let init_sub1 = (subst "f" bool_t (Conditional (Var ("f"), Var ("x"), Var ("y")))) in
    unit_test (subst "y" num2 (subst "x" num1 init_sub1) 
                = Conditional (Bool (true), num1, num2))
        "subst conditional";
    unit_test (subst "f" fun1 (App (Var ("f"), Num (2))) = App (fun1, Num (2)))
        "subst App";
    unit_test (subst varx num1 (Fun (varx, Binop (Plus, Var (varx), Num (2))))
                = Fun (varx, Binop (Plus, Var (varx), Num (2))))
        "subst fun base case";
    unit_test (subst varx num1 (Fun (var0, Binop (Plus, Var (varx), Num (2))))
                = Fun (var0, Binop (Plus, num1, Num (2))))
        "subst fun 13.13";
    unit_test (subst varx num1 (Fun (var0, Binop (Plus, Var (varx), Num (2))))
                = Fun (var0, Binop (Plus, num1, Num (2))))
        "subst fun 13.13";
    let init_sub2 = 
        subst "f" (Fun ("z", Var ("y"))) (App (Fun ("y", Binop (Plus, 
                                                                App (Var ("f"), 
                                                                    Num (3)), 
                                                                    Var ("y"))), 
                                               Num (1))) in
    unit_test (free_vars init_sub2
                = vars_of_list ["y"])
        "subst fun 13.14 + app";
    unit_test (subst "x" num1 (Let ("x", Binop (Plus, Var ("x"), Num (2)), Var ("x")))
                = Let ("x", Binop (Plus, Num (2), Num (2)), Var ("x")))
        "let 13.15";
    unit_test 
        (subst "x" num1 (Let ("y", 
                             Binop (Plus, Var ("x"), Num (7)), 
                             Binop (Plus, Var ("x"), Var ("y"))))
        = Let ("y", Binop (Plus, Num (2), Num (7)), Binop (Plus, Num (2), Var ("y"))))
        "let 13.16";
    unit_test (subst "x" 
                     (Binop (Plus, Var ("y"), Num (3))) 
                     (Let ("y", Binop (Plus, Var ("x"), Num (2)), Var ("x"))) 
                = Let ("var3", 
                      Binop (Plus, Binop (Plus, Var ("y"), Num (3)), Num (2)), 
                      Binop (Plus, Var ("y"), Num (3))))
        "let 13.17";;

let stage4_tests () =
    (* Env not relevant to eval_s *)
    let empty = Env.empty () in 
    (* Substitution evaluation tests *)
    unit_test (eval_s num1 empty = Env.Val (num1))
        "eval_s Num/other literals";
    unit_test (try 
                eval_s (Var ("x")) empty <> eval_s (Var ("x")) empty 
              with 
                EvalError (_) -> true | _ -> false)
        "eval_s unbound Var";
    unit_test (eval_s cond_1 empty = Env.Val (num1))
        "eval_s conditional";
    unit_test (eval_s fun1 empty = Env.Val (fun1))
        "eval_s fun";
    unit_test (eval_s binop_ints empty = Env.Val (Num (19)))
        "eval_s binop";
    unit_test (try 
                eval_s binop_broken empty <> eval_s binop_broken empty 
              with 
                EvalError (_) -> true | _ -> false)
        "eval_s Binop fail";
    unit_test (eval_s let1 empty = Env.Val (Num (37)))
        "eval_s let + app";
    unit_test (try
                eval_s (App (Num (4), Num (3))) empty <> eval_s (App (Num (4), Num (3))) empty
              with
                EvalError (_) -> true | _ -> false) 
        "eval_s App fail";
    unit_test (eval_s fact empty = Env.Val (Num 6))
        "eval_s let rec + conditional + app";;

let stage6_tests () =
    (* Env module tests *)
    let empty = Env.empty () in 
    unit_test (try 
                Env.lookup empty "x" <> Env.lookup empty "x" 
              with 
                EvalError (_) -> true | _ -> false)
        "lookup fail";
    unit_test (Env.value_to_string (Closure (Num (1), empty)) = "Val = 1, Env : {}")
        "value_to_string closure w env";
    unit_test (Env.value_to_string ~printenvp: false (Closure (Num (1), empty)) = "Val = 1, Env : {}")
        "value_to_string closure w/o env";
    let test_env = 
        Env.extend (Env.extend empty "x" (ref (Env.Val (Num 2)))) "y" (ref (Env.Val (Num 17))) in
    unit_test (Env.env_to_string test_env = "{x |-> 2;y |-> 17;}")
        "env_to_string regular environment + extend";
    unit_test (Env.lookup test_env "x" = Env.Val (Num 2))
        "lookup test1" ;
    let test_env2 = Env.extend test_env "x" (ref (Env.Val (Num 25))) in 
    unit_test (Env.env_to_string test_env2 = "{y |-> 17;x |-> 25;}")
        "extend override";
    unit_test (Env.lookup test_env2 "x" = Env.Val (Num 25))
        "lookup test2" ;;

let stage7_tests () = 
    let empty = Env.empty () in 
    (* Dynamic environment evaluation tests *)
    unit_test (eval_d num1 empty = Env.Val (num1))
        "eval_d Num/other literals";
    unit_test (try 
                eval_d (Var ("x")) empty <> eval_d (Var ("x")) empty 
              with 
                EvalError (_) -> true | _ -> false)
        "eval_d unbound Var";
    unit_test (eval_d cond_1 empty = Env.Val (num1))
        "eval_d conditional";
    unit_test (eval_d fun1 empty = Env.Val (fun1))
        "eval_d fun";
    unit_test (eval_d binop_ints empty = Env.Val (Num (19)))
        "eval_d binop";
    unit_test (try 
                eval_d binop_broken empty <> eval_d binop_broken empty 
              with 
                EvalError (_) -> true | _ -> false)
        "eval_d Binop fail";
    unit_test (eval_d let1 empty = Env.Val (Num (37)))
        "eval_d let + app";
    unit_test (eval_d (Let ("x", Num (5), 
                       Let ("f", Fun ("y", Binop (Plus, Var ("x"), Var ("y"))), 
                       Let ("x", Num (3), App (Var ("f"), Num (4)))))) 
                       empty
                = Env.Val (Num 7))
        "eval_d dynamic update";
    let test_env = 
        Env.extend (Env.extend empty "x" (ref (Env.Val (Num 2)))) "y" (ref (Env.Val (Num 17))) in 
    unit_test (eval_d (Binop (Plus, Var ("x"), Var ("y"))) test_env = Env.Val (Num 19))
        "eval_d environment needed";
    unit_test (try
                eval_d (App (Num (4), Num (3))) empty <> eval_d (App (Num (4), Num (3))) empty
              with
                EvalError (_) -> true | _ -> false) 
        "eval_d App fail";
    unit_test (eval_d fact empty = Env.Val (Num 6))
        "eval_d let rec + conditional + app";;

let stage8_tests () =
    let empty = Env.empty () in 
    (* Lexical environment evaluation tests *)
    unit_test (eval_l num1 empty = Env.Val (num1))
        "eval_l Num/other literals";
    unit_test (try 
                eval_l (Var ("x")) empty <> eval_l (Var ("x")) empty 
              with 
                EvalError (_) -> true | _ -> false)
        "eval_l unbound Var";
    unit_test (eval_l cond_1 empty = Env.Val (num1))
        "eval_l conditional";
    unit_test (Env.value_to_string (eval_l fun1 empty) = "Val = fun x -> x + 2, Env : {}")
        "eval_l fun";
    unit_test (eval_l binop_ints empty = Env.Val (Num (19)))
        "eval_l binop";
    unit_test (try 
                eval_l binop_broken empty <> eval_l binop_broken empty 
              with 
                EvalError (_) -> true | _ -> false)
        "eval_l Binop fail";
    unit_test (eval_l let1 empty = Env.Val (Num (37)))
        "eval_l let + app";
    unit_test (eval_l (Let ("x", Num (5), 
                       Let ("f", Fun ("y", Binop (Plus, Var ("x"), Var ("y"))), 
                       Let ("x", Num (3), App (Var ("f"), Num (4)))))) 
                       empty
                = Env.Val (Num 9))
        "eval_l diff than dynamic";
    let test_env = 
        Env.extend (Env.extend empty "x" (ref (Env.Val (Num 2)))) "y" (ref (Env.Val (Num 17))) in 
    unit_test (eval_l (Binop (Plus, Var ("x"), Var ("y"))) test_env = Env.Val (Num 19))
        "eval_l environment needed";
    unit_test (try
                eval_l (App (Num (4), Num (3))) empty <> eval_l (App (Num (4), Num (3))) empty
              with
                EvalError (_) -> true | _ -> false) 
        "eval_l App fail";
    unit_test (eval_l fact empty = Env.Val (Num 6))
        "eval_l let rec + conditional + app";;
    
let tests () =
  (* stage1 tests *)
  stage1_tests ();
  (* stage2 tests *)
  stage2_tests ();
  (* stage3_tests *)
  stage3_tests ();
  (* stage4_tests *)
  stage4_tests ();
  (* stage6_tests *)
  stage6_tests ();
  (* stage7_tests *)
  stage7_tests ();
  (* stage8_tests *)
  stage8_tests ();

  () ;;

let _ = tests () ;;