open Projet
open Printf

(* variables pour les test*)

let p1 = Var "P1"
let p2 = Var "P2"
let q1 = Var "Q1"
let q2 = Var "Q2"
let f1 = Equivalent(q1,q2)
let f2 = Equivalent(p1,p2)
let ex1 = And(f1,f2)
let a = [1;2;2;3;3;3;4;4;5]
let env = [("P1",true);("P2",false);("Q1",false);("Q2",true)]

(*fonctions de conversion en string pour affichage*)

let rec string_of_formula f = match f with
Value(b) -> (string_of_bool b)
|Var(s) -> s
|Not(t) -> "Not ("^string_of_formula t^")"
|And(t1,t2) -> "("^(string_of_formula t1)^") And ("^(string_of_formula t2)^")"
|Or(t1,t2) -> "("^(string_of_formula t1)^") Or ("^(string_of_formula t2)^")"
|Implies(t1,t2) -> "("^(string_of_formula t1)^") Implies ("^(string_of_formula t2)^")"
|Equivalent(t1,t2) -> "("^(string_of_formula t1)^") Equivalent ("^(string_of_formula t2)^")"

let string_of_decTree dec = 
let rec aux1 i = if i = 0 then "" else " "^(aux1 (i-1)) in 
let rec aux2 dec i =match dec with
DecLeaf(b) ->"DecLeaf "^(string_of_bool b)
|DecRoot(s,g,d) -> "\n"^(aux1 i)^"DecRoot("^s^","^aux2 g (i+1)^","^aux2 d (i+1)^")"
in aux2 dec 0

let rec string_of_Bddnode bddnode = match bddnode with
BddLeaf(i,b) -> "BddLeaf("^string_of_int i^","^string_of_bool b^")"
|BddNode(i,s,g,d) -> "BddNode("^string_of_int i^","^s^","^string_of_int g^","^string_of_int d^")"

let string_of_Bdd bdd = let (int0,bddlist) = bdd in
"("^string_of_int int0^",["^
let rec f x = match x with t::q -> string_of_Bddnode t^";"^(f q)|[] -> "" in f bddlist
^"]"^")"

let string_of_DecTreeNbr dec =
let rec aux1 i = if i = 0 then "" else " "^(aux1 (i-1)) in 
let rec aux2 dec n =match dec with
DecLeafNbr(i,b) ->"DecLeafNbr("^string_of_int i^", "^(string_of_bool b)^")"
|DecRootNbr(i,s,g,d) -> "\n"^(aux1 n)^"DecRootNbr("^string_of_int i^", "^s^","^aux2 g (n+1)^","^aux2 d (n+1)^")"
in aux2 dec 0

(*affichage des tests*)

let () = printf "Tests des fonctions de la partie 1 :\n\n"
let () = printf "a = [1;2;2;3;3;3;4;4;5]\n"
let () = printf "length a : %d\n" (length a)
let () = printf "isin a 2 : %b\n" (isin a 2)
let () = printf "isin a 6 : %b\n" (isin a 6)
let () = printf "unic a : ["
let () = List.iter (printf "%d;") (unic a)
let () = printf "]\n"
let () = printf "\nTests des fonctions de la partie 2 : \n\n"
let () = printf "ex1 = %s\n" (string_of_formula ex1)
let () = printf "getVars ex1 : ["
let () = List.iter (printf "%s;") (getVars ex1)
let () = printf "]\n"
let () = printf "env = [(\"P1\",true);(\"P2\",false);(\"Q1\",false);(\"Q2\",true)]\n"
let () = printf "findvalue env \"Q1\" : %b\n" (findvalue env "Q1")
let () = printf "evalFormula env ex1 : %b\n" (evalFormula env ex1)
let () = printf "buildDecTree ex1 : %s\n" (string_of_decTree (buildDecTree ex1))
let () = printf "\nTests des fonctions de la partie 3 : \n\n"
let () = printf "isDecLeafIn (buildDecTree ex1) (DecLeaf(true)) : %b\n" (isDecLeafIn (buildDecTree ex1) (DecLeaf(true)))
let () = printf "isDecLeafIn (buildDecTree (Or(Not(p1),p1))) (DecLeaf(false)) : %b\n" (isDecLeafIn (buildDecTree (Or(Not(p1),p1))) (DecLeaf(false)))
let () = printf "buildBdd ex1 = %s\n" (string_of_Bdd (buildBdd ex1))
let () = printf "\nTests des fonctions de la partie 4 : \n\n"
let () = printf "simplifyBdd (buildBdd ex1) : %s\n" (string_of_Bdd (simplifyBdd (buildBdd ex1)))
let () = printf "\nTests des fonctions de la partie 5 : \n\n"
let () = printf "isTautologyNoBdd Or(q1,q1) : %b\n" (isTautologyNoBdd (Or(q1,q1)))
let () = printf "isTautologyNoBdd Or(Not(q1),q1) : %b\n" (isTautologyNoBdd (Or(Not(q1),q1)))
let () = printf "isTautology Or(q1,q1) : %b\n" (isTautology (Or(q1,q1)))
let () = printf "isTautology Or(Not(q1),q1) : %b\n" (isTautology (Or(Not(q1),q1)))
let () = printf "areEquivalentNoBdd (Implies(p1,p2)) (Or(Not(p1),p2)) : %b\n" (areEquivalentNoBdd (Implies(p1,p2)) (Or(Not(p1),p2)))
let () = printf "areEquivalentNoBdd (Implies(p1,p2)) (Or(p1,p2)) : %b\n" (areEquivalentNoBdd (Implies(p1,p2)) (Or(p1,p2)))
let () = printf "areEquivalent (Implies(p1,p2)) (Or(p1,p2)) : %b\n" (areEquivalent (Implies(p1,p2)) (Or(p1,p2)))
let () = printf "areEquivalent (Implies(p1,p2)) (Or(q1,p2)) : %b\n" (areEquivalent (Implies(p1,p2)) (Or(q1,p2)))
let () = printf "\nTests des fonctions de la partie 6 : \n\n"
let () = printf "nbrTree (buildDecTree ex1) : %i\n" (nbrTree (buildDecTree ex1))
let () = printf "addNbrTree (buildDecTree ex1) : %s\n"(string_of_DecTreeNbr (addNbrTree (buildDecTree ex1)))