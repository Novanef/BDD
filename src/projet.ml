type tformula = 
  |Value of bool
  |Var of string
  |Not of tformula
  |And of tformula * tformula
  |Or of tformula * tformula
  |Implies of tformula * tformula
  |Equivalent of tformula * tformula 

type decTree =
  |DecLeaf of bool
  |DecRoot of  string * decTree * decTree


let p1 = Var "P1"
let p2 = Var "P2"
let q1 = Var "Q1"
let q2 = Var "Q2"
let f1 = Equivalent(q1,q2)
let f2 = Equivalent(p1,p2)
let ex1 = And(f1,f2)

let rec length l = match l with
[] -> 0
|t::q -> 1 + length q

let rec isin l e = match l with
[] -> false
|t::q -> if e = t then true else isin q e

let rec unic l = match l with
|[] -> l
|t::q -> if isin q t then unic q else t::(unic q)

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 1*)

let getVars t = let rec aux t l = match t with
Value(s) -> l
|Var(s) -> s::l
|Not(t1) -> aux t1 l
|And(t1,t2) -> (aux t1 l)@(aux t2 l)
|Or(t1,t2) -> (aux t1 l)@(aux t2 l)
|Implies(t1,t2)-> (aux t1 l)@(aux t2 l)
|Equivalent(t1,t2)-> (aux t1 l)@(aux t2 l) in unic (List.sort compare (aux t []))

(*------------------------------------------------------------------------------------------------------------------------------*)

type env = (string*bool) list

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 2*)

let rec findvalue env s = match env with
[] -> failwith"L'environnement ne contient pas la valeur."
|t::q -> match t with
(a,b) -> if a = s then b else findvalue q s

let rec evalFormula env f = match f with
Value(b) -> b
|Var(s) -> findvalue env s
|Not(t) -> not (evalFormula env t)
|And(t1,t2) -> (evalFormula env t1) && (evalFormula env t2)
|Or(t1,t2) -> (evalFormula env t1) || (evalFormula env t2)
|Implies(t1,t2) -> (not (evalFormula env t1)) || (evalFormula env t2)
|Equivalent(t1,t2) -> ((evalFormula env t1) && (evalFormula env t2)) || ((not (evalFormula env t1)) && (not (evalFormula env t2)))

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 3*)

let buildDecTree f = let rec aux f l env= match l with
[]->failwith""
|[s] -> DecRoot(s, DecLeaf(evalFormula ((s,false)::env) f), DecLeaf(evalFormula ((s,true)::env) f))
|t::q -> DecRoot(t, aux f q ((t,false)::env), aux f q ((t,true)::env))
in aux f (getVars f) []

(*------------------------------------------------------------------------------------------------------------------------------*)

type bddNode =
  |BddLeaf of int*bool
  |BddNode of int*string*int*int

type bdd = (int * (bddNode list))

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 4*)

let rec isDecLeafIn dec1 dec2 = match dec1 with
DecRoot(str,g,d) -> (isDecLeafIn g dec2) || (isDecLeafIn d dec2)
|_-> dec1 = dec2

let initBdd dec = if (isDecLeafIn dec (DecLeaf(true))) 
  then if (isDecLeafIn dec (DecLeaf(false))) 
    then [BddLeaf(2,false);BddLeaf(1,true)],[DecLeaf(false); DecLeaf(true)] 
  else [BddLeaf(1,true)],[DecLeaf(true)] 
else if (isDecLeafIn dec (DecLeaf(false))) 
  then [BddLeaf(2,false)],[DecLeaf(true)] 
else [],[]

let rec grabDecTreeInt dec bdd declist = match bdd,declist with
[],[]-> failwith"arbre introuvable"
|BddLeaf(int1,bool1)::q1,DecLeaf(bool2)::q2 -> if dec = DecLeaf(bool1) 
  then int1 
else grabDecTreeInt dec q1 q2
|BddNode(int1,str1,g1,d1)::q1, DecRoot(str2,g2,d2)::q2 -> if dec = DecRoot(str1,g2,d2) 
  then int1 
else grabDecTreeInt dec q1 q2
|_,_ -> failwith"probleme de correspondance des arguments"

let rec buildBddListFromDecTree dec bdd declist = match dec with
DecLeaf(_) -> (bdd,declist)
|DecRoot(str,g,d) -> if (isin declist dec) 
  then (bdd,declist)
  else if (isin declist g)
    then if (isin declist d)
      then ((BddNode(1 + (length bdd), str, grabDecTreeInt g bdd declist, grabDecTreeInt d bdd declist))::bdd, (DecRoot(str,g,d))::declist)
    else let (bddD, declistD) = buildBddListFromDecTree d bdd declist in 
    ((BddNode(1 + (length bdd), str, grabDecTreeInt g bdd declist, grabDecTreeInt d bddD declistD))::bddD, (DecRoot(str,g,d))::declistD)
  else if isin declist d
    then let (bddG, declistG) = buildBddListFromDecTree g bdd declist in 
    ((BddNode(1 + (length bddG ), str, grabDecTreeInt g bddG declistG, grabDecTreeInt d bdd declist))::bddG, (DecRoot(str,g,d))::declistG)
  else let (bddG, declistG) = buildBddListFromDecTree g bdd declist in 
  let (bddD, declistD) = buildBddListFromDecTree d bddG declistG in
  ((BddNode(1 + (length bddD), str, grabDecTreeInt g bddD declistD, grabDecTreeInt d bddD declistD))::bddD, (DecRoot(str,g,d))::declistD)

let buildBdd f = let dec = buildDecTree f in
let bdd,declist = initBdd dec in ( length (fst(buildBddListFromDecTree dec bdd declist)),fst(buildBddListFromDecTree dec bdd declist))

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 5*)

let rec splf bddlist redundant target = match bddlist with
BddLeaf(int1,bool1)::q -> BddLeaf(int1,bool1)::(splf q redundant target)
|BddNode(int2,str1,g,d)::q -> 
if redundant = g 
  then if redundant = d 
    then BddNode(int2, str1, target, target)::(splf q redundant target)
  else BddNode(int2,str1,target,d)::(splf q redundant target)
else if redundant = d 
  then BddNode(int2,str1,g,target)::(splf q redundant target)
else BddNode(int2,str1,g,d)::(splf q redundant target)
|[] -> bddlist

let rec deleteRedundant bddlist1 bddlist2 = let rec isRedundant bddlist target = match bddlist with
BddLeaf(int1,bool1)::q -> isRedundant q target
|BddNode(int1,str1,g,d)::q -> if ((g = target) ||(d = target)) then false else isRedundant q target
|[]-> true
in match bddlist1 with
BddLeaf(int1,bool1)::q-> if (isRedundant bddlist2 int1)
  then deleteRedundant q bddlist2
else BddLeaf(int1,bool1)::(deleteRedundant q bddlist2)
|BddNode(int1,str1,g,d)::q -> if ( (isRedundant bddlist2 int1))
  then deleteRedundant q bddlist2
else BddNode(int1,str1,g,d)::(deleteRedundant q bddlist2)
|[] -> []

let simplifyBdd bdd = let rec aux bddlist1 bddlist2 = match bddlist1 with
BddLeaf(int1,bool1)::q -> aux q bddlist2
|BddNode(int2,str1,g,d)::q -> if (g = d) then aux q (splf bddlist2 int2 g) else aux q bddlist2
|[] -> bddlist2 
in match bdd with
(int0,bddlist) -> match bddlist with
BddNode(int1,str1,g,d)::q -> if g =d 
  then (int0,(deleteRedundant (aux bddlist bddlist) (aux bddlist bddlist)) )
else begin match (aux bddlist bddlist) with
|t::q -> (int0,t::(deleteRedundant (aux bddlist bddlist) (aux bddlist bddlist)) )
|[] -> (0,[]) end
|_->failwith""

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 6*)
(*isTautology sans Bdd*)

let rec addTrueFalse l = match l with
[] -> []
|t::q -> (true::t)::((false::t)::(addTrueFalse q))

let rec boolListsSizeN n = match n with
|0 -> []
|1 -> [[true];[false]]
|_ -> addTrueFalse (boolListsSizeN (n-1))

let boolListsFormula f = let lf = getVars f in let n = length lf in boolListsSizeN n

let boolListToEnv l f = let lf = getVars f in let rec aux l lf = match l with
[]->[]
|t::q -> match lf with
t1::q1 -> (t1,t)::(aux q q1)
|_->failwith"" in aux l lf

let envLists f = let lf = boolListsFormula f in let rec aux lf f = match lf with
[]->[]
|t::q -> (boolListToEnv t f)::(aux q f) in aux lf f

let isTautologyNoBdd f = let el = envLists f in let rec aux el f = match el with
|[] -> true
|t::q -> if evalFormula t f then aux q f else false in aux el f

(*isTautology avec Bdd*)

let isTautology f = let bddf = simplifyBdd (buildBdd f) in
match bddf with
(int,l) -> (length l = 1)

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 7*)

(*AreEquivalent sans Bdd*)

let areEquivalentNoBdd f1 f2 = isTautologyNoBdd(Equivalent(f1,f2))

(*areEquivalent avec Bdd*)

let lengthBdd bdd = match bdd with
(int,l) -> length l

let rec replaceAdd bdd22list before after newG newD = match bdd22list with 
BddNode(int1,str1,g,d)::q -> if int1 = before 
  then (BddNode(after,str1,newG,newD))::(replaceAdd q before after newG newD)
else if g = before 
  then if d = before 
    then replaceAdd q before after newG newD 
  else BddNode(int1,str1,after,d)::(replaceAdd q before after newG newD)
else if d = before 
  then BddNode(int1,str1,g,after)::(replaceAdd q before after newG newD)
else BddNode(int1,str1,g,d)::(replaceAdd q before after newG newD)
|_-> bdd22list

let buildTwoDecTrees f1 f2 = let rec aux f l env = match l with
|[]->DecLeaf(evalFormula env f)
|t::q -> DecRoot(t,aux f q ((t,true)::env),aux f q ((t,false)::env))
in let l = unic ((getVars f1)@(getVars f2)) in (aux f1 l [],aux f2 l [])

let bddList bdd = match bdd with
(int1,l) -> l

let areEquivalent f1 f2 = let rec aux bdd1 bdd2 bdd22 test = match bdd1, bdd2 with
|BddNode(int1,str1,g1,d1)::q1,BddNode(int2,str2,g2,d2)::q2 -> if str1=str2 
  then let newbdd1,newbdd22 = aux q1 q2 (replaceAdd bdd22 (int2) (int1 + test) (g1+test) (d1+test)) test in
  (BddNode(int1+test,str1,g1+test,d1+test)::newbdd1,newbdd22) 
else (bdd1,bdd22)
|_->(bdd1,bdd22) in 
let dec1,dec2 = buildTwoDecTrees f1 f2 in 
let dd1,declist1 = initBdd dec1 in
let dd2,declist2 = initBdd dec2 in
let bdd1,bdd2 = (simplifyBdd (0,fst(buildBddListFromDecTree dec1 dd1 declist1)), simplifyBdd (0,fst(buildBddListFromDecTree dec2 dd2 declist2))) in
let test = lengthBdd bdd2 in 
let (newbdd1,newbdd2) = aux (bddList bdd1) (bddList bdd2) (bddList bdd2) test in newbdd1 = newbdd2

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 8*)

let dotBdd nom_fichier bdd = let fichier = open_out (nom_fichier ^ ".dot") in let rec aux bddlist = match bddlist with
|[]->""
|BddLeaf(int1,bool1)::q ->  ( string_of_int int1) ^
 " [style = bold, label = \"" ^
  (if bool1 then "T" else "F") ^
   "\"];\n"  ^
   aux q
|BddNode(int1,str1,g,d)::q -> (string_of_int int1) ^ 
" [ label=\"" ^ 
str1 (*^" : " ^ (string_of_int int1)*) ^
 "\"];\n" ^
  (string_of_int int1) ^
   " -> " ^ 
   (string_of_int g) ^
    " [color=red,style=dashed];\n" ^ 
    (string_of_int int1) ^ " -> " ^ 
    (string_of_int d) ^ ";\n" ^ 
   aux q
in match bdd with
(int,bddlist) -> output_string fichier ("digraph G {\n" ^(aux bddlist) ^ "}");
close_out fichier

(*------------------------------------------------------------------------------------------------------------------------------*)
(*Question 9*)

type decTreeNbr =
|DecLeafNbr of int*bool
|DecRootNbr of int*string*decTreeNbr*decTreeNbr

let rec nbrTree dec = match dec with
DecLeaf _ -> 1
|DecRoot(_,g,d) -> 1 + (nbrTree g) + (nbrTree d)

let addNbrTree dec = let rec aux dec count = match dec with
DecLeaf(bool1)-> DecLeafNbr(count,bool1)
|DecRoot(str1,g,d) -> DecRootNbr(count,str1,aux g (count+1),aux d (count+1+(nbrTree g)))
in aux dec 1

let dotDec nom_fichier dec  = let fichier = open_out (nom_fichier ^ ".dot") in let rec aux bddlist = match bddlist with
|DecLeafNbr(int1,bool1) -> (string_of_int int1) ^
" [style = bold, label =\"" ^
(if bool1 then "T" else "F") ^
"\"];\n"
|DecRootNbr(int1,str1,DecLeafNbr(intG,bool1),DecLeafNbr(intD,bool2)) -> (string_of_int int1) ^
"[ label =\"" ^str1^"\"];\n" ^
(string_of_int int1) ^
"->" ^
(string_of_int intG) ^
" [color = red, style = dashed];\n" ^
(string_of_int int1) ^
"->" ^
(string_of_int intD) ^
";\n" ^
aux (DecLeafNbr(intG,bool1)) ^
aux (DecLeafNbr(intD,bool2))
|DecRootNbr(int1,str1,DecRootNbr(intG,strG,gG,gD), DecRootNbr(intD,strD,dG,dD)) -> (string_of_int int1) ^
"[ label = \"" ^str1 ^ "\"];\n" ^
(string_of_int int1) ^
"->" ^
(string_of_int intG) ^
" [color = red, style = dashed];\n" ^
(string_of_int int1) ^
"->" ^
(string_of_int intD) ^
";\n" ^
aux (DecRootNbr(intG,strG,gG,gD)) ^
aux (DecRootNbr(intD,strD,dG,dD))
|_->failwith"" in output_string fichier ("Digraph G {" ^ (aux (addNbrTree dec)) ^ "}");
close_out fichier
