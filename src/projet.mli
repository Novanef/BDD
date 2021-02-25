type tformula =
    Value of bool
  | Var of string
  | Not of tformula
  | And of tformula * tformula
  | Or of tformula * tformula
  | Implies of tformula * tformula
  | Equivalent of tformula * tformula
type decTree = DecLeaf of bool | DecRoot of string * decTree * decTree
val p1 : tformula
val p2 : tformula
val q1 : tformula
val q2 : tformula
val f1 : tformula
val f2 : tformula
val ex1 : tformula
val length : 'a list -> int
val isin : 'a list -> 'a -> bool
val unic : 'a list -> 'a list
val getVars : tformula -> string list
type env = (string * bool) list
val findvalue : ('a * 'b) list -> 'a -> 'b
val evalFormula : (string * bool) list -> tformula -> bool
val buildDecTree : tformula -> decTree
type bddNode = BddLeaf of int * bool | BddNode of int * string * int * int
type bdd = int * bddNode list
val isDecLeafIn : decTree -> decTree -> bool
val initBdd : decTree -> bddNode list * decTree list
val grabDecTreeInt : decTree -> bddNode list -> decTree list -> int
val buildBddListFromDecTree :
  decTree -> bddNode list -> decTree list -> bddNode list * decTree list
val buildBdd : tformula -> int * bddNode list
val splf : bddNode list -> int -> int -> bddNode list
val deleteRedundant : bddNode list -> bddNode list -> bddNode list
val simplifyBdd : int * bddNode list -> int * bddNode list
val addTrueFalse : bool list list -> bool list list
val boolListsSizeN : int -> bool list list
val boolListsFormula : tformula -> bool list list
val boolListToEnv : 'a list -> tformula -> (string * 'a) list
val envLists : tformula -> (string * bool) list list
val isTautologyNoBdd : tformula -> bool
val isTautology : tformula -> bool
val areEquivalentNoBdd : tformula -> tformula -> bool
val lengthBdd : 'a * 'b list -> int
val replaceAdd : bddNode list -> int -> int -> int -> int -> bddNode list
val buildTwoDecTrees : tformula -> tformula -> decTree * decTree
val bddList : 'a * 'b -> 'b
val areEquivalent : tformula -> tformula -> bool
val dotBdd : string -> 'a * bddNode list -> unit
type decTreeNbr =
    DecLeafNbr of int * bool
  | DecRootNbr of int * string * decTreeNbr * decTreeNbr
val nbrTree : decTree -> int
val addNbrTree : decTree -> decTreeNbr
val dotDec : string -> decTree -> unit
