type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

(* 
    Looks up the values in the binding
*)  
let rec lookup(name:string, env: Bindings) = 
    match env with
    | [] -> None
    | (xName, xInt)::xs ->
        if (xName = name) then
            Some(xInt)
        else
            lookup(name, xs)

(* 
    Inserts a new binding in the right place
*)  
let rec insert(name:string, value: int, b: Bindings) = 
    match b with
    | [] -> [(name, value)]
    |(xName, xInt)::xs ->
        if (xInt >= value) then
            (name, value)::b
        else
            (xName, xInt)::insert(name, value, xs)

(* 
    Evaluating the expression
*)                                           
let rec eval(exp : Exptree, env:Bindings) = 
    match exp with
    | Const constant -> Some constant
    | Var variable -> lookup(variable, env)
    
    (* 
        Further pattern matching required if Addition
        Addition can only happen if both Exptrees have some values 
         Otherwise None returned 
    *)
    | Add (t1, t2) ->
        match (eval(t1, env), eval(t2, env)) with
        | (Some x, Some y) -> Some (x + y)
        | (None, None) -> None
        | (None, _) -> None
        | (_, None) -> None
    
    (* 
        Further pattern matching required if Multiplication
        Multiplication can only happen if both Exptrees have some values
        Otherwise None returned 
    *)
    | Mul (t1, t2) -> 
        match (eval(t1 ,env), eval(t2, env)) with
        | (Some x, Some y) -> Some (x * y)
        | (None, None) -> None
        | (None, _) -> None
        | (_, None) -> None
