module hw4
(* Ece Pidik 260620324 *)

type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool = 
    match t with
    | Var id -> 
        if (id = x) then true
        else false
    | Const int -> false
    | Term (idTerm, termList) ->
        List.exists (occurs x) termList

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : term) (x : id) (t : term) : term =
    match t with
    | Const int -> t
    | Var id -> 
        if (id = x) then s
        else t
    | Term (idTerm, termList) ->
        Term (idTerm, List.map(subst s x) termList)

let apply (s : substitution) (t : term) : term =
    List.fold(fun xs x -> subst (snd(x)) (fst(x)) xs) t s

let rec unify (s : term) (t : term) : substitution =
    match s with
    | Const ints ->
        match t with
        | Const intt -> 
            if(ints = intt) then []
            else failwith "not unifiable"
        | Var idt -> [(idt, s)]
        | Term (idt, termList) -> failwith "not unifiable: term constant clash"
    | Var ids ->
        match t with
        | Const intt -> [(ids, t)]
        | Var idt -> 
            if(ids = idt) then []
            else [(ids, t)]
        | Term (_) as t ->
            if(occurs ids t) then failwith "not unifiable: circularity"
            else [(ids, t)]
    | Term (ids, sTermList) as s -> 
        match t with
        | Const intt -> failwith "not unifiable: term constant clash"
        | Var idt ->  
            if(occurs idt s) then failwith "not unifiable: circularity"
            else [(idt, s)]
        | Term (idt, tTermList) ->
            if (idt = ids && List.length tTermList = List.length sTermList) then 
                failwith "I couldn't implement it"
            else failwith "not unifiable: head symbol conflict"

(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution = 
    match s with
    | [] -> []
    | (terms, termt) :: termList ->
        let s2 = unify_list termList in
        let s1 = unify (apply s2 terms) (apply s2 termt) in
        s1 @ s2
