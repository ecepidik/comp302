module Hw3

(* Assignment 3 *) (* Do not edit this line. *)
(* Student name: Ece Pidik, Id Number: 260620324 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops. *)

(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref

let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}

(* This converts an RList to an ordinary list, which is then displayed. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This converts a cell to a list.  You may find it useful for testing.  No need to
use it in your solution. *)

let cellToRList (c:Cell):RList = ref (Some c)

(* This is what you need to code. *)
    
let reverse (lst: RList) = 
    let rec reverseHelper(cEnd, cInit) = 
        match cInit with 
        | None -> lst := cEnd
        | Some c -> reverseHelper(cInit, !c.next ); c.next := cEnd                                
    reverseHelper(None, !lst); lst

(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance | ChangePassword of string | Close

let makeProtectedAccount(openingBalance: int, password: string) = 
    let total = ref openingBalance
    let pass = ref password
    fun(enteredPass : string, trans : transaction) ->
        match trans with
        | Withdraw(amount) ->
            if((!total > amount) && (!pass = enteredPass)) then 
                (total := !total - amount) ; (printf "New balance: %i" !total)
            elif (enteredPass <> !pass) then 
                printfn "Incorrect password"
            else 
                printfn "Insufficient funds"
        | Deposit(amount) ->
            if (!pass = enteredPass) then 
                (total := !total + amount) ; (printfn "New balance: %i" !total)
            else 
                (printf "Incorrect password")
        | CheckBalance ->
            if (!pass = enteredPass) then 
                printfn "Balance: %i" !total
            else 
                printf "Incorrect password"
        | ChangePassword(newPass) ->
            if (!pass = enteredPass) then 
                (pass := newPass) ; (printfn "You changed your password")
            else 
                printf "Incorrect password"
        | Close ->
            if (!pass = enteredPass) then 
                (printfn "Account closed successfully")
            else 
                printf "Incorrect password"

(* Close Account is not implemented correctly *)


(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

let bfIter f ltr = 
    let qt = Queue<ListTree<'a>>()
    qt.Enqueue(ltr)
    while(qt.Count > 0) do
        let (Node(x,list)) = qt.Dequeue()
        f(x)
        List.iter (fun y -> qt.Enqueue(y)) list

(* Some examples I used for testing.  *)
let n5 = Node(5,[])
let n6 = Node(6,[])
let n7 = Node(7,[])
let n8 = Node(8,[])
let n9 = Node(9,[])
let n10 = Node(10,[])
let n12 = Node(12,[])
let n11 = Node(11,[n12])
let n2 = Node(2,[n5;n6;n7;n8])
let n3 = Node(3,[n9;n10])
let n4 = Node(4,[n11])
let n1 = Node(1,[n2;n3;n4])

(* Just for testing, not needed for your solution. *)
let showNode n =
  match n with
    | Node(i,_) -> (printfn "%i" i)

    
bfIter (fun n -> printfn "%i" n) n1
