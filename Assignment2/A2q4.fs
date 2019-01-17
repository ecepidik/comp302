
(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

let pointsMade (r: Result) = 
    match r with
    |((s1, g1), (s2, g2)) ->
        if (g1 < g2) then 
            ((s1, 0 |> Points), (s2, 3 |> Points))
        elif (g1 > g2) then 
            ((s1, 3 |> Points), (s2, 0 |> Points))
        else
            ((s1, 0 |> Points), (s2, 0 |> Points))


let initEntry (name:Team) = (name, Points 0)
           
let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table =
    let helper(t:Table, team:Team, Points points:Points) =
        match Map.find team t with
        | Points pointsCurrent -> t.Add(team, (points + pointsCurrent) |> Points)
    
    match r with
    |((s1, Goals g1), (s2, Goals g2)) -> 
        helper(helper(t, s1, Points g1), s2, Points g2)

let rec weekendUpdate(t:Table,rl: Result list): Table =
    match rl with
    |[] -> t
    |x::xs -> weekendUpdate(updateTable(t,x), xs)

let rec seasonUpdate(t:Table, sll:Result list list) : Table =
    match sll with
    |[[]] -> t
    |rl::rls -> seasonUpdate(weekendUpdate(t,rl), rls)

let less((s1,n1):Team * Points, (s2,n2):Team * Points) = 
    if (n1 > n2) then 
        false
    else
        true

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)
                                                  