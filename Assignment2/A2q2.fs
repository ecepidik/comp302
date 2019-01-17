type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly =
    let rec helper(c, e, p) =
        match p with
        | [] -> []
        | (xc,xe)::xs -> (xc * c, xe + e)::(helper(c, e, xs))
    helper(c, e, p) |> Poly


let addTermToPoly(Term (c,e):term, Poly p:poly):poly = 
    let rec helper(c, e, p) =
        match p with
        | [] -> [(c, e)]
        | (xc, xe)::xs -> 
            if(e = xe) then (c + xc, xe)::xs
            elif(xe < e) then (c, e)::xs
            else (xc, xe)::(helper(c, e, xs))
    helper(c, e, p) |> Poly

let addPolys(Poly p1:poly, Poly p2:poly):poly = 
    let rec helper(p1, p2) =
        match (p1, p2) with
        | ([], []) -> []
        | ([], p2) -> p2
        | (p1, []) -> p1
        | ((xc1,xe1)::x1s, (xc2,xe2)::x2s) ->
            if (xe1 = xe2) then
                (xc1 + xc2, xe1):: helper(x1s, x2s)
            elif (xe1 > xe2) then
                (xc1,xe1)::helper(x1s, (xc2,xe2)::x2s) 
            else
                (xc2,xe2)::helper((xc1,xe1)::x1s, x2s) 
    helper(p1, p2) |> Poly

let rec multPolys(Poly p1:poly, Poly p2:poly):poly =
    match (p1, p2) with
    | ([],[]) -> [] |> Poly
    | ([], p2) -> [] |> Poly
    | (p1, []) -> [] |> Poly
    | (x1::x1s, x2::x2s) -> addPolys(multiplyPolyByTerm(x1 |> Term, p2 |> Poly), multPolys(x1s |> Poly, p2 |> Poly))

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float = 
    List.fold (fun acc elem -> acc + elem) 0.0 (p |> List.map(fun x -> evalTerm v (x |> Term)))

let diffPoly (Poly p):poly = 
    let rec helper(poly) =
        match poly with
        | [] -> []
        | (xc, xe)::xs ->
            if (xe = 0) then
                []
            else
                (xc * (float)xe, xe - 1)::helper(xs)
    helper(p) |> Poly

(*

let t1 = Term (2.0,2)
let p4 : poly = Poly [(8.0, 7); (3.0, 5); (4.0, 4); (14.0, 3); (5.0, 2); (7.0, 1); (1.5, 0)]
let p5 : poly = Poly [(18.0, 12); (24.0, 9); (84.0, 8); (18.0, 7); (8.0, 6); (56.0, 5); (110.0, 4); (42.0, 3); (4.5, 2)]

let p3 = multPolys(p5,p4)

let printPoly (Poly p) =
    p 
    |> List.map (fun (c, e) -> if e = 0 then sprintf "%.1f" c else sprintf "%.1fx^%d" c e)
    |> String.concat " + "
    |> printfn "%s"

printPoly(p3) 

*)


