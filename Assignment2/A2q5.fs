type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>
let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]


let makeRoadMap data =
    let rec helper (roadData, Roads roadMap: RoadMap) =
        match roadData with 
        | [] -> roadMap |> Roads
        | (destination, connectingCities)::xs ->
            helper(xs, (roadMap.Add(City destination, Set.ofList(List.map City connectingCities)) |> Roads))
    helper(data, (Map.empty<Destination, Set<Destination>>) |> Roads)


