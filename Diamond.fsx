open System

let genLists lst =
  [ for e in lst do yield List.map (fun x -> if x = e then x else '.') lst ]

let mirror lst =
  match lst with
   | [] -> []
   | h::t -> List.rev t @ lst

let diamond letters =
  letters |> genLists 
          |> List.rev 
          |> List.map (fun a -> mirror a) 
          |> mirror 

let toStr d =
  d |> List.map (fun a -> new string(Array.ofList(a))) 
    |> String.concat "\n"

['A' .. 'Z'] |> diamond |> toStr |> printfn "%s"
