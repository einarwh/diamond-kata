open System

let genLists lst =
  [ for e in lst do yield List.map (fun x -> if x = e then x else '.') lst ]

let mirror lst =
  match lst with
   | [] -> []
   | h::t -> List.rev t @ [h] @ t

let diamond letters =
  letters |> 
  genLists |> 
  List.map (fun a -> new string(Array.ofList(mirror a))) |> 
  List.rev |> 
  mirror |> 
  String.concat "\n"

diamond ['A' .. 'Z'] |> printfn "%s"