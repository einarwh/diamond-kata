open System

let rec genList lst ix = 
  match lst with
   | [] -> []
   | h::t ->
     let c = if ix = 0 then h else '.'
     c :: genList t (ix - 1)

let genLists lst =
  [ for x in 0 .. (List.length lst) - 1 do yield genList lst x ]

let mirror lst =
  match lst with
   | [] -> []
   | h::t -> List.rev t @ [h] @ t

let diamond letters =
  letters |> 
  genLists |> 
  List.map (fun a -> Array.ofList(mirror a)) |> 
  List.rev |> 
  mirror |> 
  List.map (fun x -> new string(x)) |>
  String.concat "\n"

diamond ['A' .. 'Z'] |> printfn "%s"