open System

let rec genList lst ix = 
  match lst with
   | [] -> []
   | h::t ->
     let c = if ix = 1 then h else '.'
	 c :: genList t (ix - 1)

let genLists lst =
  [ for x in 1 .. List.length lst do yield genList lst x ]

let mirr lst =
  match lst with
   | [] -> []
   | h::t -> List.rev t @ [h] @ t

let diamond letters =
  letters |> 
  genLists |> 
  List.map (fun a -> Array.ofList(mirr a)) |> 
  List.rev |> 
  mirr |> 
  List.map (fun x -> new string(x)) |>
  String.concat "\n"

diamond ['A' .. 'F'] |> printfn "%s"