open Core

let year = 2023
let day = 4

let parse_card str =
  let card =
    String.split str ~on:':'
    |> List.last_exn
    |> String.split ~on:'|'
    |> List.map ~f:(fun s ->
      String.split s ~on:' '
      |> List.filter ~f:(fun value -> String.is_empty value |> not)
      |> List.map ~f:int_of_string)
  in
  match card with
  | [ h; t ] -> h, t
  | _ -> failwith "Invalid length of list"
;;

let calculate_card ((winning, draw) : int list * int list) : int =
  let length =
    List.filter winning ~f:(fun w -> List.mem draw w ~equal:Int.( = )) |> List.length
  in
  length
;;

let power_card (value : int list * int list) : int =
  let length = calculate_card value in
  if length > 0 then Int.pow 2 (length - 1) else 0
;;

let rec calculate_card_accum ~accum lst =
  match lst with
  | [] -> accum
  | (num, value) :: tail ->
    let new_accum = accum + num in
    let modified = List.take tail value |> List.map ~f:(fun (n, v) -> n + num, v) in
    let dropped = List.drop tail value in
    let new_tail = modified @ dropped in
    calculate_card_accum ~accum:new_accum new_tail
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let lines = String.split_lines input in
    let result =
      List.map lines ~f:(fun card -> card |> parse_card |> power_card)
      |> List.fold_left ~init:0 ~f:( + )
      |> string_of_int
    in
    Ok result
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let lines = String.split_lines input in
    let result =
      List.map lines ~f:(fun card ->
        card |> parse_card |> calculate_card |> fun v -> 1, v)
      |> calculate_card_accum ~accum:0
      |> string_of_int
    in
    Ok result
  ;;
end
