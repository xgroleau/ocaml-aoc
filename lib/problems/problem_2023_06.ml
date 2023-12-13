open Core

let year = 2023
let day = 6

let parse_data (str : string) : string list =
  String.split str ~on:':'
  |> List.last_exn
  |> String.split ~on:' '
  |> List.filter ~f:(fun s -> not (String.is_empty s))
;;

let solve_race (max_time, distance) : int =
  let wtf = Sequence.range 0 max_time ~start:`inclusive ~stop:`inclusive in
  wtf
  |> Sequence.filter ~f:(fun hold_t -> (max_time - hold_t) * hold_t > distance)
  |> Sequence.length
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let data = String.split_lines input in
    let times =
      List.hd_exn data |> parse_data |> List.map ~f:(fun s -> int_of_string s)
    in
    let distances =
      List.last_exn data |> parse_data |> List.map ~f:(fun s -> int_of_string s)
    in
    let res =
      List.zip_exn times distances
      |> List.map ~f:solve_race
      |> List.fold_left ~init:1 ~f:(fun v x -> v * x)
      |> string_of_int
    in
    Ok res
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let data = String.split_lines input in
    let time = List.hd_exn data |> parse_data |> String.concat |> int_of_string in
    let distance = List.last_exn data |> parse_data |> String.concat |> int_of_string in
    let res = solve_race (time, distance) |> string_of_int in
    Ok res
  ;;
end
