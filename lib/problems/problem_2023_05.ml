open Core

let year = 2023
let day = 5

let parse_line_map line =
  let line = String.split line ~on:' ' |> List.map ~f:int_of_string in
  match line with
  | [ dest; src; len ] -> dest, src, len
  | _ -> failwith "Invalid map format"
;;

let parse_map str : (int * int * int) list =
  String.split_lines str |> List.tl_exn |> List.map ~f:parse_line_map
;;

let solve_map seed map =
  (* Printf.printf "Map %d\n" (List.length map); *)
  List.find_map map ~f:(fun (dest, src, len) ->
    if seed >= src && seed < src + len then Some (seed - src + dest) else None)
  |> Option.value ~default:seed
;;

let map_values seed maps = List.fold maps ~init:seed ~f:solve_map

let parse_seeds sections =
  List.hd_exn sections
  |> String.split ~on:':'
  |> List.last_exn
  |> String.split ~on:' '
  |> List.filter ~f:(fun value -> String.is_empty value |> not)
  |> List.map ~f:int_of_string
;;

let rec split_into_pairs = function
  | a :: b :: tl -> (a, b) :: split_into_pairs tl
  | [ _ ] ->
    failwith "Invalid map format" (* Handle the case of a single remaining element *)
  | [] -> []
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let sections = Str.split (Str.regexp "\n\n+") input in
    let seeds = parse_seeds sections in
    let maps = List.tl_exn sections |> List.map ~f:parse_map in
    let result =
      List.map seeds ~f:(fun seed -> map_values seed maps)
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn
      |> string_of_int
    in
    Ok result
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let sections = Str.split (Str.regexp "\n\n+") input in
    let seeds =
      parse_seeds sections
      |> split_into_pairs
      |> List.map ~f:(fun (init, len) -> List.init len ~f:(fun idx -> idx + init))
      |> List.concat
    in
    let maps = List.tl_exn sections |> List.map ~f:parse_map in
    let result =
      List.map seeds ~f:(fun seed -> map_values seed maps)
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn
      |> string_of_int
    in
    Ok result
  ;;
end
