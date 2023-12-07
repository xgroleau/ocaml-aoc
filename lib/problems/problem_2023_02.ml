open Core

let year = 2023
let day = 2

module Part_1 = struct
  let handle_draw str =
    let game_data = String.split str ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) in
    let num = List.hd_exn game_data |> int_of_string in
    let color = List.last_exn game_data in
    match color with
    | "red" -> num <= 12
    | "green" -> num <= 13
    | "blue" -> num <= 14
    | _ -> false
  ;;

  let parse_draw str =
    let draw = String.split str ~on:',' in
    List.for_all draw ~f:(fun value -> handle_draw value)
  ;;

  let parse_draws str =
    let draws = String.split str ~on:';' in
    List.for_all draws ~f:(fun value -> parse_draw value)
  ;;

  let get_game_id str =
    let game, rest = String.lsplit2_exn str ~on:':' in
    let game_id = String.sub ~pos:5 ~len:(String.length game - 5) game in
    let game_id = int_of_string game_id in
    game_id, rest
  ;;

  let run (input : string) : (string, string) result =
    let lines = String.split_lines input in
    let total =
      List.filter_map lines ~f:(fun value ->
        let id, rest = get_game_id value in
        match parse_draws rest with
        | true -> Some id
        | false -> None)
      |> List.fold_left ~init:0 ~f:( + )
      |> string_of_int
    in
    Ok total
  ;;
end

module Part_2 = struct
  exception Error of string

  let sort_colors_exn (red, green, blue) str num =
    match str with
    | "red" -> num :: red, green, blue
    | "green" -> red, num :: green, blue
    | "blue" -> red, green, num :: blue
    | _ -> raise (Error "Failed, invalid color type")
  ;;

  let handle_draw num_list str =
    let game_data = String.split str ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) in
    let num = List.hd_exn game_data |> int_of_string in
    let color = List.last_exn game_data in
    sort_colors_exn num_list color num
  ;;

  let parse_draw num_list str =
    let draw = String.split str ~on:',' in
    List.fold draw ~init:num_list ~f:(fun accum value -> handle_draw accum value)
  ;;

  let parse_draws str =
    let draws = String.split str ~on:';' in
    let red, green, blue = List.fold draws ~init:([], [], []) ~f:(fun accum value -> parse_draw accum value) in
    let max_in_list lst =
      match List.max_elt lst ~compare:Int.compare with
      | Some value -> value
      | None -> raise (Error "Empty list")
    in
    let red_max = max_in_list red in
    let green_max = max_in_list green in
    let blue_max = max_in_list blue in
    red_max * green_max * blue_max
  ;;

  let run (input : string) : (string, string) result =
    let lines = String.split_lines input in
    let total =
      List.fold ~init:0 lines ~f:(fun accum value ->
        let _, draws = String.lsplit2_exn value ~on:':' in
        let power = parse_draws draws in
        accum + power)
      |> string_of_int
    in
    Ok total
  ;;
end
