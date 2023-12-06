open Core;;

let year = 2023
let day = 2


module Part_1 = struct

    let handle_draw str =
      let game_data = String.split str ~on:' '
        |> List.filter ~f:(fun s -> not (String.is_empty s)) in
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
      List.for_all draw ~f:(fun (value) ->
          handle_draw value
        )
    ;;


    let parse_draws str =
      let draws = String.split str ~on:';' in
      List.for_all draws ~f:(fun (value) ->
          parse_draw value
        )
    ;;

    let get_game_id str =
      let (game, rest) = String.lsplit2_exn str ~on:':' in
      let game_id = String.sub ~pos:5 ~len:(String.length game - 5) game in
      let game_id = int_of_string game_id in
      (game_id, rest)
    ;;



    let run (input : string) : (string, string) result =
       let lines = String.split_lines input in
       let total = List.filter_map lines ~f:(fun (value) ->
           let (id, rest) = get_game_id value in
           match parse_draws rest with
           | true -> Some id
           | false -> None
         ) |> List.fold_left ~init:0 ~f:(+)
           |> string_of_int in

      Ok total
    ;;
end

module Part_2 = struct
    let run (input : string) : (string, string) result =
      Ok input
    ;;
end
