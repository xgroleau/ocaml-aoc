open Core;;

let year = 2023
let day = 2


module Part_1 = struct
  (*let blocks_color = ["blue", "green", "red"];;*)

    (*  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green *)
    let get_game_id str =
      let (game, rest) = String.lsplit2_exn str ~on:':' in
      let game_id = String.sub ~pos:5 ~len:(String.length game - 5) game in
      let game_id = int_of_string game_id in
      Printf.printf "%d rest %s\n" game_id rest;
      (game_id, rest)

    let run (input : string) : (string, string) result =
       let lines = String.split_lines input in
       List.iter lines ~f:(fun (value) ->
           let _ = get_game_id value in
           ()
         );

      Ok input
    ;;
end

module Part_2 = struct
    let run (input : string) : (string, string) result =
      Ok input
    ;;
end
