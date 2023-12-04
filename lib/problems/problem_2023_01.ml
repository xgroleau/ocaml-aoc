open Core;;

let year = 2023
let day = 1


module Part_1 = struct

    let run (input : string) : (string, string) result =
       let lines = String.split_lines input in
       let total = List.fold lines ~init:0 ~f:(fun acc line ->
           let chars = String.to_list  line in
           let numbers = List.filter chars ~f:Char.is_digit in
           let first = List.hd_exn numbers in
           let last = List.last_exn numbers in
           let number = Fmt.str "%c%c" first last |> Int.of_string in
         acc + number) |> Fmt.str "%d" in
      Ok total
    ;;
end

module Part_2 = struct
    let numbers_cases =
        [ "one", '1'
        ; "two", '2'
        ; "three", '3'
        ; "four", '4'
        ; "five", '5'
        ; "six", '6'
        ; "seven", '7'
        ; "eight", '8'
        ; "nine", '9'
        ; "1", '1'
        ; "2", '2'
        ; "3", '3'
        ; "4", '4'
        ; "5", '5'
        ; "6", '6'
        ; "7", '7'
        ; "8", '8'
        ; "9", '9'
        ]
    ;;

    (* Find if a substring is in string, starting from a given position*)
    let to_digit str pos =
      List.find_map numbers_cases ~f:(fun (num_str, num_val) ->
        match String.substr_index ~pos str ~pattern:num_str with
        | Some v when v  = pos -> Some num_val
        | _ -> None)
    ;;


    (* Iterate the whole string  to find a match *)
    let to_number str =
      let range = List.range 0 (String.length str) in
      List.filter_map range ~f:(fun (pos) -> to_digit str pos)

    let run (input : string) : (string, string) result =
       let lines = String.split_lines  input in
       let total = List.fold lines ~init:0 ~f:(fun acc line ->
           let numbers = to_number line in
           let first = List.hd_exn numbers in
           let last = List.last_exn numbers in
           let number = Fmt.str "%c%c" first last |> Int.of_string in
         acc + number) |> Fmt.str "%d" in
      Ok total
    ;;
end
