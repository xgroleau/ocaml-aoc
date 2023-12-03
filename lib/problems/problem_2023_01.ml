open Core;;

let year = 2023
let day = 1


module Part_1 = struct

    let run (input : string) : (string, string) result =
       let lines = String.split_lines  input in


       let total = List.fold lines ~init:0 ~f:(fun acc line ->
           let chars = String.to_list  line in
           let numbers = List.filter chars ~f:Char.is_digit in
           let first = List.hd_exn numbers in
           let last = List.last_exn numbers in
           let number = Fmt.str "%c%c" first last |> Int.of_string in
         acc + number) |> Fmt.str "%d" in
      Ok total
end

module Part_2 = struct
    let run (input : string) : (string, string) result = Ok input
end
