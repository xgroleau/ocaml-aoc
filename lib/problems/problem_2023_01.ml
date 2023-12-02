open Core;;

let year = 2023
let day = 1

module Part_1 = struct

    let run (input : string) : (string, string) result =

       let new_line = String.split_on_char '\n' input in
      (* strin *)
      (* let str = input |> new_line |> String.to_seq in *)
      Ok input
end

module Part_2 = struct
    let run (input : string) : (string, string) result = Ok input
end
