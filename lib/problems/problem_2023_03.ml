open Core

let year = 2023
let day = 3
let neighborhood : (int * int) list = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]
let is_dot (a : char) : bool = Char.( = ) a '.'
let is_number (a : char) : bool = Char.between a ~low:'0' ~high:'9'
let is_symbol (a : char) : bool = a |> is_number |> not && a |> is_number |> not
let is_gear (a : char) : bool = Char.( = ) a '*'

(* Step 2: Group contiguous characters into strings along with their starting xitions *)
let rec generate_number_list acc str start_x x y = function
  | [] ->
    (* Add the last string to the accumulator, if any *)
    if String.length str > 0 then (start_x, y, str) :: acc else acc
  | (nx, ny, c) :: tail ->
    if nx = x + 1 && y = ny
    then
      (* Contiguous character: append it to the current string *)
      generate_number_list acc (str ^ Char.escaped c) start_x nx ny tail
    else (
      (* New string starts: add the current string to the accumulator, if any, and start a new string *)
      let acc = if String.length str > 0 then (start_x, y, str) :: acc else acc in
      generate_number_list acc (Char.escaped c) nx nx ny tail)
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    (* Printf.printf "%s" input; *)
    let lines = String.split_lines input in
    let values =
      List.mapi lines ~f:(fun y line ->
        let chars = String.to_list line in
        List.filter_mapi chars ~f:(fun x c -> if is_dot c then None else Some (x, y, c)))
      |> List.concat
    in
    (* Positions that touches the symbol *)
    let valid_pos =
      List.filter_map values ~f:(fun (x, y, c) ->
        match is_symbol c with
        | true -> Some (x, y)
        | false -> None)
      |> List.map ~f:(fun (x, y) -> List.map neighborhood ~f:(fun (nx, ny) -> nx + x, ny + y))
      |> List.concat
    in
    (* The numbers *)
    let nums =
      List.filter values ~f:(fun (_x, _y, c) -> is_number c)
      |> List.sort ~compare:(fun (x1, y1, _) (x2, y2, _) -> if y1 <> y2 then Int.compare y1 y2 else Int.compare x1 x2)
    in
    let sx, sy, sc = List.hd_exn nums in
    let tail = List.tl_exn nums in
    let numbers_list = generate_number_list [] (Char.escaped sc) sx sx sy tail in
    (* Filter out the invalid number string *)
    let value =
      List.filter_map numbers_list ~f:(fun (x, y, s) ->
        let valid = List.exists valid_pos ~f:(fun (vx, vy) -> x <= vx && vx <= x + (String.length s - 1) && vy = y) in
        match valid with
        | true ->
          (* Printf.printf "Valid pos %d %d, %s\n" x y s; *)
          Some (int_of_string s)
        | false -> None)
      |> List.fold_left ~init:0 ~f:( + )
      |> string_of_int
    in
    Ok value
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let lines = String.split_lines input in
    let values =
      List.mapi lines ~f:(fun y line ->
        let chars = String.to_list line in
        List.filter_mapi chars ~f:(fun x c -> if is_dot c then None else Some (x, y, c)))
      |> List.concat
    in
    (* Positions that touches the symbol *)
    let valid_pos =
      List.filter_map values ~f:(fun (x, y, c) ->
        match is_gear c with
        | true -> Some (x, y)
        | false -> None)
      |> List.map ~f:(fun (x, y) -> List.map neighborhood ~f:(fun (nx, ny) -> nx + x, ny + y))
    in
    (* The numbers *)
    let nums =
      List.filter values ~f:(fun (_x, _y, c) -> is_number c)
      |> List.sort ~compare:(fun (x1, y1, _) (x2, y2, _) -> if y1 <> y2 then Int.compare y1 y2 else Int.compare x1 x2)
    in
    let sx, sy, sc = List.hd_exn nums in
    let tail = List.tl_exn nums in
    let numbers_list = generate_number_list [] (Char.escaped sc) sx sx sy tail in
    let value =
      List.filter_map valid_pos ~f:(fun gears_pos ->
        let gears_num : string list =
          List.filter_map numbers_list ~f:(fun (x, y, s) ->
            match List.exists gears_pos ~f:(fun (vx, vy) -> x <= vx && vx <= x + (String.length s - 1) && vy = y) with
            | true -> Some s
            | false -> None)
        in
        if List.length gears_num = 2
        then Some (List.fold_left gears_num ~init:1 ~f:(fun s1 s2 -> s1 * int_of_string s2))
        else None)
      |> List.fold_left ~init:0 ~f:( + )
      |> string_of_int
    in
    Ok value
  ;;
end
