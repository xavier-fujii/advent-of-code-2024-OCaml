(* ocamlc -o s s.ml && ./s *)

let read_file_to_list filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file -> close_in ic ; List.rev acc
  in
  loop []

let lines = read_file_to_list "input.txt"

(* part-one start *)

let is_all_decreasing numbers =
  snd
  @@ List.fold_left
       (fun acc cur_number ->
         ( cur_number
         , snd acc && cur_number < fst acc && abs (cur_number - fst acc) <= 3
         ) )
       (List.hd numbers + 1, true)
       numbers

let is_all_increasing_or_all_decreasing numbers =
  is_all_decreasing numbers || (is_all_decreasing @@ List.rev numbers)

let is_report_safe s =
  s |> String.split_on_char ' ' |> List.map int_of_string
  |> is_all_increasing_or_all_decreasing

let safe_count =
  List.map is_report_safe lines
  |> List.map (fun safe -> if safe then 1 else 0)
  |> List.fold_left ( + ) 0

let () = print_int safe_count

let () = print_endline "\n"

(* part-one end *)

(* part-two start *)

(* let rec remove_at idx lst = match lst with | [] -> [] | _ :: tl when idx =
   0 -> tl | hd :: tl -> hd :: remove_at (idx - 1) tl

   let range i = List.init i succ

   let is_all_decreasing_mutant numbers = List.map2 (fun idx lst -> remove_at
   idx lst) (range @@ List.length numbers)

   let is_all_increasing_or_all_decreasing_mutant numbers =
   is_all_decreasing_mutant numbers || (is_all_decreasing_mutant @@ List.rev
   numbers) *)

(* part-two end *)
