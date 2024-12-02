(* ocamlc -I +str str.cma -o s s.ml && ./s *)

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
let left_list : string List.t =
  let extract_left s = List.hd (Str.split_delim (Str.regexp "   ") s) in
  List.map extract_left lines

let left_numbers = List.fast_sort compare (List.map int_of_string left_list)

let right_list : string List.t =
  let extract_right s =
    List.hd (List.rev (Str.split_delim (Str.regexp "   ") s))
  in
  List.map extract_right lines

let right_numbers =
  List.fast_sort compare (List.map int_of_string right_list)

let fold_f acc a b = acc + abs (a - b)

let total_distance = List.fold_left2 fold_f 0 left_numbers right_numbers

let () = print_int total_distance

let () = print_endline "\n"
(* part-one end *)

(* part-two start *)

module IntHash = struct
  type t = int

  let equal i j = i = j

  let hash i = i land max_int
end

module IntHashtbl = Hashtbl.Make (IntHash)

(* create hash table *)
let counts = IntHashtbl.create (List.length right_numbers)

(* fill above hash table with numbers and counts *)
let () =
  List.iter
    (fun s ->
      let count_c =
        IntHashtbl.find_opt counts s |> Option.value ~default:0
      in
      IntHashtbl.replace counts s (count_c + 1) )
    right_numbers

(* fold function for calculating similarity score *)
let fold_f acc b =
  acc + (b * (IntHashtbl.find_opt counts b |> Option.value ~default:0))

let similarity_score = List.fold_left fold_f 0 left_numbers

let () = print_int similarity_score

let () = print_endline "\n"

(* part-two end *)
