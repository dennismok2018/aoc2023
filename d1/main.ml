module C = struct
  include Core
end
(* let consume_input_text_line_by_line ~filepath ~consume =
  let rec exhaust channel =
    match In_channel.input_line channel with
    | None -> ()
    | Some line ->
      consume line;
      exhaust channel
  in
  In_channel.with_open_text filepath (fun channel -> exhaust channel)
;;

(* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst *)

(* let left_and_right_consume_input_text_line_by_line
   ~filepath
   ~condition
   ~left
   ~right
   ?first
   ?last
   ()
   =
   let continue = ref true in
   let rec loop ~condition ~left ~right ~first ~last ~channel =
   (match first with
   | None -> ()
   | Some f -> f ());
   (match condition () with
   | true -> continue := left channel
   | false -> right ());
   (match last with
   | None -> ()
   | Some f -> f ());
   if continue.contents then loop ~condition ~left ~right ~first ~last ~channel
   in
   ; ()In_channel.with_open_text filepath (fun channel ->
   loop ~condition ~left ~right ~first ~last ~channel)
   ;; *)

(* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; *)
(* todo *)

let if_numeric c = 
  try (int_of_char c) |> ignore; (int_of_char c) >= 48 && (int_of_char c < 58)
  with Failure _ -> false

let extract_numerics_from_line line =
   List.rev @@ String.fold_left (fun acc c -> if if_numeric c then ((int_of_char c) - 48) ::acc else acc) [] line
;;

let head_and_tail lst = 
int_of_string ((string_of_int @@ (List.hd lst)) ^ 
  (string_of_int ( List.hd @@ List.rev lst) )) 

let load_input filepath =
  let sum = ref 0 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line -> 
    let lst_of_ints = extract_numerics_from_line line in
    print_string @@ ("hd" ^ string_of_int @@ List.hd lst_of_ints) ^ "::";
    print_string @@ ("tail" ^ string_of_int @@ List.hd @@ List.rev lst_of_ints) ^ "\n";
    let i = head_and_tail lst_of_ints in
    sum := !sum + i
  ); 
  print_endline @@ string_of_int !sum
   

(*  *)
let run () = 
  load_input "./input"
;;
(*  *)
let _ = run () *)

(*  *)

(* p2 *)

let consume_input_text_line_by_line ~filepath ~consume =
  let rec exhaust channel =
    match In_channel.input_line channel with
    | None -> ()
    | Some line ->
      consume line;
      exhaust channel
  in
  In_channel.with_open_text filepath (fun channel -> exhaust channel)
;;

(* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst *)

(* let left_and_right_consume_input_text_line_by_line
   ~filepath
   ~condition
   ~left
   ~right
   ?first
   ?last
   ()
   =
   let continue = ref true in
   let rec loop ~condition ~left ~right ~first ~last ~channel =
   (match first with
   | None -> ()
   | Some f -> f ());
   (match condition () with
   | true -> continue := left channel
   | false -> right ());
   (match last with
   | None -> ()
   | Some f -> f ());
   if continue.contents then loop ~condition ~left ~right ~first ~last ~channel
   in
   ; ()In_channel.with_open_text filepath (fun channel ->
   loop ~condition ~left ~right ~first ~last ~channel)
   ;; *)

(* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; *)
(* todo *)

type t =
  | Maybe_numeric of char list
  | Numeric of int

let extract clst =
  let patterns =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in
  let rec aux remaining tmp acc =
    match remaining with
    | hd :: tl ->
      let ntmp = tmp @ [ hd ] in
      let nstr = String.of_seq @@ List.to_seq @@ ntmp in
      let found =
        List.fold_left
          (fun acc p ->
            if C.String.is_substring ~substring:p nstr
            then (
              let i =
                match p with
                | "one" -> 1
                | "two" -> 2
                | "three" -> 3
                | "four" -> 4
                | "five" -> 5
                | "six" -> 6
                | "seven" -> 7
                | "eight" -> 8
                | "nine" -> 9
                | _ -> invalid_arg "noway "
              in
              i :: acc)
            else acc)
          []
          patterns
      in
      if List.length found = 0 then aux tl ntmp acc else aux tl [ hd ] found @ acc
    | [] -> List.rev acc
  in
  aux clst [] []
;;

let if_numeric c =
  try
    int_of_char c |> ignore;
    int_of_char c >= 48 && int_of_char c < 58
  with
  | Failure _ -> false
;;

let wrapper_lst_hd lst =
  try
    List.hd lst |> ignore;
    Some (List.hd lst)
  with
  | Failure _ -> None
;;

let extract_numerics_from_line line =
  print_endline @@ "starts with:" ^ line;
  let tlst =
    String.fold_left
      (fun acc c ->
        if if_numeric c
        then Numeric (int_of_char c - 48) :: acc
        else (
          match wrapper_lst_hd acc with
          | Some (Numeric _) -> Maybe_numeric [ c ] :: acc
          | Some (Maybe_numeric l) -> Maybe_numeric (l @ [ c ]) :: List.tl acc
          | None -> Maybe_numeric [ c ] :: acc))
      []
      line
  in
  List.fold_left
    (fun acc t ->
      match t with
      | Numeric i -> i :: acc
      | Maybe_numeric l -> List.rev (extract l) @ acc)
    []
  @@ tlst
;;

let load_input filepath =
  let sum = ref 0 in
  let count = ref 0 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    let lst_of_ints = extract_numerics_from_line line in
    let rev_lst_of_ints = List.rev lst_of_ints in
    print_string "Resulted: ";
    List.iter
      (fun i ->
        print_int i;
        print_string ",")
      lst_of_ints;
    print_endline "";
    count := !count + 1;
    let i = (List.hd lst_of_ints * 10) + List.hd rev_lst_of_ints in
    print_endline @@ "10= " ^ string_of_int @@ (List.hd lst_of_ints * 10);
    print_endline @@ "1= " ^ string_of_int @@ List.hd rev_lst_of_ints;
    print_endline @@ "i= " ^ string_of_int i;
    print_endline @@ "^^^^^^^^^^^^^^line" ^ string_of_int !count ^ "^^^^^^^^^^^";
    sum := !sum + i);
  print_endline @@ string_of_int !sum
;;

(*  *)
let run () = load_input "./input"

(*  *)
let _ = run ()
