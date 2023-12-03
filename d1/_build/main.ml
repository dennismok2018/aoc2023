(* open Core *)
open Str
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
    in In_channel.with_open_text filepath (fun channel -> exhaust channel)
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

let count_forward s =
  let re = Str.regexp "one|two|three|four|five|six|seven|eight|nine" in
  let count = ref 0 in
  let rec aux i = 
    let ni = Str.search_forward re s i in
    if ni > i then (count := !count +1; aux ni) else () ;
  in aux 0;
  !count
  
let extract_forward s =
  let re = Str.regexp "one|two|three|four|five|six|seven|eight|nine" in
  let took = ref [] in
  let rec aux i = 
    let ni = Str.search_forward re s i in
    if ni > i then (took := !took @ [Str.matched_string s]; aux ni) else () ;
  in aux 0;
  !took

let if_numeric c = 
  try (int_of_char c) |> ignore; (int_of_char c) >= 48 && (int_of_char c < 58)
  with Failure _ -> false

let wrapper_lst_hd lst = 
  try Some(List.hd lst) with Failure _ -> None

type t = Maybe_numeric of char list | Numeric of int

let extract_numerics_from_line line =
 List.filter_map (fun (t) -> match t with
 | Numeric i -> Some i
 | _ -> None
 ) @@ List.rev @@ String.fold_left (fun acc c -> 
    if if_numeric c then (
      
      let acc = (match wrapper_lst_hd acc with
      | Some Maybe_numeric l  ->  (
        let maybe_list_of_int = (
        match count_forward @@ String.of_seq @@ List.to_seq l with
        | 0 -> None
        | _ -> 
          Some (extract_forward @@ String.of_seq @@ List.to_seq l )
        ) in match  maybe_list_of_int with 
        | None -> acc
        | Some lst_of_ints -> (List.tl acc) @ (List.map (fun s -> Numeric (int_of_string s)) lst_of_ints)  
      )
      | _ -> acc )in
      Numeric ((int_of_char c) - 48) ::acc
      
    ) else 
   (match wrapper_lst_hd acc with 
   | Some Numeric _ -> Maybe_numeric ([c]) :: acc
   | Some Maybe_numeric l  -> Maybe_numeric (l @ [c]) :: acc
   | None  -> acc
   ) 
    
    ) [] line
;;

let head_and_tail lst = 
int_of_string ((string_of_int @@ (List.hd lst)) ^ 
  (string_of_int ( List.hd @@ List.rev lst) )) 

let load_input filepath =
  let sum = ref 0 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line -> 
    let lst_of_ints = extract_numerics_from_line line in
    let i = head_and_tail lst_of_ints in
    sum := !sum + i
  ); 
  print_endline @@ string_of_int !sum
   

(*  *)
let run () = 
  load_input "./input"
;;
(*  *)
let _ = run ()
