module JaneStreet = struct
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

(* p1 *)
(* type t =
  | Red of int
  | Green of int
  | Blue of int

let populate_game line =
  match String.split_on_char ':' line with
  | head :: rest ->
    (match String.split_on_char ' ' head with
     | _ :: line_num ->
       let chunks = String.split_on_char ';' (List.hd rest) in
       let graps =
         List.map
           (fun chunk ->
             let grap = ref [] in
             let rbg = String.split_on_char ',' chunk in
             List.iter
               (fun rbOrG ->
                 match String.split_on_char ' ' (String.trim rbOrG) with
                 | count :: color ->
                   (match List.hd color with
                    | "red" -> grap := Some (Red (int_of_string count)) :: !grap
                    | "blue" -> grap := Some (Blue (int_of_string count)) :: !grap
                    | "green" -> grap := Some (Green (int_of_string count)) :: !grap
                    | _ -> invalid_arg "not right color")
                 | _ -> invalid_arg "nah")
               rbg;
             !grap)
           chunks
       in
       int_of_string @@ List.hd line_num, graps
     | _ -> invalid_arg "nah")
  | _ -> invalid_arg "nah"
;;

let check params =
  List.fold_left
    (fun acc p ->
      if acc == false
      then acc
      else (
        match p with
        | Some (Red c) -> 12 >= c
        | Some (Green c) -> 13 >= c
        | Some (Blue c) -> 14 >= c
        | _ -> invalid_arg "nah"))
    true
    params
;;

let load_input filepath =
  let ans = ref 0 in
  let count = ref 0 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    count := !count + 1;
    let i, params = populate_game line in
    print_endline @@ string_of_int i;
    List.iter
      (fun grap ->
        List.iter
          (fun t ->
            match t with
            | Some (Red c) -> print_string @@ "RED" ^ string_of_int c ^ " "
            | Some (Green c) -> print_string @@ "GREEN" ^ string_of_int c ^ " "
            | Some (Blue c) -> print_string @@ "BLUE" ^ string_of_int c ^ " "
            | _ -> invalid_arg "nah")
          grap;
        print_string ";")
      params;
    if check @@ List.flatten params then ans := !ans + i;
    print_endline @@ "\n^^^^^^^^^^^^^^line" ^ string_of_int !count ^ "^^^^^^^^^^^");
  print_endline @@ string_of_int !ans
;;

(*  *)
let run () = load_input "./input"

(*  *)
let _ = run () *)

(*  *)
(* p2 *)
type t =
  | Red of int
  | Green of int
  | Blue of int

let populate_game line =
  match String.split_on_char ':' line with
  | head :: rest ->
    (match String.split_on_char ' ' head with
     | _ :: line_num ->
       let chunks = String.split_on_char ';' (List.hd rest) in
       let graps =
         List.map
           (fun chunk ->
             let grap = ref [] in
             let rbg = String.split_on_char ',' chunk in
             List.iter
               (fun rbOrG ->
                 match String.split_on_char ' ' (String.trim rbOrG) with
                 | count :: color ->
                   (match List.hd color with
                    | "red" -> grap := Some (Red (int_of_string count)) :: !grap
                    | "blue" -> grap := Some (Blue (int_of_string count)) :: !grap
                    | "green" -> grap := Some (Green (int_of_string count)) :: !grap
                    | _ -> invalid_arg "not right color")
                 | _ -> invalid_arg "nah")
               rbg;
             !grap)
           chunks
       in
       int_of_string @@ List.hd line_num, graps
     | _ -> invalid_arg "nah")
  | _ -> invalid_arg "nah"
;;

let replace_if_larger batch n =
  let matched = ref false in
  let result =
    List.fold_left
      (fun acc i ->
        match i, n with
        | Red c1, Red c2 ->
          matched := true;
          if c1 >= c2 then i :: acc else n :: acc
        | Green c1, Green c2 ->
          matched := true;
          if c1 >= c2 then i :: acc else n :: acc
        | Blue c1, Blue c2 ->
          matched := true;
          if c1 >= c2 then i :: acc else n :: acc
        | _ -> i :: acc)
      []
      batch
  in
  if List.length result < 3 && !matched = false then n :: result else result
;;

let product_of_all batch =
  List.fold_left
    (fun acc i ->
      match i with
      | Red c1 -> c1 * acc
      | Green c1 -> c1 * acc
      | Blue c1 -> c1 * acc)
    1
    batch
;;

let load_input filepath =
  let ans = ref 0 in
  let count = ref 0 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    count := !count + 1;
    let i, params = populate_game line in
    print_string @@ string_of_int i ^ ": ";
    List.iter
      (fun grap ->
        List.iter
          (fun t ->
            match t with
            | Some (Red c) -> print_string @@ "RED" ^ string_of_int c ^ " "
            | Some (Green c) -> print_string @@ "GREEN" ^ string_of_int c ^ " "
            | Some (Blue c) -> print_string @@ "BLUE" ^ string_of_int c ^ " "
            | _ -> invalid_arg "nah")
          grap;
        print_string ";")
      params;
    let extracted_params =
      List.map (fun opt ->
        match opt with
        | Some (Red c) -> Red c
        | Some (Green c) -> Green c
        | Some (Blue c) -> Blue c
        | _ -> invalid_arg "nah")
      @@ List.flatten params
    in
    let max_of_each =
      List.fold_left (fun acc i -> replace_if_larger acc i) [] extracted_params
    in
    print_string @@ "\nmins of " ^ string_of_int i ^ ": ";
    List.iter
      (fun t ->
        match t with
        | Red c -> print_string @@ "RED" ^ string_of_int c ^ " "
        | Green c -> print_string @@ "GREEN" ^ string_of_int c ^ " "
        | Blue c -> print_string @@ "BLUE" ^ string_of_int c ^ " ")
      max_of_each;
    ans := !ans + product_of_all max_of_each;
    print_endline @@ "\n^^^^^^^^^^^^^^line" ^ string_of_int !count ^ "^^^^^^^^^^^");
  print_endline @@ string_of_int !ans
;;

(*  *)
let run () = load_input "./input"
let _ = run ()
