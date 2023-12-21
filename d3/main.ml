module JaneStreet = struct
  include Core
end

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

let if_numeric c =
  try
    int_of_char c |> ignore;
    int_of_char c >= 48 && int_of_char c < 58
  with
  | Failure _ -> false
;;

type affected_cols =
  { high : int
  ; low : int
  }

type relevant =
  | Part_number of string
  | Symbol of string

type t =
  { relevant : relevant
  ; bounds : affected_cols
  }

let parse str : t list * t list =
  let current_col = ref 0 in
  let numerics = ref [] in
  let symbols, part_numbers = ref [], ref [] in
  String.iter
    (fun c ->
      if if_numeric c then numerics := c :: !numerics;
      if c = '.'
      then
        if List.length !numerics > 0
        then (
          let s = String.of_seq @@ List.to_seq @@ List.rev !numerics in
          part_numbers
          := { relevant = Part_number s
             ; bounds =
                 { high =
                     (if !current_col - String.length s > 0
                      then !current_col - String.length s - 1
                      else !current_col - String.length s)
                 ; low = !current_col
                 }
             }
             :: !part_numbers;
          numerics := []);
      if false = if_numeric c && c <> '.'
      then (
        let sym = String.of_seq @@ List.to_seq @@ [ c ] in
        symbols
        := { relevant = Symbol sym; bounds = { high = !current_col; low = !current_col } }
           :: !symbols;
        if List.length !numerics > 0
        then (
          let s = String.of_seq @@ List.to_seq @@ List.rev !numerics in
          part_numbers
          := { relevant = Part_number s
             ; bounds =
                 { high =
                     (if !current_col - String.length s > 0
                      then !current_col - String.length s - 1
                      else !current_col - String.length s)
                 ; low = !current_col
                 }
             }
             :: !part_numbers;
          numerics := []));
      current_col := !current_col + 1)
    str;
  if List.length !numerics > 0
  then (
    let s = String.of_seq @@ List.to_seq @@ List.rev !numerics in
    part_numbers
    := { relevant = Part_number s
       ; bounds =
           { high =
               (if !current_col - String.length s > 0
                then !current_col - String.length s - 1
                else !current_col - String.length s)
           ; low = !current_col
           }
       }
       :: !part_numbers;
    numerics := []);
  !symbols, !part_numbers
;;

let if_touches t1 t2 : bool =
  match t1, t2 with
  | ( { relevant = Part_number _; bounds = { high = h; low = l } }
    , { relevant = Symbol _; bounds = { high = p; low = pp } } ) -> h <= p && pp <= l
  | ( { relevant = Symbol _; bounds = { high = p; low = pp } }
    , { relevant = Part_number _; bounds = { high = h; low = l } } ) -> h <= p && pp <= l
  | _ -> invalid_arg "nah"
;;

let load_input filepath =
  let ans = ref 0 in
  let count = ref 0 in
  let last_line_of_symbols = ref [] in
  let last_line_of_part_numbers = ref [] in
  let last_line_valid_part_numbers = ref [] in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    let current_line_valid_part_numbers = ref [] in
    let last_line_extra_valid_part_numbers = ref [] in
    let current_line_of_symbols, current_line_of_numbers = parse line in
    List.iter
      (fun symbol ->
        List.iter
          (fun part_number ->
            if if_touches symbol part_number && false = List.fold_left (fun contains p -> if contains then contains else p = part_number) false !last_line_valid_part_numbers 
            then last_line_extra_valid_part_numbers := part_number :: !last_line_extra_valid_part_numbers)
          !last_line_of_part_numbers)
      current_line_of_symbols;
    List.iter
      (fun part_number ->
        List.iter
          (fun symbol ->
            if if_touches symbol part_number
            then current_line_valid_part_numbers := part_number :: !current_line_valid_part_numbers)
          !last_line_of_symbols)
      current_line_of_numbers;
    List.iter
      (fun part_number ->
        List.iter
          (fun symbol ->
            if if_touches symbol part_number
            then current_line_valid_part_numbers := part_number :: !current_line_valid_part_numbers)
          current_line_of_symbols)
      current_line_of_numbers;
    print_string @@ "syms: ";
    List.iter
      (fun s ->
        match s with
        | { relevant = Symbol sym; bounds = { high = h; low = l } } ->
          print_string
          @@ sym
          ^ "{high="
          ^ string_of_int h
          ^ ";"
          ^ "low="
          ^ string_of_int l
          ^ "} "
        | _ -> invalid_arg "nah")
      !last_line_of_symbols;
    print_string @@ "\n";
    print_string @@ "part_nums: ";
    List.iter
      (fun pn ->
        match pn with
        | { relevant = Part_number n; bounds = { high = h; low = l } } ->
          print_string
          @@ n
          ^ "{high="
          ^ string_of_int h
          ^ ";"
          ^ "low="
          ^ string_of_int l
          ^ "} "
        | _ -> invalid_arg "nah")
      current_line_of_numbers;
    print_string @@ "\n";
    ans := !ans +
      List.fold_left
         (fun acc t ->
           match t with
           | { relevant = Part_number part_number; _ } ->
             print_endline @@ "tobeinted: " ^ part_number;
             acc + int_of_string part_number
           | _ -> invalid_arg "nah")
           0
           (!last_line_extra_valid_part_numbers @ !current_line_valid_part_numbers)
;
    last_line_valid_part_numbers := !current_line_valid_part_numbers;
    last_line_of_part_numbers := current_line_of_numbers;
    last_line_of_symbols := current_line_of_symbols;
    count := !count + 1;
    print_endline @@ "\n^^^^^^^^^^^^^^line" ^ string_of_int !count ^ "^^^^^^^^^^^");
  print_endline @@ string_of_int !ans
;;

(*  *)
let run () = load_input "./input"
let _ = run ()
