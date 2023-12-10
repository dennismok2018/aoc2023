module JaneStreet = {
  include Core;
};
/* let consume_input_text_line_by_line ~filepath ~consume =
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
   let _ = run () */

/*  */

/* p2 */

let consume_input_text_line_by_line = (~filepath, ~consume) => {
  let rec exhaust = channel =>
    switch (In_channel.input_line(channel)) {
    | None => ()
    | Some(line) =>
      consume(line);
      exhaust(channel);
    };

  In_channel.with_open_text(filepath, channel => exhaust(channel));
};

/* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst */

/* let left_and_right_consume_input_text_line_by_line
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
   ;; */

/* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; */
/* todo */

type t =
  | Maybe_numeric(list(char))
  | Numeric(int);

let patterns = [
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
];

let extract = clst => {
  let rec aux = (remaining, tmp, acc) =>
    switch (remaining) {
    | [hd, ...tl] =>
      let ntmp = tmp @ [hd];
      let nstr = String.of_seq @@ List.to_seq @@ ntmp;
      let found =
        List.fold_left(
          (acc, p) =>
            if (JaneStreet.String.is_substring(~substring=p, nstr)) {
              let i =
                switch (p) {
                | "one" => 1
                | "two" => 2
                | "three" => 3
                | "four" => 4
                | "five" => 5
                | "six" => 6
                | "seven" => 7
                | "eight" => 8
                | "nine" => 9
                | _ => invalid_arg("noway ")
                };

              [i, ...acc];
            } else {
              acc;
            },
          [],
          patterns,
        );

      if (List.length(found) == 0) {
        aux(tl, ntmp, acc);
      } else {
        aux(tl, [hd], found) @ acc;
      };
    | [] => acc
    };

  aux(clst, [], []);
};

let if_numeric = c =>
  try(
    {
      int_of_char(c) |> ignore;
      int_of_char(c) >= 48 && int_of_char(c) < 58;
    }
  ) {
  | Failure(_) => false
  };

let wrapped_lst_hd = lst =>
  try(
    {
      List.hd(lst) |> ignore;
      Some(List.hd(lst));
    }
  ) {
  | Failure(_) => None
  };

let extract_numerics_from_line = line => {
  print_endline @@ "starts with:" ++ line;
  let tlst =
    String.fold_left(
      (acc, c) =>
        if (if_numeric(c)) {
          [Numeric(int_of_char(c) - 48), ...acc];
        } else {
          switch (wrapped_lst_hd(acc)) {
          | Some(Numeric(_)) => [Maybe_numeric([c]), ...acc]
          | Some(Maybe_numeric(l)) => [
              Maybe_numeric(l @ [c]),
              ...List.tl(acc),
            ]
          | None => [Maybe_numeric([c]), ...acc]
          };
        },
      [],
      line,
    );

  List.fold_left(
    (acc, t) =>
      switch (t) {
      | Numeric(i) => [i, ...acc]
      | Maybe_numeric(l) => extract(l) @ acc
      },
    [],
  ) @@
  tlst;
};

let load_input = filepath => {
  let sum = ref(0);
  let count = ref(0);
  consume_input_text_line_by_line(
    ~filepath,
    ~consume=line => {
      let lst_of_ints = extract_numerics_from_line(line);
      let rev_lst_of_ints = List.rev(lst_of_ints);
      print_string("Resulted: ");
      List.iter(
        i => {
          print_int(i);
          print_string(",");
        },
        lst_of_ints,
      );
      print_endline("");
      count := count^ + 1;
      let i = List.hd(lst_of_ints) * 10 + List.hd(rev_lst_of_ints);
      print_endline @@ "10= " ++ string_of_int @@ List.hd(lst_of_ints) * 10;
      print_endline @@ "1= " ++ string_of_int @@ List.hd(rev_lst_of_ints);
      print_endline @@ "i= " ++ string_of_int(i);
      print_endline @@
      "^^^^^^^^^^^^^^line"
      ++ string_of_int(count^)
      ++ "^^^^^^^^^^^";
      sum := sum^ + i;
    },
  );
  print_endline @@ string_of_int(sum^);
};

/*  */
let run = () => load_input("./input");

/*  */
let _ = run();
