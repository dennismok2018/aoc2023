module JaneStreet = {
  include Core;
};

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

/* p1 */

let if_numeric = c =>
  try(
    {
      int_of_char(c) |> ignore;
      int_of_char(c) >= 48 && int_of_char(c) < 58;
    }
  ) {
  | Failure(_) => false
  };

type affected_cols = {
  high: int,
  low: int,
};

type relevant =
  | Part_number(string)
  | Symbol(string);

type t = {
  id: string,
  relevant,
  bounds: affected_cols,
};

let parse = (str, line_num): (list(t), list(t)) => {
  let current_col = ref(0);
  let numerics = ref([]);
  let (symbols, part_numbers) = (ref([]), ref([]));
  String.iter(
    c => {
      if (if_numeric(c)) {
        numerics := [c, ...numerics^];
      };
      if (c == '.') {
        if (List.length(numerics^) > 0) {
          let s = String.of_seq @@ List.to_seq @@ List.rev(numerics^);
          part_numbers :=
            [
              {
                id: line_num ++ string_of_int(current_col^),
                relevant: Part_number(s),
                bounds: {
                  high:
                    if (current_col^ - String.length(s) > 0) {
                      current_col^ - String.length(s) - 1;
                    } else {
                      current_col^ - String.length(s);
                    },
                  low: current_col^,
                },
              },
              ...part_numbers^,
            ];
          numerics := [];
        };
      };
      if (false == if_numeric(c) && c != '.') {
        let sym = String.of_seq @@ List.to_seq @@ [c];
        symbols :=
          [
            {
              id: line_num ++ string_of_int(current_col^),
              relevant: Symbol(sym),
              bounds: {
                high: current_col^,
                low: current_col^,
              },
            },
            ...symbols^,
          ];
        if (List.length(numerics^) > 0) {
          let s = String.of_seq @@ List.to_seq @@ List.rev(numerics^);
          part_numbers :=
            [
              {
                id: line_num ++ string_of_int(current_col^),
                relevant: Part_number(s),
                bounds: {
                  high:
                    if (current_col^ - String.length(s) > 0) {
                      current_col^ - String.length(s) - 1;
                    } else {
                      current_col^ - String.length(s);
                    },
                  low: current_col^,
                },
              },
              ...part_numbers^,
            ];
          numerics := [];
        };
      };
      current_col := current_col^ + 1;
    },
    str,
  );
  if (List.length(numerics^) > 0) {
    let s = String.of_seq @@ List.to_seq @@ List.rev(numerics^);
    part_numbers :=
      [
        {
          id: line_num ++ string_of_int(current_col^),
          relevant: Part_number(s),
          bounds: {
            high:
              if (current_col^ - String.length(s) > 0) {
                current_col^ - String.length(s) - 1;
              } else {
                current_col^ - String.length(s);
              },
            low: current_col^,
          },
        },
        ...part_numbers^,
      ];
    numerics := [];
  };
  (symbols^, part_numbers^);
};

let if_touches = (t1, t2): bool =>
  switch (t1, t2) {
  | (
      {id: _, relevant: Part_number(_), bounds: {high: h, low: l}},
      {id: _, relevant: Symbol(_), bounds: {high: p, low: pp}},
    ) =>
    h <= p && pp <= l
  | (
      {id: _, relevant: Symbol(_), bounds: {high: p, low: pp}},
      {id: _, relevant: Part_number(_), bounds: {high: h, low: l}},
    ) =>
    h <= p && pp <= l
  | _ => invalid_arg("nah")
  };

/* let load_input filepath =
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
               if if_touches symbol part_number
                  && false
                     = List.fold_left
                         (fun contains p -> if contains then contains else p = part_number)
                         false
                         !last_line_valid_part_numbers
               then
                 last_line_extra_valid_part_numbers
                 := part_number :: !last_line_extra_valid_part_numbers)
             !last_line_of_part_numbers)
         current_line_of_symbols;
       List.iter
         (fun part_number ->
           List.iter
             (fun symbol ->
               if if_touches symbol part_number
               then
                 current_line_valid_part_numbers
                 := part_number :: !current_line_valid_part_numbers)
             !last_line_of_symbols)
         current_line_of_numbers;
       List.iter
         (fun part_number ->
           List.iter
             (fun symbol ->
               if if_touches symbol part_number
               then
                 current_line_valid_part_numbers
                 := part_number :: !current_line_valid_part_numbers)
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
       ans
       := !ans
          + List.fold_left
              (fun acc t ->
                match t with
                | { relevant = Part_number part_number; _ } ->
                  print_endline @@ "tobeinted: " ^ part_number;
                  acc + int_of_string part_number
                | _ -> invalid_arg "nah")
              0
              (!last_line_extra_valid_part_numbers @ !current_line_valid_part_numbers);
       last_line_valid_part_numbers := !current_line_valid_part_numbers;
       last_line_of_part_numbers := current_line_of_numbers;
       last_line_of_symbols := current_line_of_symbols;
       count := !count + 1;
       print_endline @@ "\n^^^^^^^^^^^^^^line" ^ string_of_int !count ^ "^^^^^^^^^^^");
     print_endline @@ string_of_int !ans
   ;; */

/*  */
/* let run () = load_input "./input"
   let _ = run () */

/* p2 */
type grea_combination = {
  left: option(t),
  middle: t,
  right: option(t),
};

let load_input = filepath => {
  let ans = ref(0);
  let count = ref(0);
  let last_line_looking_left_and_right = ref([]);
  let last_line_looking_rights = ref([]);
  let last_line_of_part_numbers = ref([]);
  consume_input_text_line_by_line(
    ~filepath,
    ~consume=line => {
      let (current_line_of_symbols, current_line_of_numbers) =
        parse(line, string_of_int(count^));

      let middles =
        List.filter_map(
          s =>
            switch (s.relevant) {
            | Symbol("*") => Some({left: None, middle: s, right: None})
            | _ => None
            },
          current_line_of_symbols,
        );

      let left_matched_with_current_stars = ref([]);
      List.iter(
        part_number =>
          List.iter(
            middle =>
              if (if_touches(middle.middle, part_number)) {
                left_matched_with_current_stars :=
                  [
                    {
                      left: Some(part_number),
                      middle: middle.middle,
                      right: None,
                    },
                    ...left_matched_with_current_stars^,
                  ];
              },
            middles,
          ),
        current_line_of_numbers,
      );
      List.iter(
        part_number =>
          List.iter(
            middle =>
              if (if_touches(middle.middle, part_number)) {
                left_matched_with_current_stars :=
                  [
                    {
                      left: Some(part_number),
                      middle: middle.middle,
                      right: None,
                    },
                    ...left_matched_with_current_stars^,
                  ];
              },
            middles,
          ),
        last_line_of_part_numbers^,
      );
      let all_matched_with_current_stars = ref([]);
      List.iter(
        matched =>
          switch (matched.left) {
          | Some(part_number) =>
            List.iter(
              middle =>
                switch (middle.left, middle.middle) {
                | (Some(l), m) =>
                  if (l != part_number && if_touches(m, part_number)) {
                    all_matched_with_current_stars :=
                      [
                        {left: Some(l), middle: m, right: Some(part_number)},
                        ...all_matched_with_current_stars^,
                      ];
                  }
                | _ => invalid_arg("_")
                },
              left_matched_with_current_stars^,
            )
          | _ => invalid_arg("_")
          },
        left_matched_with_current_stars^,
      );
      let left_matched_with_last_stars = ref([]);
      List.iter(
        part_number =>
          List.iter(
            middle =>
              if (if_touches(middle.middle, part_number)) {
                left_matched_with_last_stars :=
                  [
                    {
                      left: Some(part_number),
                      middle: middle.middle,
                      right: None,
                    },
                    ...left_matched_with_last_stars^,
                  ];
              },
            last_line_looking_left_and_right^,
          ),
        current_line_of_numbers,
      );
      let all_matched_with_last_stars = ref([]);
      List.iter(
        matched =>
          switch (matched.left) {
          | Some(part_number) =>
            List.iter(
              middle =>
                switch (middle.left, middle.middle) {
                | (Some(l), m) =>
                  if (l != part_number && if_touches(m, part_number)) {
                    all_matched_with_last_stars :=
                      [
                        {left: Some(l), middle: m, right: Some(part_number)},
                        ...all_matched_with_last_stars^,
                      ];
                  }
                | _ => invalid_arg("_")
                },
              left_matched_with_last_stars^,
            )
          | _ => invalid_arg("_")
          },
        left_matched_with_last_stars^,
      );
      List.iter(
        part_number =>
          List.iter(
            middle =>
              switch (middle.left, middle.middle) {
              | (Some(l), m) =>
                if (l != part_number && if_touches(m, part_number)) {
                  all_matched_with_last_stars :=
                    [
                      {left: Some(l), middle: m, right: Some(part_number)},
                      ...all_matched_with_last_stars^,
                    ];
                }
              | _ => invalid_arg("_")
              },
            last_line_looking_rights^,
          ),
        current_line_of_numbers,
      );
      print_string @@ "last_line_looking_left_and_right: ";
      List.iter(
        s =>
          switch (s) {
          | {
              left: None,
              middle: {id, relevant: Symbol("*"), bounds: {high: h, low: l}},
              right: None,
            } =>
            print_string @@
            "[id="
            ++ id
            ++ ", "
            ++ "{high="
            ++ string_of_int(h)
            ++ ";"
            ++ "low="
            ++ string_of_int(l)
            ++ "}] "
          | _ => invalid_arg("nah")
          },
        last_line_looking_left_and_right^,
      );
      print_string @@ "\n";
      print_string @@ "last_line_looking_rights: ";
      List.iter(
        s =>
          switch (s) {
          | {
              left:
                Some({
                  id: idl,
                  relevant: Part_number(p),
                  bounds: {high: hl, low: ll},
                }),
              middle: {id, relevant: Symbol("*"), bounds: {high: h, low: l}},
              right: None,
            } =>
            print_string @@
            "["
            ++ p
            ++ "[idl="
            ++ idl
            ++ ", "
            ++ "{high="
            ++ string_of_int(hl)
            ++ ";"
            ++ "low="
            ++ string_of_int(ll)
            ++ "}], "
            ++ "[id="
            ++ id
            ++ ", "
            ++ "{high="
            ++ string_of_int(h)
            ++ ";"
            ++ "low="
            ++ string_of_int(l)
            ++ "}]"
            ++ "]"
          | _ => invalid_arg("nah")
          },
        last_line_looking_rights^,
      );
      print_string @@ "\n";
      print_string @@ "part_nums: ";
      List.iter(
        pn =>
          switch (pn) {
          | {id, relevant: Part_number(n), bounds: {high: h, low: l}} =>
            print_string @@
            n
            ++ "[id="
            ++ id
            ++ ", "
            ++ "{high="
            ++ string_of_int(h)
            ++ ";"
            ++ "low="
            ++ string_of_int(l)
            ++ "}] "
          | _ => invalid_arg("nah")
          },
        current_line_of_numbers,
      );
      print_string @@ "\n";
      print_string @@ "middles: ";
      List.iter(
        middle =>
          switch (middle) {
          | {
              left: None,
              middle: {
                id,
                relevant: Symbol(_star),
                bounds: {high: h, low: l},
              },
              right: None,
            } =>
            print_string @@
            "[id="
            ++ id
            ++ ", "
            ++ "{high="
            ++ string_of_int(h)
            ++ ";"
            ++ "low="
            ++ string_of_int(l)
            ++ "}] "
          | _ => invalid_arg("nah")
          },
        middles,
      );
      print_string @@ "\n";
      last_line_looking_left_and_right := middles;
      last_line_looking_rights := left_matched_with_current_stars^;
      last_line_of_part_numbers := current_line_of_numbers;
      ans :=
        ans^
        + List.fold_left(
            (acc, t) =>
              switch (t) {
              | {
                  left: Some({relevant: Part_number(pl), _}),
                  middle: _,
                  right: Some({relevant: Part_number(pr), _}),
                } =>
                print_endline @@ "left: " ++ pl ++ " right: " ++ pr;
                acc + int_of_string(pl) * int_of_string(pr);
              | _ => invalid_arg("nah")
              },
            0,
            List.fold_left(
              (acc, m) =>
                switch (m) {
                | {left: l1, middle: m1, right: r1} =>
                  if (List.exists(
                        ma =>
                          switch (ma) {
                          | {left: l2, middle: m2, right: r2} =>
                            l1 == r2 && l2 == r1 && m1 == m2
                          },
                        acc,
                      )) {
                    acc;
                  } else {
                    [m, ...acc];
                  }
                },
              [],
              all_matched_with_last_stars^,
            )
            @ List.fold_left(
                (acc, m) =>
                  switch (m) {
                  | {left: l1, middle: m1, right: r1} =>
                    if (List.exists(
                          ma =>
                            switch (ma) {
                            | {left: l2, middle: m2, right: r2} =>
                              l1 == r2 && l2 == r1 && m1 == m2
                            },
                          acc,
                        )) {
                      acc;
                    } else {
                      [m, ...acc];
                    }
                  },
                [],
                all_matched_with_current_stars^,
              ),
          );
      count := count^ + 1;
      print_endline @@
      "\n^^^^^^^^^^^^^^line"
      ++ string_of_int(count^)
      ++ "^^^^^^^^^^^";
    },
  );
  print_endline @@ string_of_int(ans^);
};

let run = () => load_input("./input");
let _ = run();
