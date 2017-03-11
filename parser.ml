type t =
  | Text of string
  | Tag of string * (string * string) list * t list

type s =
  | Reused of string
  | Next_line

let get_level s =
  let rec f i =
    if i = String.length s then
      i
    else if s.[i] = '\t' then
      f (i+1)
    else
      i-1 in
  f 0

let indent s level =
  String.make level '\t' ^ s

let rec disp level = function
  | Text s ->
    indent s level |> print_endline
  | Tag (t, a, c) ->
    (
      indent t level |> print_endline;
      (* TODO *)
      List.iter (disp (level+1)) c
    )

let subexplain s p l =
  let len = String.length s in
  Printf.printf "|%s|, p=%d, l=%d, t=%d\n" s p l len;
  if len = p then
    print_endline (String.make (p+1) ' ' ^ "-")
  else
    print_endline (String.make (p+1) ' ' ^ "*" ^ String.make (len-p-1) ' ' ^ "*");
  String.sub s p l

let rec parse source ch brothers =
  try
    let l = match source with
      | Reused s -> s
      | Next_line -> input_line ch in
    try
      let level = get_level l in
      let p = String.index l ':' in
      let before = String.sub l (level+1) (p-level-1) in
      (* FIXME parse attributes *)
      let first_child =
        let s = subexplain l (p+1) (String.length l - p - 2 - level) in
        if s = "" then
          []
        else
          [Text s] in
      try
        let next = input_line ch in
        let next_level = get_level next in
        if next_level < level then
          Tag (before, [], first_child) :: brothers, Reused next
        else if next_level = level then
          parse (Reused next) ch (Tag (before, [], first_child) :: brothers)
        else
          let children, next' = parse (Reused next) ch [] in
          Tag (before, [], first_child @ children) :: brothers, next'
      with End_of_file ->
        Tag (before, [], first_child) :: brothers, Next_line
    with Not_found ->
      parse Next_line ch (Text l :: brothers)
  with End_of_file ->
    brothers, Next_line


let () =
  ignore (subexplain "abc" 1 2);
  let l, _ = parse Next_line stdin [] in
  List.iter (disp 0) l
