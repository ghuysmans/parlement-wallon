type s =
  | Reused of string
  | Next_line

(* TODO keep the line number for better error messages *)
type 'a t = s -> ('a, string) result * s


let return x source =
  Ok x, source

(* maybe useless, but anyway... *)
let fail_gobble e _ =
  Error e, Next_line

let not_interested x l _ =
  Ok x, Reused l
let oh_sorry = not_interested

let (>>=) m f s =
  match m s with
  | Ok x, s' -> f x s'
  | Error e, s' -> Error e, s'

let interesting_line l =
  l="" ||
  try
    ignore (Str.search_forward (Str.regexp "[A-Za-z]") l 0);
    true
  with Not_found ->
    false

let rec read_one source =
  try
    let l = match source with
      | Reused s -> s
      | Next_line -> read_line () in
    if interesting_line l then
      return (String.trim l) Next_line
    else
      read_one Next_line
  with End_of_file ->
    fail_gobble "EOF" Next_line

let read_nonempty =
  let rec f () =
    read_one >>= fun l ->
    if l = "" then
      f ()
    else
      return l in
  f ()

let read_par =
  let rec f acc =
    read_one >>= fun l ->
    if l = "" then
      return (List.rev acc)
    else
      f (l :: acc) in
  f []

let read_join =
  read_par >>= fun x -> return (String.concat " " x)

let run m =
  match m Next_line with
  | Ok x, _ -> x
  | Error e, _ -> failwith e

let expect what how err =
  how >>= fun that ->
  if that = what then
    return ()
  else
    fail_gobble ("expected " ^ err what)

let read_if cond how =
  how >>= fun that ->
  if cond that then
    return (Some that)
  else
    oh_sorry None that

let id x = x

let index l e =
  let rec f i = function
    | [] -> -1
    | x :: t ->
        if x = e then
          i
        else
          f (i+1) t in
  f 0 l

let default_to d = function
  | Some x -> x
  | None -> d

(* http://langref.org/ocaml/strings/reversing-a-string/reverse-characters *)
let reverse str =
  let l = Str.split (Str.regexp "") str in
  List.fold_left (fun a b -> b ^ a) "" l


let meta =
  let extract_date_heure s =
    let re = Str.regexp
      ("Date *: *\\([^ 0-9]+ \\)?\\([0-9]+\\) \\([^ 0-9]+\\) " ^
        "\\([0-9][0-9][0-9][0-9]\\) +" ^
        "Heure *: *\\([0-9]+\\)h\\([0-9]+\\)?") in
    let les_mois = ["janvier"; "février"; "mars"; "avril"; "mai"; "juin";
      "juillet"; "août"; "septembre"; "octobre"; "novembre"; "décembre"] in
    if Str.string_match re s 0 then
      let mois = Str.matched_group 3 s in
      if List.mem mois les_mois then
        let min =
          try
            Str.matched_group 7 s |> int_of_string
          with Invalid_argument _ ->
            0 in
        return (Unix.mktime {
          Unix.tm_sec = 0;
          tm_min = min;
          tm_hour = Str.matched_group 5 s |> int_of_string;
          tm_mday = Str.matched_group 2 s |> int_of_string;
          tm_mon = index les_mois mois;
          tm_year = (Str.matched_group 4 s |> int_of_string) - 1900;
          tm_wday = -1;
          tm_yday = -1;
          tm_isdst = true;
        })
      else
        fail_gobble "mois"
    else
      fail_gobble "date/heure" in
  let extract_lieu_huisclos s =
    (* dark magic here, you're not supposed to understand this awoken *)
    let re = Str.regexp "\\(SOLC-SIUH \\)?\\(.*\\) *: *ueiL$" in
    let rev = reverse s in
    if Str.string_match re rev 0 then
      return (
        Str.matched_group 2 rev |> String.trim |> reverse,
        try
          ignore (Str.matched_group 1 rev);
          true
        with Not_found ->
          false
      )
    else
      fail_gobble "lieu" in
  expect "PARLEMENT DE WALLONIE" read_nonempty id >>= fun () ->
  read_join >>= fun commission ->
  expect "CONVOCATION" read_nonempty id >>= fun () ->
  read_nonempty >>= extract_date_heure >>= fun (ts, tm) ->
  read_nonempty >>= extract_lieu_huisclos >>= fun (lieu, huisclos) ->
  return ()

let contains needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found ->
    false

let is_title t =
  let re = Str.regexp "[^a-z]+$" in
  Str.string_match re t 0

let read_lines cond group =
  let rec f all cur =
    let gr () =
      if List.length cur = 0 then
        all
      else
        group cur :: all in
    read_if cond read_one >>= function
    | Some "" -> f (gr ()) []
    | Some l -> f all (l :: cur)
    | None -> return (List.rev (gr ())) in
  f [] []

let orgatravaux =
  read_if (contains "ORGANISATION DES TRAVAUX") read_nonempty >>= function
  | None -> return []
  | Some _ -> read_lines
    (fun x -> not (is_title x))
    (fun x -> String.concat " " (List.rev x))

let clean_enum l =
  let gr cur all =
    if List.length cur = 0 then
      all
    else
      String.concat " " (List.rev cur) :: all in
  let f (cur, all) x =
    if x.[0] = '-' then
      [String.sub x 1 (String.length x - 1) |> String.trim], gr cur all
    else
      String.trim x :: cur, all in
  let cur, all = List.fold_left f ([], []) l in
  gr cur all


let auditions =
  read_if (contains "AUDITIONS") read_join >>= function
  | None -> return []
  | Some _ ->
    read_if (fun x -> not (is_title x)) read_nonempty >>= function
    | None -> return []
    | Some _ ->
      read_lines
        (fun x -> not (contains "Audition de :" x))
        (fun x -> String.concat " " (List.rev x)) >>= fun l ->
      let sujet = List.hd l in
      read_one >>= fun _ ->
      read_lines ((<>) "") id >>= fun l ->
      let qui = List.concat l |> clean_enum in
      List.iter (fun x -> print_endline ("!" ^ x)) qui;
      return []


let () = List.iter print_endline (clean_enum ["-a"; "-b"; "bbb"])
let () = run (
  meta >>= fun () ->
  orgatravaux >>= fun ot ->
  auditions >>= fun au ->
  return ()
)
