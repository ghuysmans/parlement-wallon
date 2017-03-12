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

let read_if what how =
  how >>= fun that ->
  if that = what then
    return true
  else
    oh_sorry false that

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


let p =
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
    let re = Str.regexp "Lieu *: *\\(.*\\)" in
    if Str.string_match re s 0 then
      let raw = Str.matched_group 1 s in
      (* Damien: reverse match! *)
      let re = Str.regexp "\\(.*\\) HUIS-CLOS$" in
      if Str.string_match re raw 0 then
        return (Str.matched_group 1 raw |> String.trim, true)
      else
        return (raw, false)
    else
      fail_gobble "lieu" in
  expect "PARLEMENT DE WALLONIE" read_nonempty id >>= fun () ->
  read_join >>= fun commission ->
  expect "CONVOCATION" read_nonempty id >>= fun () ->
  read_nonempty >>= fun rawdate ->
  extract_date_heure rawdate >>= fun (ts, tm) ->
  read_nonempty >>= fun rawlieu ->
  extract_lieu_huisclos rawlieu >>= fun (lieu, huisclos) ->
  Printf.printf "comm=%s, ts=%f, lieu=%s, huisclos=%s\n"
    commission ts lieu (if huisclos then "Y" else "N");
  return ()

let () = run p
