type s =
  | Reused of string
  | Next_line

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
      return l Next_line
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


(*
 * passer dans un mode dès qu'on voit un truc spécial (arriérés...)
 * ignorer les chiffres (éventuellement 7- 1) seuls sur une ligne
 *)

let () = run read_par |> List.iter print_endline
