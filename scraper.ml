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

let read_par =
  let rec f acc =
    read_one >>= fun l ->
    if l = "" then
      return (List.rev acc)
    else
      f (l :: acc) in
  f []

let run m =
  match m Next_line with
  | Ok x, _ -> x
  | Error e, _ -> failwith e


(*
 * passer dans un mode dès qu'on voit un truc spécial (arriérés...)
 * ignorer les chiffres (éventuellement 7- 1) seuls sur une ligne
 *)

let () = run read_par |> List.iter print_endline
