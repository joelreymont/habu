module L = Lexing
module E = MenhirLib.ErrorReports
module LU = MenhirLib.LexerUtil
open Sexplib
open Tag

type pos = Position.t [@@deriving sexp]

let ( let* ) = Result.bind

let ( and* ) left right =
  match left, right with
  | Ok left, Ok right -> Ok (left, right)
  | Error left, Error right -> Error (left @ right)
  | Error e, _ | _, Error e -> Error e
;;

module Error = struct
  type t =
    [ `Invalid of pos
    | `Duplicate of pos
    | `Not_found of pos
    | `Syntax_error of pos
    ]
  [@@deriving sexp]
end

module Result = struct
  type 'a t = ('a, Error.t) Result.t

  let ok v = Ok v
  let error (e : Error.t) = Error e

  let sexp_of_t sexp_of_a = function
    | Ok r -> sexp_of_a r
    | Error e -> Error.sexp_of_t e
  ;;

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | Sexp.List [ Atom "ok"; s ] -> Ok (a_of_sexp s)
    | Sexp.List [ Atom "error"; s ] -> Error (Error.t_of_sexp s)
    | _ -> Conv.of_sexp_error "t_of_sexp: atom needed" sexp
  ;;

  let of_value pred v =
    let pos = tag v in
    let value = value v in
    if pred value then
      Ok value
    else
      Error (`Invalid pos)
  ;;

  let ensure_not_empty l = of_value (fun l -> not (List.is_empty l)) l
  let ensure_positive n = of_value (fun n -> n > 0) n
  let ensure_non_negative n = of_value (fun n -> n >= 0) n

  let rec fold f accu l =
    match l with
    | [] -> Ok accu
    | r :: l ->
      (match r with
       | Error _ as e -> e
       | Ok a -> fold f (f accu a) l)
  ;;

  let rec iter f = function
    | [] -> Ok ()
    | x :: xs ->
      (match f x with
       | Error _ as e -> e
       | Ok _ -> iter f xs)
  ;;

  let map = Result.map
  let map_error = Result.map_error

  let map_list f l =
    let rec f' accu f = function
      | [] -> Ok accu
      | x :: xs ->
        (match f x with
         | Error _ as e -> e
         | Ok a -> f' (a :: accu) f xs)
    in
    f' [] f l
  ;;
end
