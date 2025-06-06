(** This module provides a uniform way of reporting (located) error message. *)

val exit_on_error : unit -> unit
(** [exit_on_error ()] forces the program to stop if an error is encountered.
    (This is the default behavior.) *)

val resume_on_error : unit -> unit
(** [resume_on_error ()] makes the program throw the exception {!Error}
    if an error is encountered. *)

exception Error of Position.t list * string

val print_error : Position.t list -> string -> string
(** [print_error positions msg] formats an error message. *)

val error : Position.t -> string -> 'a
(** [error p msg] prints [msg] and stops the program. *)

val error2 : Position.t -> Position.t -> string -> 'a
(** [error2 p1 p2 msg] prints two positions instead of one. *)

val errorN : Position.t list -> string -> 'a
(** [errorN ps msg] prints several positions. *)

val global_error : string -> 'a
(** [global_error msg] prints [msg] and stops the program. *)
