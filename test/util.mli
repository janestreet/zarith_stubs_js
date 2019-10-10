open! Z
open! Core_kernel

module Dynamic : sig
  val quickcheck : ?include_zero:bool -> ?trials:int -> f:(t -> unit) -> unit -> unit
  val quickcheck_pair : ?include_zero:bool -> f:(t -> t -> unit) -> unit -> unit
  val quickcheck_tripple : ?include_zero:bool -> f:(t -> t -> t -> unit) -> unit -> unit
end

module Filter : sig
  type t

  val not_zero : t
  val positive : t
  val small : t
  val odd : t
  val combine : t list -> t
end

module Static : sig
  val quickcheck
    :  ?quiet:bool
    -> ?verbose:bool
    -> ?filter:Filter.t
    -> f:(t -> Sexp.t)
    -> unit
    -> unit

  val quickcheck_pair
    :  ?quiet:bool
    -> ?verbose:bool
    -> ?filter:Filter.t
    -> f:(t -> t -> Sexp.t)
    -> unit
    -> unit

  val quickcheck_tripple
    :  ?quiet:bool
    -> ?verbose:bool
    -> ?filter:Filter.t
    -> f:(t -> t -> t -> Sexp.t)
    -> unit
    -> unit
end

val sexp_of_t : t -> Sexp.t
