
module Console : sig
  val logger : Lwt_log_core.logger
end

module Dom : sig
  type t
  val make : unit -> t
  val logger : t -> Lwt_log_core.logger

  val clear : t -> unit
  val dom : t -> Dom_html.divElement Js.t

end

module DomExt : sig
  type t
  val make : ?opened:bool -> unit -> t
  val logger : t -> Lwt_log_core.logger

  val add_command_handler : t -> (string -> unit Lwt.t) -> unit
  val dom : t -> Dom_html.divElement Js.t

end
