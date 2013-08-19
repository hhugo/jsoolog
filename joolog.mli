
include module type of Lwt_log_core
  with type logger = Lwt_log_core.logger
   and type level  = Lwt_log_core.level
   and type template = Lwt_log_core.template
   and type section = Lwt_log_core.section
   and module Section = Lwt_log_core.Section

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
