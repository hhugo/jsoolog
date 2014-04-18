
include module type of Lwt_log_core
  with type logger = Lwt_log_core.logger
   and type level  = Lwt_log_core.level
   and type template = Lwt_log_core.template
   and type section = Lwt_log_core.section
   and module Section = Lwt_log_core.Section

module Console : sig
  (** Logger for the javascript console *)
  val logger : Lwt_log_core.logger
end

module SimpleDom : sig
  (** Logger into dom element *)

  (** Type of dom logger *)
  type t

  (** Create a dom logger *)
  val make : unit -> t

  (** Get the Lwt logger from a dom logger *)
  val logger : t -> Lwt_log_core.logger

  (** Clear the list of logs *)
  val clear : t -> unit

  (** Get the Dom element from a dom logger *)
  val dom : t -> Dom_html.divElement Js.t

end

module Dom : sig
  (** Logger into dom element. Wrapper arround SimpleDom : compact/expand view, clear, input command *)

  (** Type of dom logger *)
  type t

  (** Create a dom logger *)
  val make : ?opened:bool -> unit -> t

  (** Get the Lwt logger from a dom logger *)
  val logger : t -> Lwt_log_core.logger

  (** Add a command handler to the logger *)
  val add_command_handler : t -> (string -> unit Lwt.t) -> unit

  (** Get the Dom element from a dom logger *)
  val dom : t -> Dom_html.divElement Js.t

end
