
let cmd = function
  | "warn" -> Joolog.warning "warn"
  | "info" -> Joolog.info "info"
  | "debug" -> Joolog.debug "debug"
  | "error" -> Joolog.error "error"
  | "fatal" -> Joolog.fatal "fatal"
  | ss -> Joolog.fatal ss

let _ = (Js.Unsafe.coerce Dom_html.window)##cmd <- (fun s -> cmd (Js.to_string s))

let _ = Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let t = Joolog.Dom.make () in
    let img = Dom_html.createImg Dom_html.document in
    img##src <- Js.string "fake.png";
    Dom.appendChild (Dom_html.document##body) (Joolog.Dom.dom t);
    Dom.appendChild (Dom_html.document##body) img;
    Joolog.default :=  Joolog.broadcast [Joolog.Console.logger;Joolog.Dom.logger t];
    Lwt_log_core.Section.set_level Lwt_log_core.Section.main Lwt_log_core.Debug;
    Joolog.Dom.add_command_handler t cmd;
    Js._true)
