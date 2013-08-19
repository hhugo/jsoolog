
let _ = Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let t = Joolog.DomExt.make () in
    Dom.appendChild (Dom_html.document##body) (Joolog.DomExt.dom t);
    Joolog.default :=  Joolog.broadcast [Joolog.Console.logger;Joolog.DomExt.logger t];
    Js._true)
