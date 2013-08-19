open Lwt_log_core
module D = Dom

let error = error_f
let fatal = fatal_f
let warning = warning_f
let notive = notice_f
let info = info_f
let debug = debug_f


let default_handler : (Js.js_string Js.t -> Js.js_string Js.t -> int -> bool Js.t) option ref = ref
    (Some (fun msg url line ->
         let msg = Js.to_string msg in
         let url = Js.to_string url in
         ign_error ~location:(url, line, 0) msg;
         Js._false))

let _ = (Obj.magic Dom_html.window)##onerror <- Js.wrap_callback (fun msg url line ->
    match ! default_handler with
      | None -> Js._false
      | Some h -> h msg url line)


module Console = struct
  let logger = Lwt_log_core.make
      ~close:(fun _ -> Lwt.return_unit)
      ~output:(fun section level logs ->
          let str =
            (Js.string
               (Printf.sprintf "[%s] %s" (Section.name section) (String.concat "\n" logs)))
          in
          (match level with
            | Debug -> Firebug.console##debug(str)
            | Info
            | Notice ->  Firebug.console##info(str)
            | Warning -> Firebug.console##warn(str)
            | Error
            | Fatal ->   Firebug.console##error(str)
          );
          Lwt.return_unit
        )

end

module History = struct

  type t = {
    current : string option;
    prev : string list;
    next : string list
  }

  let cons_opt opt l = match opt with
    | None -> l
    | Some x -> x::l

  let reset {current;prev;next} =
    {current = None;
     next = [];
     prev = match current with
       | None | Some "" -> List.rev_append next prev
       | Some c -> List.rev_append next (c::prev)}

  let up {current;prev;next} =
    match prev with
      | [] -> {current;prev;next}
      | x::xs ->
        {current=Some x;prev=xs;next= match current with
             | None | Some "" -> next
             | Some c ->  c :: next}

  let down t =
    match t with
      | {current = (None | Some ""); next = [] } -> {t with current = Some ""}
      | {current;prev;next = []} -> {t with current = Some ""; prev=cons_opt current prev}
      | {current;prev;next = x::xs} -> {current = Some x; next = xs; prev = cons_opt current prev}

  let add t str =
    let t = reset t in
    assert (t.current = None && t.next = []);
    match t.prev with
      | x::_ when x = str -> t
      | prev -> { t with prev = str :: prev}

end

module Dom = struct

  let color_of_kind = function
    | Debug ->          "black"
    | Error | Fatal ->  "red"
    | Warning ->        "yellow"
    | Info | Notice ->  "green"

  let xhtml_of_log (k,s) =
    let span = Dom_html.createSpan Dom_html.document in
    span##style##fontWeight <- Js.string "bold";
    span##style##color <- Js.string (color_of_kind k);
    span##innerHTML <- (Js.string (Printf.sprintf "[%s]" (String.capitalize (string_of_level k))));

    let a = Dom_html.createA Dom_html.document in
    a##innerHTML <- (Js.string (s));

    let li = Dom_html.createLi Dom_html.document in
    D.appendChild li span;
    D.appendChild li a;
    li

  type t = {
    mutable last_s : string;
    mutable last_n : int;
    mutable last_dom : Dom_html.liElement Js.t;
    dom : Dom_html.divElement Js.t
  }

  let make () =
    let dom = Dom_html.createUl Dom_html.document in
    {
      last_s = "";
      last_n = 1;
      last_dom = Dom_html.createLi Dom_html.document;
      dom
    }

  let append dom  x = try Dom.appendChild dom x with _ -> ()
  let clear t = t.dom##innerHTML <- Js.string ""

  let removeSelf node =
    let node = (node :> D.node Js.t) in
    Js.Opt.iter (node##parentNode) (fun p -> ignore(p##removeChild(node)) )

  let logger t = Lwt_log_core.make
      ~close:(fun _ -> Lwt.return_unit)
      ~output:(fun section level logs ->
          let s = String.concat "/n" logs in
          if s = t.last_s
          then
            begin
              t.last_n <- t.last_n + 1;
              removeSelf t.last_dom;
              let d = xhtml_of_log (level,Printf.sprintf "%d-%s" t.last_n s) in
              t.last_dom <- d;
              append t.dom d
            end
          else
            begin
              let d = xhtml_of_log (level,s) in
              t.last_n <- 1;
              t.last_dom <- d;
              t.last_s <- s;
              append t.dom d
            end;
          Lwt.return_unit)

end

module DomExt = struct

  type t = {
    base : Dom.t;
    dom : Dom_html.divElement Js.t;
    mutable history : History.t;
    mutable parsers : (string -> unit Lwt.t) list;
  }

  let display_block d s = ignore (
      React.S.map ( function
          | true ->  d##style##display <- Js.string "none"
          | false -> d##style##display <- Js.string "block") s
    )

  let add_command_handler t f =
    t.parsers <- f :: t.parsers

  let make ?opened () =
    let unseen,set_unseen = React.S.create (0,0) in
    let shown,set_shown = React.S.create false in
    let toread = React.S.l2 (fun (w,e) b ->
        if b
        then 0,0
        else w,e) unseen shown in

    let new_log,send_new_log = React.E.create () in

    let _ = React.S.sample (fun k b ->
        match b,k with
          | false,Error ->
            Lwt.async (fun () ->
                let w,e = React.S.value unseen in
                set_unseen (w,succ e); Lwt.return_unit)
          | false,Warning ->
            Lwt.async (fun () ->
                let w,e = React.S.value unseen in
                set_unseen (succ w,e); Lwt.return_unit)
          | _,_ -> ()
      ) new_log shown in

    let _ = React.S.map (function
        | true -> Lwt.async (fun () -> set_unseen (0,0);Lwt.return_unit)
        | _ -> ()) shown in


    (* Command handlers *)

  let input_box =
    let x = Dom_html.createInput Dom_html.document in
    (* x##_type <- Js.string "text"; *)
    x##placeholder <- Js.string "CMD";
    x##id <- Js.string "search_input";
    x

  let parse s =
    history_add s;
    let rec loop = function
      | [] -> Lwt.return_unit
      | f::fs ->
        try_lwt f s with _ -> loop fs
    in Lwt.async (fun () ->
        lwt () = loop !parsers in
        input_box##value <- Js.string "" ;
        Lwt.return_unit)


  let debug_container =
    let dom_hide =
      let x = Dom_html.createA Dom_html.document in
      x##onclick <- Dom_html.handler (fun _ -> set_shown false; Js._true);
      x##innerHTML <- Js.string "Hide";
      x in
    let dom_clear =
      let x = Dom_html.createA Dom_html.document in
      x##onclick <- Dom_html.handler (fun _ -> Dom.clear (); Js._true);
      x##innerHTML <- Js.string "Clear";
      x in

    let dom_div = Dom_html.createDiv Dom_html.document in

    D.appendChild dom_div dom_hide;
    D.appendChild dom_div dom_clear;
    D.appendChild dom_div Dom.dom;
    D.appendChild dom_div input_box;

    display_block dom_div (React.S.map (not) shown);
    dom_div

  let debug_button =
    let title_dom = Dom_html.createA Dom_html.document in
    let _ = React.S.map (function
        | 0,0 ->
          title_dom##innerHTML <- Js.string "Debug";
          title_dom##style##color <- Js.string "black"
        | w,0 ->
          title_dom##innerHTML <- Js.string  (Printf.sprintf "Debug:%dw" w);
          title_dom##style##color <- Js.string "yellow"
        | w,e ->
          title_dom##innerHTML <- Js.string  (Printf.sprintf "Debug:%dw,%de" w e);
          title_dom##style##color <- Js.string "red"
      ) toread in

    title_dom##onclick <- Dom_html.handler (fun _ -> set_shown true; Js._true);

    let div = Dom_html.createDiv Dom_html.document in
    D.appendChild div title_dom;
    display_block div shown;
    div

  let _ = add_command_handler (fun s -> error_f "cmd not found : %s" s)

  let logger t = Lwt_log_core.make
      ~close:(fun _ -> Lwt.return_unit)
      ~output:(fun section level logs -> send_new_log level; Lwt.return_unit)

  let logger = broadcast [logger;Dom.logger]

  let dom =
    let dom_div = Dom_html.createDiv Dom_html.document in
    D.appendChild dom_div debug_container;
    D.appendChild dom_div debug_button;
    dom_div

  let init ?(opened=false) () =
    ignore (input_box##onkeyup <- Dom_html.handler (fun e ->
        if e##keyCode == Keycode.return
        then parse (Js.to_string (input_box##value))
        else if e##keyCode == Keycode.up
        then match history_up () with
          | None -> ()
          | Some x -> input_box##value <- Js.string x
        else if e##keyCode == Keycode.down
        then match history_down () with
          | None -> ()
          | Some x -> input_box##value <- Js.string x
        else ();
        Js._false));
    D.appendChild Dom_html.document##body dom;
    set_shown opened

end


let _ = Dom_html.window##onload <- Dom_html.handler (fun _ ->
  DomExt.init ();
  default:=broadcast [Console.logger;DomExt.logger];
  Js._true)
