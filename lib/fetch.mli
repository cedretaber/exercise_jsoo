open Js_of_ocaml

val fetch_json : string -> _ Js.t Promise.t

val fetch_text : string -> Js.js_string Js.t Promise.t
