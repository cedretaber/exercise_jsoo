open Js_of_ocaml

let log any : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [|Js.Unsafe.inject any|]
