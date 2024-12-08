open Js_of_ocaml
open Utils

let todo_url = "https://jsonplaceholder.typicode.com/todos/1"

let parse_todo json =
  match
    Js.Optdef.to_option json##.userId,
    Js.Optdef.to_option json##.id,
    Js.Optdef.to_option json##.title,
    Js.Optdef.to_option json##.completed
  with
  | Some userId, Some id, Some title, Some completed ->
      Printf.sprintf
        "userId: %d, id: %d, title: %s, completed: %b"
        (Js.to_int32 userId |> Int32.to_int)
        (Js.to_int32 id |> Int32.to_int)
        (Js.to_string title)
        (Js.to_bool completed)
  | _ ->
      "Invalid JSON"

let fetch_todo_as_json _ =
  Fetch.fetch_json todo_url
  |> Promise.then_ (fun json ->
    Console.log json;
    parse_todo json
  )

let parse_todo = function
  | `Assoc [
      ("userId", `Int userId);
      ("id", `Int id);
      ("title", `String title);
      ("completed", `Bool completed)
    ] ->
      Printf.sprintf "userId: %d, id: %d, title: %s, completed: %b" userId id title completed
  | json ->
      Printf.sprintf "Invalid JSON: %s" (Yojson.Safe.show json)

let fetch_todo_as_text _ =
  Fetch.fetch_text todo_url
  |> Promise.then_ (fun text ->
    Console.log text;
    let text = Js.to_string text in
    let json = Yojson.Safe.from_string text in
    parse_todo json
  )

let onclick f =
  f (fun todo -> (Dom_html.getElementById "fetch_result")##.textContent := Js.some (Js.string todo));
  Js._false

let onclick_json _ =
  print_endline "fetching todo as json";
  let f = fun f -> fetch_todo_as_json () |> Promise.then_ f |> ignore in
  onclick f

let onclick_text _ =
  print_endline "fetching todo as text";
  let f = fun f -> fetch_todo_as_text () |> Promise.then_ f |> ignore in
  onclick f

let onload _ =
  (Dom_html.getElementById "fetch_button_json")##.onclick := Dom_html.handler onclick_json;
  (Dom_html.getElementById "fetch_button_text")##.onclick := Dom_html.handler onclick_text;
  Js._false

let _ =
  Dom_html.window##.onload := Dom_html.handler onload
