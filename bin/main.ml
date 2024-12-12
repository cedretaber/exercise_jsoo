open Js_of_ocaml
open Utils

module Todo = struct
  type t = {
    user_id : int;
    id : int;
    title : string;
    completed : bool;
  }

  let make ~user_id ~id ~title ~completed = { user_id; id; title; completed }

  let to_string = function
    | { user_id; id; title; completed } ->
        Printf.sprintf "userId: %d, id: %d, title: %s, completed: %b" user_id id title completed
end

let todo_url = "https://jsonplaceholder.typicode.com/todos/1"

let parse_todo json =
  match
    Js.Optdef.to_option json##.userId,
    Js.Optdef.to_option json##.id,
    Js.Optdef.to_option json##.title,
    Js.Optdef.to_option json##.completed
  with
  | Some user_id, Some id, Some title, Some completed ->
      Result.Ok (
        Todo.make
          ~user_id:(Js.to_int32 user_id |> Int32.to_int)
          ~id:(Js.to_int32 id |> Int32.to_int)
          ~title:(Js.to_string title)
          ~completed:(Js.to_bool completed)
      )
  | _ ->
      Result.Error "Invalid JSON"

let fetch_todo_as_json _ =
  Fetch.fetch_json todo_url
  |> Promise.then_ (fun json ->
    Console.log json;
    parse_todo json
  )

let parse_todo = function
  | `Assoc [
      ("userId", `Int user_id);
      ("id", `Int id);
      ("title", `String title);
      ("completed", `Bool completed)
    ] ->
      Result.Ok (Todo.make ~user_id ~id ~title ~completed)
  | json ->
      Result.Error (Printf.sprintf "Invalid JSON: %s" @@ Yojson.Safe.show json)

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
  let f = fun f ->
    fetch_todo_as_json ()
    |> Promise.then_ (fun result ->
      match result with
      | Result.Ok todo -> Todo.to_string todo
      | Result.Error err -> err
    )
    |> Promise.then_ f
    |> ignore
  in
  onclick f

let onclick_text _ =
  print_endline "fetching todo as text";
  let f = fun f ->
    fetch_todo_as_text ()
    |> Promise.then_ (fun result ->
      match result with
      | Result.Ok todo -> Todo.to_string todo
      | Result.Error err -> err
    )
    |> Promise.then_ f
    |> ignore
  in
  onclick f

let onload _ =
  (Dom_html.getElementById "fetch_button_json")##.onclick := Dom_html.handler onclick_json;
  (Dom_html.getElementById "fetch_button_text")##.onclick := Dom_html.handler onclick_text;
  Js._false

let _ =
  Dom_html.window##.onload := Dom_html.handler onload
