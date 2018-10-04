open Core
open Lwt
open Fetch_lib

let remove_unimportant_attributes node =
  let open Soup in
  fold_attributes (fun _ k _ ->
      if k <> "href" then
        delete_attribute k node
    ) () node

let rec tree_remove_attributes node =
  remove_unimportant_attributes node;
  let open Soup in
  node |> children |> elements |> iter tree_remove_attributes

let () =
  match Sys.argv with
  | [|_; url|] ->
    let tbl = read_from_cache () in
    let body = Lwt_main.run (fetch ~cache:tbl url) in
    write_to_cache tbl;
    let open Soup in
    let content = parse body $ "#content" in
    let title =
      children content
      |> elements
      |> filter (fun el -> name el = "h2")
      |> first
      |> Option.map ~f:to_string |> Option.value ~default:"?" in
    let nodes = content $ ".entry" |> children |> elements |> to_list in
    let () = List.iter ~f:tree_remove_attributes nodes in
    print_endline ("title: " ^ title);
    print_endline ("entry: " ^ (List.map ~f:to_string nodes |> String.concat ~sep:"\n"))

  | _ ->
    Out_channel.output_string stderr "pass URL as an argument.\n"
