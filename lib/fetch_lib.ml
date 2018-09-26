open Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

type page_cache =
  String.t Base.Hashtbl.M(String).t
[@@deriving sexp]

let _fetch url =
  Client.get (Uri.of_string url) >>= (fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.printf "Body of length: %d\n" (String.length body);
      body
    )

let cached ~cache fetch =
  fun url ->
    match Hashtbl.find cache url with
    | Some page ->
      Lwt.return page
    | None ->
      Lwt.( fetch url >|= (fun page ->
          let _ = Hashtbl.add cache ~key:url ~data:page in
          page
        ))

let cache_file =
  "cache.bin"

(* let with_cache_in f =
 *   In_channel.with_file cache_file ~f *)

let with_cache_out f =
  Out_channel.with_file cache_file ~f

let write_to_cache tbl =
  with_cache_out (fun oc ->
      Sexp.output oc (sexp_of_page_cache tbl))

let read_from_cache () =
  let empty = Hashtbl.create ~size:1 (module String) in
  match Sys.file_exists cache_file with
  | `No | `Unknown ->
    write_to_cache empty;
    empty
  | `Yes ->
    try
      page_cache_of_sexp (Sexp.load_sexp cache_file)
    with _ ->
      empty

let fetch =
  cached _fetch
