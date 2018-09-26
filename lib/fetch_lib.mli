type page_cache

val write_to_cache : page_cache -> unit

val read_from_cache : unit -> page_cache

val fetch: cache:page_cache -> string -> string Lwt.t
