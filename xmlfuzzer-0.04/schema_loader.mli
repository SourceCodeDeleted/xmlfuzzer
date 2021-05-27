(* This is the XML Schema loader.
   It produces an OCaml structure from an XML tree. *)

exception Cycle of {{ Atom }}
  (** Raised in presence of an ill-formed recursion in a schema *)

val parse_schema :
  include_uri:(string -> string -> string) ->
    (* local uri, include location -> new uri *)
  import_uri:(string -> string -> string -> string option) ->
    (* local uri, import location, import ns -> new uri *)
  load:(string -> Schema_dtd.schema) ->
  string -> Schema.schema
