(* This file is part of xmlfuzzer.
 *
 * xmlfuzzer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * xmlfuzzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with xmlfuzzer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2010-2011 Alexander Markov <apsheronets@gmail.com>
 *)

module L = Ocamlduce.Load
open Schema
open Printf

let load_xml fn =
  let xml = Xml.parse_file fn in
  let l = L.make ~ns:true () in
  let rec aux = function
    | Xml.Element (tag, attrs, child) ->
        L.start_elem l tag attrs; List.iter aux child; L.end_elem l ()
    | Xml.PCData s ->
        L.text l s in
  aux xml;
  L.get l

let include_uri base local =
  let l =
    if Filename.is_relative local
    then Filename.concat (Filename.dirname base) local
    else local in
  Printf.eprintf "Include %s\n" l; flush stderr;
  l

let import_uri imported base local ns =
  if local = "" then None
  else
    let l =
      if Filename.is_relative local
      then Filename.concat (Filename.dirname base) local
      else local in
    if List.mem l !imported
    then None
    else (imported := l :: !imported;
    Printf.eprintf "Import %s\n" l; flush stderr; Some l)

let load url =
  try {{ (load_xml url :? Schema_dtd.schema) }}
  with Failure s ->
    Printf.eprintf "Invalid document:%s@." s;
    exit 2

let load s =
  Schema_loader.parse_schema
    ~include_uri ~import_uri:(import_uri (ref [])) ~load s

let () =
  let xsd       = ref "" in
  let root_elem = ref "" in
  let batch = ref "" in
  let count = ref 0  in
  let min_level  = ref 3 in
  let max_level  = ref 10 in
  let max_repeat = ref 10 in
  let max_elem   = ref 10000 in
  let ignore_tags  = ref "" in
  let ignore_attrs = ref "" in
  let preferred_tags = ref "" in
  let skip_namespaces = ref false in
  let no_cdata = ref false in
  let help =
    "xmlfuzzer - an XML fuzzing tool\n" ^
    "Usage: xmlfuzzer -xsd FILE -root-elem ELEM [OPTIONS]" in
  let l = [
    "-xsd"      , Arg.Set_string xsd      , "FILE  xsd file";
    "-root-elem", Arg.Set_string root_elem, "ELEM  root element";
    "-batch", Arg.Set_string batch, "DIR   batch mode";
    "-count", Arg.Set_int count,
      sprintf "NUM   count of files for batch mode; default is %u" !count;
    "-min-level",  Arg.Set_int min_level,
      sprintf " NUM  minimum level of tree; default is %u" !min_level;
    "-max-level",  Arg.Set_int max_level,
      sprintf " NUM  maximum level of tree; this is a soft limit; default is %u" !max_level;
    "-max-repeat", Arg.Set_int max_repeat,
      sprintf "NUM  how much can various elements be contained in other element; default is %u" !max_repeat;
    "-max-elem", Arg.Set_int max_elem,
      sprintf "NUM  maximum of elements in the whole tree; default is %u" !max_elem;
    "-ignore-tags",  Arg.Set_string ignore_tags,  "FILE  file with tags to be ignored";
    "-ignore-attrs", Arg.Set_string ignore_attrs, "FILE  file with attributes to be ignored";
    "-preferred-tags", Arg.Set_string preferred_tags, "FILE  file with tags to be not ignored by random";
    "-skip-namespaces", Arg.Set skip_namespaces, "do not output namespaces";
    "-no-cdata", Arg.Set no_cdata, "write empty strings only; for testing";
  ] in
  Arg.parse l (fun _ -> raise (Arg.Bad help)) help;
  let lines_of_file path =
    let inp = open_in path in
    let rec loop acc =
      try loop ((input_line inp)::acc)
      with End_of_file -> close_in inp; List.rev acc in
    loop [] in
  let lines_of_file x =
    if !x <> "" then lines_of_file !x else [] in
  let ignore_tags    = lines_of_file ignore_tags    in
  let ignore_attrs   = lines_of_file ignore_attrs   in
  let preferred_tags = lines_of_file preferred_tags in

  if !xsd       = "" then (Printf.eprintf "you must specify a xsd file\n"    ; exit 1);
  if !root_elem = "" then (Printf.eprintf "you must specify a root element\n"; exit 1);

  let s =
    try load !xsd
    with
      | Xml.Error err ->
          eprintf "Parsing error: %s\n" (Xml.error err);
          exit 2
      | exn ->
          eprintf "Error: %s\n"
          (Printexc.to_string exn);
          exit 2 in
  let d =
    List.find (fun x ->
      let _, s = Ocamlduce.Atom.get x.el_name in
      let s = Ocamlduce.Utf8.get s in
      s = !root_elem) s.elements in
  Random.self_init ();
  let module F = Fuzz.Make (struct
    let min_level  = !min_level
    let max_level  = !max_level
    let max_repeat = !max_repeat
    let max_elem   = !max_elem
    let ignore_tags    = ignore_tags
    let ignore_attrs   = ignore_attrs
    let preferred_tags = preferred_tags
    let skip_namespaces = !skip_namespaces
    let no_cdata        = !no_cdata
  end) in
  if !batch <> "" then (
    if !count < 1 then (eprintf "you must specify count of files"; exit 1);
    for n = 1 to !count do (
      let width = String.length (string_of_int !count) in
      let filename = sprintf "%.*u" width n in
      let path = Filename.concat !batch filename in
      let e = F.make_element d in
      let out = open_out path in
      Ocamlduce.Print.print_xml (output_string out) e;
      close_out out
    ) done
  ) else (
    let e = F.make_element d in
    Ocamlduce.Print.print_xml print_string e;
    print_newline ()
  )


