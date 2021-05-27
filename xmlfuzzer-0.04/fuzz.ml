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

open ExtLib
open Tools
open Schema

exception Cannot_build

module type Options = sig
  val min_level:  int
  val max_level:  int
  val max_repeat: int
  val max_elem:   int
  val ignore_tags:  string list
  val ignore_attrs: string list
  val preferred_tags: string list
  val skip_namespaces: bool
  val no_cdata: bool
end

module Make(Opt: Options) = struct

  type any = {{ [ (Ocamlduce.Load.anyxml | Char)* ] }}

  let ignore_attr a =
    let comp x =
      let atom = a.at_decl.at_name in
      let _, s = Ocamlduce.Atom.get atom in
      let s = Ocamlduce.Utf8.get s in
      x = s in
    List.exists (comp) Opt.ignore_attrs

  let ignore_tag name =
    List.exists (fun x ->
      let _, s = Ocamlduce.Atom.get name in
      let s = Ocamlduce.Utf8.get s in
      x = s) Opt.ignore_tags

  let preferred_tag name =
    List.exists (fun x ->
      let _, s = Ocamlduce.Atom.get name in
      let s = Ocamlduce.Utf8.get s in
      x = s) Opt.preferred_tags

  let make_simple_definition sd =
    if Opt.no_cdata
    then {{ [] }}
    else Rand.value sd.st_facets

  let make_attribute d =
    let atom = d.at_name in
    let atom = if Opt.skip_namespaces then skip_namespace atom else atom in
    if Opt.no_cdata
    then Ocamlduce.Record.make [(atom, {{ [] }})]
    else Ocamlduce.Record.make [(atom, Rand.value d.at_typdef.st_facets)]

  let make_attribute_use al =
    List.fold_left (fun (acc : Ocamlduce.Record.t ) a ->
      let ignored = ignore_attr a in
      if a.at_required && ignored
      then raise Cannot_build
      else if (a.at_required || Random.bool ()) && (not ignored)
      then
        let a = make_attribute a.at_decl in
        {{ acc ++ a }}
      else
        acc) {{ {} }} al

  let make_cdata () =
    if Opt.no_cdata
    then {{ [] }}
    else
      Rand.some [
        Rand.random_string;
        Rand.empty_string;
        Rand.short_string;
        Rand.medium_string;
        Rand.long_string;
      ]

  let rec make_particle ?(mixed=true) (l, counter) p =
    let cdata () =
      if Random.bool ()
      then make_cdata ()
      else {{ [] }} in
    match p.part_term with
    | Elt e ->
        let max = match p.part_max with
          | None -> Opt.max_repeat
          | Some i -> {: i :} in
        let min = {: {{ p.part_min }} :} in
        let max =
          let rest = Opt.max_elem - counter in
          if rest < max then
            if rest < min
            then raise Cannot_build
            else rest
          else max in
        let count =
          (* case: this tag is important and we need at least one if only it available *)
          if (l < Opt.min_level || preferred_tag e.el_name) && max >= 1
          then Rand.int ~min:1 ~max () else
          (* case: this tree becomes too big, cut it *)
          if l > Opt.max_level && min <= 0 then 0
          else Rand.int ~min ~max () in
        let rec loop (acc : any) n attempts childs =
          if n > count
          then childs, acc
          else
            try
              let e_childs, e = make_element (l, counter+childs) e in
              let cdata = cdata () in
              loop {{ cdata @ acc @ [e] }} (n+1) 1 (childs+e_childs+1)
            with
              | Cannot_build ->
                  if attempts < 5
                  then loop acc n (succ attempts) childs
                  else if n > min
                  then childs, acc
                  else raise Cannot_build in
        loop {{ [] }} 1 1 0
    | Model (Choice   pl) ->
        let rec loop acc =
          let remove i l =
            List.rev (snd (List.fold_left (fun (n, acc) x ->
              if n = i then (succ n, acc)
              else (succ n, x :: acc)) (0, []) l)) in
          let n =
            (try
              (* try to choose preferred tag *)
              let n, _ =
                List.findi (fun _ p ->
                  match p.part_term with
                  | Elt e -> preferred_tag e.el_name
                  | _ -> false) acc in
                (* but give a small chance for another tags *)
                if Random.int 10 > 7 then raise Not_found else n
            with Not_found ->
              (try Random.int (List.length acc)
              with _ -> raise Cannot_build)) in
          let x = List.nth acc n in
          try make_particle (l, counter) x
          with Cannot_build -> loop (remove n acc) in
        loop pl
    | Model (All      pl) ->
        (* FIXME: I don't know what to do *)
        (* FIXME: add debug print *)
        0, {{ [] }}
    | Model (Sequence pl) ->
        xmlfold
          (fun (childs, (acc:any)) x ->
            let c, p = make_particle (l, childs+counter) x in
            let cdata = cdata () in
            childs+c, {{ cdata @ acc @ p }} ) (0, {{ [] }}) pl
    | Wildcard _ ->
        (* FIXME: not supported yet *)
        0, {{ [] }}

  and make_type_definition (l, counter) d : int * (Ocamlduce.Record.t * any ) =
    match d with
    | Simple sd -> 0, ({{ {} }}, make_simple_definition sd)
    | Complex d ->
        let make_content l content =
          match content with
          | CT_empty -> 0, {{ [] }}
          | CT_simple sd -> 0, make_simple_definition sd
          | CT_model (p, mixed) -> make_particle ~mixed (l, counter) p in
        let attrs =
          make_attribute_use d.ct_attrs in
        let childs, content = make_content l d.ct_content in
        match d.ct_deriv with
        | `Extension ->
            (*let c, (extra_attrs, def) = make_type_definition (l, counter+childs) d.ct_base in
            childs+c, ({{ attrs ++ extra_attrs }}, {{ def @ content }})*)
            childs, ({{ attrs }}, {{ content }})
        | `Restriction ->
            childs, (attrs, content)

  and make_element (l, counter) e : int * {{ Ocamlduce.Load.anyxml }} =
    let name = e.el_name in
    let name = if Opt.skip_namespaces then skip_namespace name else name in
    if ignore_tag name
    then raise Cannot_build
    else
      let childs, (attrs, content) =
        let l = succ l in
        match e.el_cstr with
          | Fixed s -> 0, ({{ {} }}, s)
          | Default _ -> 0, ({{ {} }}, failwith "default")
          | Free -> make_type_definition (l, succ counter) (Lazy.force e.el_typdef) in
      childs, {{ < (name) (attrs) >content }}

  let make_element e : {{ Ocamlduce.Load.anyxml }} =
    snd (make_element (1, 1) e)

end

