(* This is the XML Schema loader.
   It produces an OCaml structure from an XML tree. *)

open Schema
open Schema_tools
open Schema_dtd

let error s = failwith s

{{ namespace "http://www.w3.org/2001/XMLSchema" }}
{{ namespace xsd = "http://www.w3.org/2001/XMLSchema" }}

module QTable = Hashtbl.Make(Ocamlduce.Atom)

type 'a tbl = 'a Lazy.t QTable.t

type env = {
  e_attribute: attribute_declaration tbl;
  e_attribute_group: attribute_group_definition tbl;
  e_model_group: model_group_definition tbl;
  e_element: element_declaration tbl;
  e_simple_type: simple_type_definition tbl;
  e_complex_type: complex_type_definition tbl;
  e_notation: notation_declaration tbl;
  e_target_ns: ns;
  e_attributeFormDefault: bool;
  e_elementFormDefault: bool;

  include_uri: (string -> string -> string);
  import_uri: (string -> string -> string -> string option);
  loader: (string -> Schema_dtd.schema);
}

let qname = Ocamlduce.NamespaceScope.resolve_qname

let all_lazy = ref []
let reg_lazy l = all_lazy := (fun () -> ignore(Lazy.force l)) :: !all_lazy; l

let mk_tbl () = QTable.create 17

let insert_tbl t name e f = QTable.add t name (lazy (f e))

exception Cycle of QTable.key

let force_tbl t = QTable.fold (fun n l accu ->
         let l =
           try Lazy.force l
           with Lazy.Undefined ->  raise (Cycle n)
         in
         l :: accu) t []
let get_tbl_opt t x name = Lazy.force (QTable.find t (qname x name))
let get_tbl t x name =
  let d =
    try QTable.find t (qname x name)
    with Not_found ->
(*      QTable.iter (fun n _ -> Printf.eprintf "->%s\n" (Ocamlduce.to_string n)) t;
      Printf.eprintf "??%s\n" (Ocamlduce.to_string (qname x name)); *)
      failwith ("Cannot find schema component " ^ {: (name :? Latin1) :})
  in
  Lazy.force d

let get_element env = get_tbl env.e_element
let get_simple_type env x name =
  try get_builtin (qname x name)
  with Not_found -> get_tbl env.e_simple_type x name

let get_attribute env = get_tbl env.e_attribute
let get_attribute_group env = get_tbl env.e_attribute_group
let get_type env x name =
  try Complex (get_tbl_opt env.e_complex_type x name)
  with Not_found -> Simple (get_simple_type env x name)
let get_model_group env = get_tbl env.e_model_group

(* Helper functions to parse attributes *)

let name ns e = Ocamlduce.Atom.make (ns, {{ e.name }})

let gl_name env e = name env.e_target_ns e

let local_name form_default env e =
  let q = match {{ e.? form }} with
    | {{ [ "qualified" ] }} -> true
    | {{ [ "unqualified" ] }} -> false
    | {{ [ ] }} -> form_default in
  name (if q then env.e_target_ns else Ocamlduce.Namespace.empty) e

let opt_name env = function
  | {{ <_ name=Any ..>_ & x }} -> Some (gl_name env x)
  | {{ _ }} -> None

let parse_bool = function
  | {{ "true" | "1" }} -> true
  | {{ "false" | "0" }} -> false

let parse_opt_bool = function
  | {{ [x] }} -> parse_bool x
  | {{ [] }} -> false

let parse_opt_qualified = function
  | {{ [ "qualified" ] }} -> true
  | {{ [ "unqualified"? ] }} -> false

let parse_opt_str = function {{ [s] }} -> Some s | {{[]}} -> None

let rec split (l : {{String}}) =
  match l with
    | {{ [ ' '* x::(Char - ' ')+ rest::_* ] }} -> x :: (split rest)
    | {{ [ ' '* ] }} -> []

let parse_value_constraint = function
  | {{ <_ default fixed=?Empty ..>_ }} -> Default default
  | {{ <_ fixed default=?Empty ..>_ }} -> Fixed fixed
  | {{ <_ default=?Empty fixed=?Empty ..>_ }} -> Free

let mk_nsset = List.fold_left (fun acc x -> NsSet.add x acc) NsSet.empty

let parse_processContents = function
 | {{ [ "skip" ] }} -> `Skip
 | {{ [ "lax" ] }} -> `Lax
 | {{ [ "strict"? ] }} -> `Strict

let parse_namespace_cstr env = function
  | {{ [ "##any"? ] }} -> WNot NsSet.empty
  | {{ [ "##other" ] }} ->
      WNot (mk_nsset [ Ocamlduce.Namespace.empty; env.e_target_ns ])
  | {{ [ n ] }} ->
      WOne (
  mk_nsset (
    List.map
      (function
         | {{ "##targetNamespace" }} -> env.e_target_ns
         | {{ "##local" }} -> Ocamlduce.Namespace.empty
         | {{ s }} -> Ocamlduce.Namespace.make s)
      (split n)))

(* Simple types *)

let nonneg_facet = function
    {{ <_ value=v ..>_ }} -> Some {: Ocamlduce.NonnegInt.make v :}

let parse_ws = function
  | {{"collapse"}} -> `Collapse
  | {{"preserve"}} -> `Preserve
  | {{"replace"}} -> `Replace

open Facets
let parse_facet f (l : facet) = match l with
  | {{ <length..>_ & l }} -> { f with length = nonneg_facet l }
  | {{ <minLength..>_ & l }} -> { f with minLength = nonneg_facet l }
  | {{ <maxLength..>_ & l }} -> { f with maxLength = nonneg_facet l }
  | {{ <pattern value=v..>_ }} -> { f with pattern = or_pattern v f.pattern }
  | {{ <enumeration value=v..>_ }} -> { f with enum = or_enum v f.enum }
  | {{ <whiteSpace value=v..>_ }} -> { f with whitespace = Some (parse_ws v) }
  | {{ <maxInclusive value=v..>_ }} -> { f with maxInclusive = Some v }
  | {{ <minInclusive value=v..>_ }} -> { f with maxInclusive = Some v }
  | {{ <maxExclusive value=v..>_ }} -> { f with maxInclusive = Some v }
  | {{ <minExclusive value=v..>_ }} -> { f with maxInclusive = Some v }
  | {{ <totalDigits..>_ & l }} -> { f with totalDigits = nonneg_facet l }
  | {{ <fractionDigits..>_ & l }} -> { f with fractionDigits = nonneg_facet l }

let parse_facets f = List.fold_left parse_facet Facets.none {:f:}

let simple_restriction env x base f =
  { st_name = opt_name env x;
    st_variety = base.st_variety;
    st_facets = Facets.merge base.st_facets (parse_facets f);
    st_base = base }

let simple_list env x itemType =
  { st_name = opt_name env x;
    st_variety = List itemType;
    st_facets = Facets.none;
    st_base = simple_ur_type }

let simple_union env x memberTypes =
  { st_name = opt_name env x;
    st_variety = Union memberTypes;
    st_facets = Facets.none;
    st_base = simple_ur_type }

let rec parse_simple_type env (x : simpleType) =
  let {{ <_ ..>[ ann? y ] }} = x in
  match y with
    | {{ <restriction base ..>[ ann? f::facet*] }} ->
  simple_restriction env x (get_simple_type env x base) f
    | {{ <restriction ..>[ ann? base f::_*] }} ->
  simple_restriction env x (parse_simple_type env base) f
    | {{ <list itemType ..>_ }} ->
  simple_list env x (get_simple_type env x itemType)
    | {{ <list ..>[ ann? (item & simpleType) ] }} ->
  simple_list env x (parse_simple_type env item)
    | {{ <union memberTypes=m!(m := []) ..>[ ann? s::_* ] }} ->
  let members1 = List.map (get_simple_type env x) (split m) in
  let members2 = List.map (parse_simple_type env) {:s:} in
  simple_union env x (members1 @ members2)

let find_simple_type env (e : attribute_decl) =
  match e with
    | {{ <_ type=ty ..>[ ann? ] }} -> get_simple_type env e ty
    | {{ <_ type=?Empty ..>[ ann? ] }} -> simple_ur_type
    | {{ <_ type=?Empty ..>[ ann? x ] }} -> parse_simple_type env x

(* Attributes *)

let parse_tl_att env e =
  { at_name = gl_name env e;
    at_typdef = find_simple_type env e;
    at_cstr = parse_value_constraint e }

let parse_att env e =
  { at_name = local_name env.e_attributeFormDefault env e;
    at_typdef = find_simple_type env e;
    at_cstr = Free }

(* Wildcards *)

let parse_wildcard env e =
  { wild_process = parse_processContents {{ e.? processContents }};
    wild_cstr = parse_namespace_cstr env {{ e.? namespace }} }

let parse_wildcard_opt env = function
  | {{ [ e ] }} -> Some (parse_wildcard env e)
  | {{ [] }} -> None

(* Attribute uses *)

let parse_att_use env accu (e : {{attribute}}) =
  let use = {{ e.? use }} in
  match use with
    | {{ [ "prohibited" ] }} -> accu
    | {{ _ }} ->
  let req = match use with {{ ["required"] }} -> true | {{_}} -> false in
  let cstr = parse_value_constraint e in
  let decl = match e with
    | {{ <_ ref=x ..>_ }} -> get_attribute env e x
    | {{ e }} -> parse_att env e in
  { at_required = req;
    at_decl = decl;
    at_use_cstr = cstr } :: accu

let att_name env (e : attribute) =
  match e with
    | {{ <_ ref=x ..>_ }} -> qname e x (* (get_attribute env e x).at_name *)
    | {{ e }} -> local_name env.e_attributeFormDefault env e


let find_attr_group env (ag : attributeGroup_ref) =
  get_attribute_group env ag {{ ag.ref }}

let parse_att_uses env = function
  {{ [(ag::<attributeGroup..>_|at::attribute|w::anyAttribute|ann)*] }} ->
  let ag = List.map (find_attr_group env) {: ag :} in
  let attrs =
    List.fold_left (parse_att_use env)
      (List.flatten (List.map (fun ag -> ag . ag_uses) ag))
      {: at :}
  in
  let wild =
    List.fold_left intersect_wild (parse_wildcard_opt env w)
      (List.map (fun ag -> ag.ag_wildcard) ag) in
  attrs, wild

let parse_att_uses_prohibited env = function
    {{[ (a::<attribute use="prohibited" ..>_ | _)* ]}} ->
      List.map (att_name env) {:a:}

let attr_use_name u = u.at_decl.at_name

(* Simple content *)

let get_base env e = get_type env e {{ e.base }}

let get_complex = function
  | Complex ct -> ct
  | Simple _ -> error "base must refer to a complex type definition"

let parse_attrs_restriction env ct at =
  let attrs,wild = parse_att_uses env at in
  let attrs_base =
    let remove = QTable.create 17 in
    let add x = QTable.add remove x () in
    List.iter add (parse_att_uses_prohibited env at);
    List.iter (fun a -> add (attr_use_name a)) attrs;
    List.filter
      (fun a -> not (QTable.mem remove (attr_use_name a)))
      ct.ct_attrs in
  attrs @ attrs_base, wild

let parse_simple_content_restriction env e = match e with
 {{ <_ ..>[ ann?
          ((s & simpleType) | (/ (s:=`None)))
          f::facet*
          at::(attribute|<attributeGroup..>_|anyAttribute)* ] }} ->
   let base = get_base env e in
   let ct = get_complex base in
   let attrs,wild = parse_attrs_restriction env ct at in
   let st =
     match ct.ct_content with
       | CT_empty ->
     error "simple content restriction whose base type is a complex type with empty content model"
       | CT_simple st ->
     (match s with {{ `None }} -> st | {{s}} -> parse_simple_type env s)
       | CT_model (part,true) ->
     (* TODO: check part is nillable *)
     (match s with
       | {{ `None }} ->  error "simple content restriction whose base type is a complex type with mixed content model, but without a simpleType"
       | {{s}} -> parse_simple_type env s)
       | CT_model (part,false) ->
     error "simple content restriction whose base type is a complex type with element-only content model"
   in
   let content = CT_simple (simple_restriction env e st f) in
   base,`Restriction,attrs,wild,content

let parse_simple_content_extension env e =
   let base = get_base env e in
   let attrs,wild = parse_att_uses env {{ [e]/ }} in
   let attrs_base =
     match base with Complex ct -> ct.ct_attrs | Simple _ -> [] in
   let st = match base with
     | Complex { ct_content = CT_simple st } -> st
     | Simple st -> st
     | _ -> error "simple content extension"
   in
   let content = CT_simple st in
   base,`Extension,attrs @ attrs_base, wild, content

(* Complex types, elements, complex content *)

let effective_mixed e = match e with
  | {{ <_ ..>[ann? <complexContent mixed ..>_ ]|<_ mixed ..>_}} ->
      parse_bool mixed
      (* First match matters above ! *)
  | {{ _ }} -> false

let empty_mixed =
  Some { part_min = {{1}}; part_max = Some {{1}};
   part_term = Model (Sequence [])}

let mk_particle e term =
  { part_min =
      (match {{ e.? minOccurs }} with
   | {{ [ n ] }} -> Ocamlduce.NonnegInt.make n
   | {{ [] }} -> {{1}});
    part_max =
      (match {{ e.? maxOccurs }} with
   | {{ [ "unbounded" ] }} -> None
   | {{ [ n ] }} -> Some (Ocamlduce.NonnegInt.make n)
   | {{ []  }} -> Some {{1}});
    part_term = term }
    (* TODO: in min=max=0 -> no particle ! *)

let mk_part_model e m = mk_particle e (Model m)

let rec parse_element_decl env top e =
  { el_name =
      (if top then gl_name else local_name env.e_elementFormDefault) env e;
    el_typdef =
      reg_lazy (lazy
      (match e with
         | {{<_..>[ ann? (d & localSimpleType) _* ]}} ->
       Simple (parse_simple_type env d)
         | {{<_..>[ ann? (d & localComplexType) _* ]}} ->
       Complex (parse_complex_type env d)
         | {{<_ type=ty ..>_}} -> get_type env e ty
       (* Missing case for substitutionGroup *)
         | {{_}} -> Complex ur_type));
    el_cstr = parse_value_constraint e;
    el_nillable = parse_opt_bool {{ e.? nillable }};
  }

and parse_tl_elt env e = parse_element_decl env true e

and parse_particles env parts =
  List.map (parse_particle env) {: parts :}

and parse_local_element env e = match e with
  | {{ <_ ref=x ..>_ }} -> get_element env e x
  | {{ e }} -> parse_element_decl env false e

and parse_mg env e = match e with
  | {{ <sequence ..>[ ann? parts::nested_particle* ] }} ->
      Sequence (parse_particles env parts)
  | {{ <choice ..>[ ann? parts::nested_particle* ] }} ->
      Choice (parse_particles env parts)
  | {{ <all ..>[ ann? parts::nested_particle* ] }} ->
      All (parse_particles env parts)

and parse_particle env (e : particle) = match e with
  | {{ <group ref=x ..>_ }} ->
      mk_part_model e (get_model_group env e x).mg_def
  | {{ local_element & e }} ->
      mk_particle e (Elt (parse_local_element env e))
  | {{ any }} ->
      mk_particle e (Wildcard (parse_wildcard env e))
  | {{ x }} -> mk_part_model e (parse_mg env x)

and effective_content env mixed = function
  | {{ [ (<all ..>[ ann? ] | <sequence ..>[ ann? ] |
      <choice minOccurs="0" ..>[ ann? ])? ] }} ->
      if mixed then empty_mixed else None
  | {{ [ r ] }} -> Some (parse_particle env r)

and parse_complex_restriction env base mixed = function
  | {{ [ r::typedef_particle?
   at::(attribute|<attributeGroup..>_|anyAttribute)* ] }} ->
      let attrs,wild = parse_attrs_restriction env base at in
      let content =
  match effective_content env mixed r with
    | None -> CT_empty
    | Some p -> CT_model (p,mixed)
      in
      Complex base,`Restriction,attrs,wild,content

and parse_complex_content_restriction env mixed e = match e with
  | {{ <_ ..>[ ann? r::_* ] }} ->
      parse_complex_restriction env (get_complex (get_base env e)) mixed r

and parse_complex_content_extension env mixed e =
  let {{ <_ ..>[(r::typedef_particle | at::_)*] }} = e in
  let base = get_base env e in
  let ct = get_complex base in
  let attrs,wild = parse_att_uses env at in
  let attrs = ct.ct_attrs @ attrs in
  let content =
    match effective_content env mixed r, ct.ct_content with
      | None, c -> c
      | Some p, CT_empty -> CT_model (p,mixed)
      | Some p1, CT_model (p2,_) ->
    CT_model ({part_min = {{1}}; part_max = Some {{1}};
         part_term = Model (Sequence [p1;p2])} ,mixed)
      | Some p, CT_simple _ -> assert false
    (* The spec does not cover this case !! *)
  in
  base,`Restriction,attrs,wild,content

and parse_complex_type env e =
  let abstract = parse_opt_bool {{ e.? abstract }} in
  let base,deriv,attrs,attr_wild,content = match e with
    | {{<_ ..>[ ann? <simpleContent..>[ ann? (r & <restriction..>_)]]}} ->
       parse_simple_content_restriction env r
    | {{<_ ..>[ ann? <simpleContent..>[ ann? (r & <extension..>_)]]}} ->
  parse_simple_content_extension env r
    | {{<_ ..>[ ann? <complexContent..>[ ann? (r & <restriction..>_)]]}} ->
  parse_complex_content_restriction env (effective_mixed e) r
    | {{<_ ..>[ ann? <complexContent..>[ ann? (r & <extension..>_)]]}} ->
  parse_complex_content_extension env (effective_mixed e) r
    | {{<_ ..>[ ann? r::_* ] }} ->
  parse_complex_restriction env ur_type (effective_mixed e) r
  in
  (try check_duplicate_attribute attrs
   with DuplicateAttribute a ->
     error ("Attribute name " ^ (Ocamlduce.to_string a) ^
        " appears several times in this attribute use list"));
  { ct_uid = 0;
    ct_name = opt_name env e;
    ct_base = base;
    ct_deriv = deriv;
    ct_attrs = attrs;
    ct_attr_wild = attr_wild;
    ct_content = content;
    ct_abstract = abstract }

(* Other toplevel components *)

let parse_notation env e =
  { not_name = gl_name env e;
    not_system = parse_opt_str {{ e.?system }};
    not_public = Some {{ e.public }} }

let parse_model_gp env = function {{ <_ ..>[ ann? x ] & e }} ->
  { mg_name = gl_name env e; mg_def = parse_mg env x }

let parse_att_gp env e =
  let attrs,wild = parse_att_uses env {{ [e]/.(Any - ann) }} in
  { ag_name = gl_name env e;
    ag_uses = attrs;
    ag_wildcard = wild }

(* Main parsing *)

let register env (x : toplevel_components) =
  let reg tbl f y = insert_tbl tbl (gl_name env x) y (f env) in
  match x with
  | {{ <attribute ..>_ & x }} -> reg env.e_attribute parse_tl_att x
  | {{ <element ..>_ & x }} -> reg env.e_element parse_tl_elt x
  | {{ <simpleType ..>_ & x }} -> reg env.e_simple_type parse_simple_type x
  | {{ <complexType ..>_ & x }} -> reg env.e_complex_type parse_complex_type x
  | {{ <attributeGroup ..>_ & x }} -> reg env.e_attribute_group parse_att_gp x
  | {{ <group ..>_ & x }} -> reg env.e_model_group parse_model_gp x
  | {{ <notation ..>_ & x }} -> reg env.e_notation parse_notation x

let rec handle uri env (x : toplevel_items) =
  match x with
  | {{ toplevel_components & x }} -> register env x
  | {{ ann }} -> ()
  | {{ <import schemaLocation=l!(l:="") namespace=n!(n:="")..>_ }} ->
      let uri = env.import_uri uri {: (l :? Latin1) :} {: (n :? Latin1) :} in
      (match uri with
   | None ->
(*       Printf.eprintf "Import ignored\n"; *)
       ()
   | Some uri ->
(*       Printf.eprintf "Import %s\n" uri; *)
       let e = env.loader uri in
       let env = {
         env with
     e_target_ns =
     (match {{ e .? targetNamespace }} with
        | {{ [x] }} -> Ocamlduce.Namespace.make x
        | {{ [] }} -> Ocamlduce.Namespace.empty);
     e_attributeFormDefault =
     parse_opt_qualified {{ e.?attributeFormDefault }};
     e_elementFormDefault =
     parse_opt_qualified {{ e.?elementFormDefault }} } in
       handle_all uri env (env.loader uri))
  | {{ <redefine ..>_ }} -> failwith "redefine not implemented"
  | {{ <include schemaLocation=l ..>_ }} ->
      let uri = env.include_uri uri {: (l :? Latin1) :} in
(*      Printf.eprintf "Include %s\n" uri; *)
      let e = env.loader uri in
      handle_all uri env e


and handle_all uri env e =
  List.iter (handle uri env) {: [e]/ :}

let parse_schema ~include_uri ~import_uri ~load uri =
  all_lazy := [];
  let e = load uri in
  let env = {
    e_target_ns =
      (match {{ e .? targetNamespace }} with
   | {{ [x] }} -> Ocamlduce.Namespace.make x
   | {{ [] }} -> Ocamlduce.Namespace.empty);
    e_attributeFormDefault = parse_opt_qualified {{ e.?attributeFormDefault }};
    e_elementFormDefault = parse_opt_qualified {{ e.?elementFormDefault }};
    e_attribute = mk_tbl ();
    e_attribute_group = mk_tbl ();
    e_element = mk_tbl ();
    e_simple_type = mk_tbl ();
    e_complex_type = mk_tbl ();
    e_model_group = mk_tbl ();
    e_notation = mk_tbl ();
    loader = load;
    include_uri = include_uri;
    import_uri = import_uri;
  } in
  handle_all uri env e;
  let r = {
    simple_types = force_tbl env.e_simple_type;
    attributes = force_tbl env.e_attribute;
    attribute_groups = force_tbl env.e_attribute_group;
    elements = force_tbl env.e_element;
    complex_types = force_tbl env.e_complex_type;
    model_groups = force_tbl env.e_model_group;
    notations = force_tbl env.e_notation;
    ns = env.e_target_ns;
  } in
  while !all_lazy <> [] do
    let l = !all_lazy in
    all_lazy := [];
    List.iter (fun f -> f ()) l;
  done;
  r
