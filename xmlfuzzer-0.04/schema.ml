(* The internal OCaml representation of XML Schema. *)


module NsSet = Set.Make(Ocamlduce.Namespace)

type qname = {{ Atom }}
type ns = Ocamlduce.Namespace.t

type value_type =
  | Int
  | String

type facets = {
  length: int option;
  minLength: int option;
  maxLength: int option;
  pattern: {{ String }} option;
  enum: {{ String }} list option;
  whitespace: [ `Preserve | `Replace | `Collapse ] option;
  maxInclusive: {{ String }} option;
  maxExclusive: {{ String }} option;
  minInclusive: {{ String }} option;
  minExclusive: {{ String }} option;
  totalDigits: int option;
  fractionDigits: int option;
  (* Missing: fixed properties *)
}


type value_constraint =
  | Fixed of {{ String }}
  | Default of {{ String }}
  | Free

type wildcard_constraint =
  | WNot of NsSet.t (* More liberal than the spec. Any is an empty WNot. *)
  | WOne of NsSet.t

and wildcard = {
  wild_cstr: wildcard_constraint;
  wild_process: [`Lax | `Skip | `Strict];
}
    (* Missing: annotation *)

type simple_type_definition =
    { st_name: qname option;
      st_variety: variety;
      st_facets: facets;
      st_base: simple_type_definition;
    }
      (* Missing: fundamental facets, final, annotation *)

and variety =
  | Atomic of string
  | List of simple_type_definition
  | Union of simple_type_definition list

type attribute_declaration =
    { at_name: qname;
      at_typdef: simple_type_definition;
      at_cstr: value_constraint }
      (* Missing:
   annotation *)

and attribute_use =
    { at_required: bool;
      at_decl: attribute_declaration;
      at_use_cstr: value_constraint }

type term =
  | Elt of element_declaration
  | Model of model_group
  | Wildcard of wildcard

and model_group =
  | All of particle list
  | Choice of particle list
  | Sequence of particle list
      (* Missing: annotation *)

and content_type =
  | CT_empty
  | CT_simple of simple_type_definition
  | CT_model of particle * bool        (* mixed *)

and particle =
    { part_min: {{ 0--** }};
      part_max: {{ 0--** }} option;  (* None = unbounded *)
      part_term: term }

and element_declaration =
    { el_name: qname;
      el_typdef: type_definition Lazy.t;
      el_cstr: value_constraint;
      el_nillable: bool;
    }
      (* Missing:
   identity-constraint definitions,
   substitution group affiliation,
   substitution group exclusions,
   disallowed substitutions,
   abstract,
   annotation *)

and complex_type_definition =
    { ct_uid: int;
      ct_name: qname option;
      ct_base: type_definition;
      ct_deriv: [ `Extension | `Restriction ];
      ct_attrs: attribute_use list;
      ct_attr_wild: wildcard option;
      ct_content: content_type;
      ct_abstract: bool;
      (* Missing:
   final, abstract, prohibited substitutions, annotations *)
    }

and type_definition =
  | Simple of simple_type_definition
  | Complex of complex_type_definition

type model_group_definition =
    { mg_name : qname;
      mg_def : model_group }
      (* Missing: annotation *)

type attribute_group_definition =
    { ag_name : qname;
      ag_uses : attribute_use list;
      ag_wildcard : wildcard option }
      (* Missing: annotation *)

type notation_declaration =
    { not_name : qname;
      not_system : {{ String }} option;
      not_public : {{ String }} option }

type schema =
    { simple_types : simple_type_definition list;
      complex_types : complex_type_definition list;
      elements : element_declaration list;
      attributes : attribute_declaration list;
      attribute_groups : attribute_group_definition list;
      model_groups: model_group_definition list;
      notations: notation_declaration list;
      ns: ns
    }
      (* Missing: annotations *)

