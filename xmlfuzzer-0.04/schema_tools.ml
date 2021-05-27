open Schema

module Facets = struct
  let none = {
    length = None; minLength = None; maxLength = None;
    pattern = None; enum = None; whitespace = None;
    maxInclusive = None; maxExclusive = None;
    minInclusive = None; minExclusive = None;
    totalDigits = None; fractionDigits = None
  }

  let or_pattern (v1 : {{String}}) = function
    | None -> Some v1
    | Some (v2 : {{String}}) -> Some {{ [ !v1 '|' !v2 ] }}

  let or_enum (v1 : {{String}}) = function
    | None -> Some [v1]
    | Some v2 -> Some (v1::v2)


  (* TODO: check validation of facet merge *)

  let merge_opt v1 v2 = match v1,v2 with
    | x, None -> x
    | _, y -> y

  let merge f1 f2 =
    let m = merge_opt in {
    length         = m f1.length         f2.length        ;
    minLength      = m f1.minLength      f2.minLength     ;
    maxLength      = m f1.maxLength      f2.maxLength     ;
    pattern        = m f1.pattern        f2.pattern       ; (* TODO: intersection *)
    enum           = m f1.enum           f2.enum          ; (* TODO: intersection *)
    whitespace     = m f1.whitespace     f2.whitespace    ;
    maxInclusive   = m f1.maxInclusive   f2.maxInclusive  ;
    minInclusive   = m f1.minInclusive   f2.minInclusive  ;
    maxExclusive   = m f1.maxExclusive   f2.maxExclusive  ;
    minExclusive   = m f1.minExclusive   f2.minExclusive  ;
    totalDigits    = m f1.totalDigits    f2.totalDigits   ;
    fractionDigits = m f1.fractionDigits f2.fractionDigits;
  }

end


{{ namespace xsd = "http://www.w3.org/2001/XMLSchema" }}


let xsd_ns = fst (Ocamlduce.Atom.get {{`xsd:foo}})


module QTable = Hashtbl.Make(Ocamlduce.Atom)

let builtins = QTable.create 256
let get_builtin = QTable.find builtins
let add_builtin n =
  match n.st_name with
    | Some x -> QTable.add builtins x n
    | None -> assert false

let xsd_name (name : string) = Ocamlduce.Atom.make (xsd_ns,{{ {:name:} }})

open Facets

let rec simple_ur_type =
  { st_name = Some {{ `xsd:anySimpleType }};
    st_base = simple_ur_type;
    st_facets = Facets.none;
    st_variety = Atomic "anySimpleType" }

let () = add_builtin simple_ur_type

let mk_restriction name base facets =
  let base = get_builtin (xsd_name base) in
  add_builtin
    { st_name = Some (xsd_name name);
      st_base = base;
      st_facets = Facets.merge base.st_facets facets;
      st_variety = base.st_variety }

let mk_list name base =
  let base = get_builtin (xsd_name base) in
  add_builtin
    { st_name = Some (xsd_name name);
      st_base = simple_ur_type;
      st_facets = none;
      st_variety = List base }


let mk_primitive name =
  add_builtin
    { st_name = Some (xsd_name name);
      st_base = simple_ur_type;
      st_facets = Facets.none;
      st_variety = Atomic name }

let () =
  List.iter mk_primitive [
    "string"; "boolean"; "base64Binary"; "hexBinary";
    "float"; "decimal"; "double"; "anyURI"; "QName";
    "NOTATION"; "duration"; "dateTime"; "time"; "date";
    "gYearMonth"; "gYear"; "gMonthDay"; "gDay"; "gMonth" ]


let () =
List.iter (function
       | (name,`R(base,facets)) -> mk_restriction name base facets
       | (name,`L base) -> mk_list name base) [
  "normalizedString",`R("string",none);
  "token",`R("normalizedString",none);
  "language",`R("token",none);
  "NMTOKEN",`R("token",none);
  "NMTOKENS",`L "NMTOKEN";
  "Name",`R("token",none);
  "NCName",`R("Name",none);
  "ID",`R("NCName",none);
  "IDREF",`R("NCName",none);
  "IDREFS",`L "IDREF";
  "ENTITY",`R("NCName",none);
  "ENTITIES",`L "ENTITY";
  "integer",`R("decimal",{ none with fractionDigits = Some 0 });
  "nonPositiveInteger",`R("integer",{none with
               maxInclusive = Some {{"0"}} });
  "negativeInteger",`R("nonPositiveInteger",{none with
                 maxInclusive = Some {{"-1"}} });
  "long",`R("integer",{none with
       maxInclusive = Some {{"9223372036854775807"}};
       minInclusive = Some {{"-9223372036854775808"}}});
  "int",`R("long",{none with
       maxInclusive = Some {{"2147483647"}};
       minInclusive = Some {{"-2147483648"}}});
  "short",`R("int",{none with
       maxInclusive = Some {{"32767"}};
       minInclusive = Some {{"-32768"}}});
  "byte",`R("short",{none with
       maxInclusive = Some {{"127"}};
       minInclusive = Some {{"-128"}}});
  "nonNegativeInteger",`R("integer",{none with minInclusive = Some {{"0"}} });
  "unsignedLong", `R("nonNegativeInteger",
         {none with maxInclusive =
       Some {{"18446744073709551615"}}});
  "unsignedInt",`R("unsignedLong",{none with
             maxInclusive = Some {{"4294967295"}}});
  "unsignedShort",`R("unsignedInt",{none with
              maxInclusive = Some {{"65535"}}});
  "unsignedByte",`R("unsignedShort",{none with
               maxInclusive = Some {{"255"}}});
  "positiveInteger",`R("nonNegativeInteger",{ none with
            minInclusive = Some {{"1"}} });
]



let wild_any_lax = { wild_cstr = WNot NsSet.empty; wild_process = `Lax }

let rec ur_type =
  { ct_uid = 0;
    ct_name =  Some {{ `xsd:anyType }};
    ct_base = Complex ur_type;
    ct_deriv = `Restriction;
    ct_content =
      CT_model
  ({ part_min = {{ 1 }};
     part_max = Some {{ 1 }};
     part_term = Model (Sequence [
        { part_min = {{ 0 }};
          part_max = None;
          part_term = Wildcard wild_any_lax } ]) },
   true);
    ct_attrs = [];
    ct_attr_wild = Some wild_any_lax;
    ct_abstract = false }



let intersect_wild w1 w2 =
  match w1,w2 with
    | None,w | w,None -> w
    | Some w1, Some w2 ->
  Some { w1 with wild_cstr =
      match w1.wild_cstr, w2.wild_cstr with
        | WOne l1, WOne l2 -> WOne (NsSet.inter l1 l2)
        | WNot l1, WNot l2 -> WNot (NsSet.union l1 l2)
        | WOne l1, WNot l2 | WNot l2, WOne l1 -> WOne (NsSet.diff l1 l2) }

(* TODO: check that intersection is expressible *)


module QNameSet = Set.Make(Ocamlduce.Atom)
exception DuplicateAttribute of qname

let check_duplicate_attribute attrs =
  ignore
    (List.fold_left
       (fun all a ->
    let n = a.at_decl.at_name in
    if QNameSet.mem n all then raise (DuplicateAttribute n)
    else QNameSet.add n all)
       QNameSet.empty
       attrs)
