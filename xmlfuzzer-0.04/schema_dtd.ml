(* The external XML representation of XML Schema.
   This file was obtained by dtd2types applied to the DTD for XML Schema
   and then made more precise and cleaner by hand. *)

{{ namespace "http://www.w3.org/2001/XMLSchema" }}

type formChoice = {{ "qualified" | "unqualified" }}
type boolean = {{ "true" | "false" | "0" | "1" }}
type nonNegInt = {{ ['0'--'9'+] }}
type minOccurs = nonNegInt
type maxOccurs = {{ minOccurs | "unbounded" }}
type use = {{ "prohibited" | "optional" | "required" }}
type whitespace = {{ "collapse" | "preserve" | "replace" }}

type default_fixed = {{ <_ default=Any fixed=Any ..>_ }}
type attr_constr = {{ <_ default=Any fixed=?Empty use=?"optional" ..>_
        | <_ default=?Empty ..>_ }}

type processContents = {{ "skip" | "lax" | "strict" }}

type schema =
    {{<schema
   xml:lang=?String
   attributeFormDefault=?formChoice
   elementFormDefault=?formChoice
   id=?String
   blockDefault=?String
   finalDefault=?String
   xmlns=?String
   version=?String
   targetNamespace=?String>
   [ (_include|import|redefine|ann)*
     (ann|toplevel_components)* ] }}

(* Toplevel components *)

and toplevel_items = {{ _include|import|redefine|ann|toplevel_components }}

and toplevel_components =
    {{ topLevelSimpleType | topLevelComplexType  | topLevelElement
     | topLevelAttribute  | topLevelAttributeGroup | topLevelGroup
     | topLevelNotation }}


and topLevelSimpleType =
    {{ simpleType & <_ name=String ..>_ }}

and topLevelComplexType =
    {{ complexType & <_ name=String ..>_ }}

and topLevelElement =
    {{ element_decl & <_ form=?Empty minOccurs=?Empty maxOccurs=?Empty ..>_ }}


and topLevelAttribute =
    {{ attribute_decl & <_  form=?Empty use=?Empty ..>_ }}

and topLevelAttributeGroup =
    {{ <attributeGroup ref=?Empty id=?String name=String>[
         ann? attribute_use* anyAttribute? ]}}

and topLevelGroup =
    {{ <group
   id=?String maxOccurs=?Empty minOccurs=?Empty
   ref=?Empty name=String>[ ann? ( all | choice | sequence ) ] }}

and topLevelNotation =
    {{ <notation system=?String public=String id=?String name=String>[
         ann? ]}}

(* References *)

and attribute_ref =
    {{<attribute ref=String name=?Empty id=?String
   fixed=?String default=?String
   use=?use form=?Empty type=?Empty>[ ann? ] & attr_constr }}

and element_ref =
    {{ <element ref=String name=?Empty id=?String
         form=?Empty fixed=?Empty default=?Empty block=?Empty
   final=?Empty abstract=?Empty substitutionGroup=?Empty
   nillable=?Empty type=?Empty
   maxOccurs=?maxOccurs minOccurs=?minOccurs>[ ann? ] - default_fixed }}

and group_ref =
    {{ <group ref=String name=?Empty id=?String
   maxOccurs=?maxOccurs minOccurs=?minOccurs>[ ann? ] }}

and attributeGroup_ref =
    {{ <attributeGroup ref=String id=?String name=?Empty>[
         ann? ]}}

(* Annotations *)

and documentation =
    {{ <documentation xml:lang=?String id=?String source=?String> [ Any* ]}}
and appinfo =
    {{ <appinfo id=?String source=?String>[ Any* ]}}
and ann =
    {{ <annotation>[ ( appinfo | documentation )* ]}}

(* Include and co *)

and _include =
    {{<include id=?String schemaLocation=String>[ ann? ]}}

and redefine =
    {{<redefine id=?String schemaLocation=String>
   [ (ann|topLevelSimpleType|topLevelComplexType|topLevelAttributeGroup|
    topLevelGroup)* ]}}

and import =
    {{<import id=?String schemaLocation=?String namespace=?String>
   [ ann? ]}}


(* Simple types *)

and simpleType =
    {{ <simpleType id=?String final=?String name=?String>
   [ ann? ( simple_restriction | simple_list | simple_union ) ]}}

and localSimpleType =
    {{ simpleType & <_ name=?Empty final=?Empty>_ }}

and simple_restriction =
    {{ <restriction id=?String base=String>[ ann? facet* ]
     | <restriction id=?String base=?Empty>[ ann? simpleType facet* ] }}

and simple_union =
    {{ <union memberTypes=?String id=?String>[ ann? simpleType* ] }}

and simple_list =
    {{ <list id=?String itemType=String>[ ann? ]
     | <list id=?String itemType=?Empty>[ ann? simpleType ] }}

(* Attributes *)

and attribute_decl =
    {{<attribute name=String ref=?Empty id=?String
   form=?formChoice
   fixed=?String default=?String
   use=?use
         type=?String>
       [ ann? localSimpleType? ]
       - <_ type=Any..>[ ann? localSimpleType ]
  & attr_constr }}


and attribute = {{ attribute_ref | attribute_decl }}
and attribute_use = {{ attribute | attributeGroup_ref }}

(* Complex types *)

and complexType =
    {{<complexType
   mixed=?boolean
   block=?String
   final=?String
   abstract=?boolean
   id=?String
   name=?String>
   [ ann?
           ( simpleContent | complexContent
           | (typedef_particle? attribute_use* anyAttribute?) )
         ]}}

and localComplexType =
    {{ complexType &
       <_ name=?Empty abstract=?Empty final=?Empty block=?Empty ..>_ }}

(* Complex content *)

and complexContent =
    {{ <complexContent id=?String mixed=?boolean>
   [ ann? ( complex_restriction | complex_extension ) ]}}

and complex_restriction =
    {{ <restriction id=?String base=String>[
   ann? typedef_particle? attribute_use* anyAttribute? ]}}

and complex_extension =
    {{ <extension id=?String base=String>[
         ann? typedef_particle? attribute_use* anyAttribute? ]}}

(* Simple content *)

and simpleContent =
    {{ <simpleContent id=?String>[
         ann? ( simple_content_restriction | simple_content_extension ) ]}}

and simple_content_restriction =
    {{ <restriction id=?String base=String>[
   ann? simpleType? facet* attribute_use* anyAttribute? ] }}

and simple_content_extension =
    {{ <extension id=?String base=String>[
         ann? attribute_use* anyAttribute? ] }}

(* Elements declarations *)

and element_decl =
    {{<element name=String ref=?Empty id=?String
   form=?formChoice
   fixed=?String default=?String
   block=?String final=?String
   abstract=?boolean nillable=?boolean
   substitutionGroup=?String
   maxOccurs=?maxOccurs minOccurs=?minOccurs
         type=?String
      >[ ann? (localComplexType|localSimpleType)? (unique|key|keyref)* ]
       - default_fixed }}

and local_element =
    {{ element_ref
     | element_decl &
       <_ substitutionGroup=?Empty final=?Empty abstract=?Empty ..>_ }}

(* Wildcards *)

and anyAttribute =
    {{ <anyAttribute id=?String processContents=?processContents
   namespace=?String>[ ann? ]}}

and any =
    {{ <any id=?String maxOccurs=?maxOccurs minOccurs=?minOccurs
   processContents=?processContents namespace=?String>[ ann? ]}}

(* Particles *)

and typedef_particle = {{ group_ref | all | choice | sequence }}
and nested_particle = {{ local_element | group_ref | choice | sequence | any }}
and particle = {{ typedef_particle | nested_particle }}

and sequence =
    {{ <sequence id=?String maxOccurs=?maxOccurs minOccurs=?minOccurs>[
         ann?  nested_particle* ]}}

and choice =
    {{ <choice id=?String maxOccurs=?maxOccurs minOccurs=?minOccurs>[
         ann? nested_particle* ] }}

and all =
    {{ <all id=?String maxOccurs=?("1") minOccurs=?("1")>[
   ann? local_element* ]}}

(* Facets *)

and facet =
  {{ minExclusive | minInclusive | maxExclusive | maxInclusive
   | totalDigits | fractionDigits | length | minLength | maxLength
   | enumeration | whiteSpace | pattern }}

and facet_value = {{ <_ fixed=?boolean id=?String value=String>[ ann? ]}}
and nonneg_facet  = {{ <_ fixed=?boolean id=?String value=nonNegInt>[ ann? ]}}

and pattern = {{<pattern id=?String value=String>[ann?]}}
and whiteSpace = {{<whiteSpace
          id=?String fixed=?boolean value=whitespace>[ ann? ] }}
and enumeration = {{<enumeration id=?String value=String>[ ann? ]}}
and maxLength = {{<maxLength ..>_ & nonneg_facet}}
and minLength = {{<minLength ..>_ & nonneg_facet}}
and length = {{<length..>_ & nonneg_facet}}
and fractionDigits = {{<fractionDigits ..>_ & nonneg_facet}}
and totalDigits = {{<totalDigits ..>_ & nonneg_facet}}
and minInclusive = {{<minInclusive ..>_ & facet_value}}
and maxInclusive = {{<maxInclusive ..>_ & facet_value}}
and minExclusive = {{<minExclusive ..>_ & facet_value}}
and maxExclusive = {{<maxExclusive ..>_ & facet_value}}

(* Others *)

and field = {{<field id=?String xpath=String>[ ann? ]}}
and selector = {{<selector id=?String xpath=String>[ ann? ]}}
and keyref =
    {{<keyref id=?String refer=String name=String>[ ann? selector field+ ]}}
and key = {{<key id=?String name=String>[ ann? selector field+ ]}}
and unique = {{<unique id=?String name=String>[ ann? selector field+ ]}}

