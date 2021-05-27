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
open Big_int
open Printf

let max_big_int =
  let m = big_int_of_int64 Int64.max_int in
  mult_int_big_int 5 m
let min_big_int =
  let m = big_int_of_int64 Int64.max_int in
  mult_int_big_int (-5) m
let max_string_length = 65550

type 'a probability =
  (int * (unit -> 'a))

let some (prob: 'a probability list) : 'a =
  let base = ref 0 in
  let map =
    List.map (fun (i, f) ->
      let r = ((!base, i + !base), f) in
      base := !base + i; r) prob in
  let rand = Random.int !base in
  let _, f =
    try List.find (fun ((min, max), _) -> rand >= min && rand < max) map
    with Not_found -> assert false in
  f ()

let choose l =
  List.nth l (Random.int (List.length l))

(* FIXME: slow and ugly *)
let big_int ?(min=zero_big_int) ?(max=max_big_int) () =
  let init_len = 20 in
  (* just a big random number *)
  let initial =
    let buf = Buffer.create init_len in
    let s =
      let rec loop n =
        if n > init_len
        then Buffer.contents buf
        else (Buffer.add_char buf (char_of_int ((48 + Random.int 10))); loop (succ n)) in
      (loop 1) in
    big_int_of_string s in
  add_big_int min (mod_big_int initial (sub_big_int max min))

(** Unsigned int. Maximum for [max] is [max_int-1]! *)
let uint ?(min=0) ?(max=(max_int-1)) () =
  min + (Random.int (max - min + 1))

(** Signed int. *)
let int ?(min=(-1073741824)) ?(max=1073741823) () =
  let min = Int64.of_int min in
  let max = Int64.of_int max in
  let i = Int64.add min (Random.int64 (Int64.succ (Int64.sub max min))) in
  Int64.to_int i

let str ?(from_char=' ') ?(incl_char='~') l =
  let buf = Buffer.create l in
  let rec loop n =
    if n > l
    then Buffer.contents buf
    else (
      let min = int_of_char from_char in
      let max = int_of_char incl_char in
      let c = char_of_int (min + Random.int (max - min + 1)) in
      Buffer.add_char buf c;
      loop (succ n)) in
  loop 1

let the_big_string =
  Lazy.lazy_from_fun (fun () -> str max_string_length)

module Xml = struct

  let big_int ?min ?max () =
    let i = big_int ?min ?max () in
    let s = string_of_big_int i in
    {{ {: s :} }}

  let int ?min ?max () =
    let i = int ?min ?max () in
    let s = string_of_int i in
    {{ {: s :} }}

  let str l =
    let s =
      try
        let salt = Random.int (String.length (Lazy.force the_big_string) - l + 1) in
        String.sub (Lazy.force the_big_string) salt l
      with _ -> str l in
    {{ {: s :} }}

  let rand_len_str ?(min=0) ?(max=max_string_length) () =
    let len = min + (Random.int (max - min + 1)) in
    str len

  let integer f l =
    let s = match f.totalDigits, f.fractionDigits with
      | Some total, Some fraction ->
          sprintf "%*.*d" total fraction l
      | Some total, None ->
          sprintf "%*d" total l
      | None, Some fraction ->
          sprintf "%.*d" fraction l
      | None, None ->
          sprintf "%d" l in
    {{ {: s :} }}

  let hex     l = {{ {: Printf.sprintf "%x" l :} }} (* FIXME *)

end

let random_string  =  1, Xml.rand_len_str ?min:None ?max:None
let short_string   = 10, Xml.rand_len_str ~min:1 ~max:51
let medium_string  = 10, Xml.rand_len_str ~min:1 ~max:261
let empty_string   = 10, fun () -> let s = "" in {{ {: s :} }}
let long_string    =  1, fun () -> Xml.str max_string_length

let long_int  = 10, Xml.big_int ?min:None ?max:None
let rand_int  = 10, Xml.int     ?min:None ?max:None
let minus_one = 10, fun () -> let s = "-1" in {{ {: s :} }}
let zero      = 10, fun () -> let s = "0"  in {{ {: s :} }}

let value f =

  (* case: enum *)
  match f.enum with
  | Some l -> choose l
  | None -> (

  (* case: string with fixed length *)
  match f.length with
  | Some i -> Xml.str i
  | None -> (

  (* case: string with given min/max length *)
  match f.minLength, f.maxLength with
  | Some _, _ | _, Some _ ->
      Xml.rand_len_str ?min:f.minLength ?max:f.maxLength ()
  | None, None -> (

  (* case: something terrible, fix it *)
  match f.maxInclusive
      , f.maxExclusive
      , f.minInclusive
      , f.minExclusive
      , f.totalDigits
      , f.fractionDigits with
  | None, None, None, None, None, None ->
      some [
        random_string;
        empty_string;
        short_string;
        medium_string;
        long_string;
      ]
  | _ ->
      (*let to_big_int = function
        | Some x -> Some (big_int_of_string (Ocamlduce.Utf8.get x))
        | None -> None in
      let maxInclusive = to_big_int f.maxInclusive in
      let maxExclusive = to_big_int f.maxExclusive in
      let minInclusive = to_big_int f.minInclusive in
      let minExclusive = to_big_int f.minExclusive in*)
  (*let i = sprintf "%Ld" (Random.int64 (Int64.max_int)) in
  {{ {: i :} }} )))*)
  some [
    long_int;
    rand_int;
    minus_one;
    zero;
  ] )))

