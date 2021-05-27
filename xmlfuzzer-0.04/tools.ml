
let utf = Ocamlduce.Utf8.make

let xmlfold f s l =
  let rec fold f accu l =
    match l with
    | [] -> accu
    | a::t -> fold f (f accu a) t in
  fold f s l

let skip_namespace (atom : {{ Atom }}) : {{ Atom }}=
  let _, n = Ocamlduce.Atom.get atom in
  Ocamlduce.Atom.make (Ocamlduce.Namespace.empty, n)

