# Typeful PPX and Value Implicits

## Abstract

This talk presents Typeful PPX, a novel PPX preprocessing technique with types.
Type dependent preprocessing is fairly easy once PPX is combined with
the compiler type checker.

`ppx_implicits` is presented as a demonstration of Typeful PPX,
which provides type dependent implicit values, combining it with
the optional parameters and the first class module values of OCaml,
it is easy for a PPX to have the same functionality as Modular Implicits
and type classes.

## Typeful PPX

### PPX + typing = Typeful PPX

CamlP4 and PPX are preprocessing frameworks of OCaml and they are now widely
used to provide new functionalities to
the language. For example, syntax extension (ex. `pa_monad` and `ppx_moandic`)
and automatic code generation (ex. `deriving` and `type_conv`).

They are extremely useful in the real world programming, but untyped:
both CamlP4 and PPX are preprocessors which work over `Parsetree`,
not-yet-typed ASTs. Even if someone gets a tiny but nifty idea of
type dependent program transformation, it was almost out of the scope
of CamlP4 and PPX and it had to be implemented as a compiler modification.
Compiler modification is a very subtle to be done correctly,
especially if it involves with typing. Distributing and installing
compiler modifications are also hard: even with the help of OPAM's
`opam switch`, many OCaml users do not consider to invest their time
to try your modifications. 

*Typeful PPX* is a technique to overcome this difficulty of OCaml language
enhancement with types. It does not preprocess the input, untyped AST
of `Parsetree` directly, but type-check it firstly and works on
the typed AST of `Typedtree` to make use of type annotations. 
Once the preprocessing of the typed AST is done, it untypes the result
to an untyped AST as the final output. From the point of view of
the host compiler which invokes a typeful PPX, it is just another ordinary
but rather complicated PPX which transforms untyped ASTs.

### Pros and Cons

Typeful PPX has the following benefits compared with the direct compiler
modification.

Safe. The output of Typeful PPX is again type checked by the compiler.
Critical bugs in Typeful PPX should be found by this second type check.
In the direct compiler modification, bugs in the typing layer tend to
make the type system unsafe and they are hard to detect and fix.
This should also make users feel much easier to try new functionalities
via Typeful PPX than the compiler patching. If still unsure, users can always
print out the final output and verify what Typeful PPX actually does. 

Easy to distribute, install and use. A Typeful PPX is just a PPX.
Typeful PPXs are easily installable via the packaging system and
users can use their functions with their vanilla OCaml compiler immediately.

Easy future integration. Implemented as a transformer of `Typedtree`,
future integration of Typeful PPXs into the real compiler modification
can reuse the much of their code.

Unfortunately it has some drawbacks too:

Up-to typing layer: this is a typeful program transformation
and therefore cannot change the lower details like code generation.

Not working with toplevel (REPL): PPX preprocessing works
against each compilation unit, which is one toplevel expression in OCaml toplevel.
Typeful PPX usually must keep various type informations
across toplevel expressions therefore does not work in OCaml toplevel.

### Technical details

Typeful PPX requires to integrate the OCaml compiler type checker to PPX
but the necessary tools are already available as OCaml's compiler API library
`compiler-libs`. Just slight modification of `driver/compiler.ml` is required
to wire up the inputs and outputs of PPX, type checker
and the typeful program transformation.

For typeful program transformation, `compiler-libs` already includes `TypedtreeMap`, a module for an easy interface to build a mapper.
Program transformation does not necessarily produce well-typed AST as its result, since it is get untyped. Untyping is also ready at `tools/untypeast.ml` of OCaml compiler source.

## Ppx_implicits

`Ppx_implicits` (https://bitbucket.org/camlspotter/ppx_typeclass,
it was initially called `ppx_typeclass`) is an example of Typeful PPX,
which provides type dependent *implicit values*. It is also extended to
have *implicit parameters*, *Modular Implicits* and *type class* like features
as a PPX.

### Simple implicit values `[%imp M]`

The simplest feature of `ppx_implicits` is type dependent implicit values,
values which are constructed automatically by PPX combining some recipe values.
For example, an annotated expression `e [@imp M]` is replaced by
an expression of the same type of `e`, using the values (recipes) available under
module `M`. For example, here is a simple overloading of plus operators:

```ocaml
module Show = struct
  let int   = Pervasives.string_of_int
  let float = Pervasives.string_of_float
end

let () = assert ( (assert false)[@imp Plus] 1 = "1" )
                  (* replaced by Show.int *)

let () = assert ( (assert false)[@imp Plus] 1.2 = "1.2" )
                  (* replaced by Show.float *)
```

`Ppx_implicits` overload resolution is based on simple type unification.
It tries to unify the type of implicit values and the type of each recipe.
If the unification fails, the recipe is discarded. If there is only one
match, the matched recipe is used for the replacement. Otherwise,
if none, `ppx_implicits` fails due to no possible instance. If there are
multiple matches, it also fails due to the ambiguity.

`(assert false)[@imp M]` is often used to generate a value which
matches with its typing context. `ppx_implicits` has a sugar
`[%imp M]` for `(assert false)[@imp M]`.

Recipes can be combined recursively:

```ocaml
module Show2 = struct
  include Show
  let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* Label starts with '_' has a special meaning in recipes:
     The arguments are generated recursively from the recipe sets. *)
end

let () = assert ( [%imp Show2] [1;2] = "[ 1; 2 ]" )
                  (* replaced by Show2.list ~_x:Show2.int *)
```

Recipe dependency (or constraint using type class terminology) is expressed
having a special label names start with `_` to function arguments.

We can define `show` function at this level but it still requires explicit
dispatching of an implicit value:

```ocaml
let show (imp : 'a -> string) x = imp x
let () = assert ( show [%imp Show2] [1; 2] = "[ 1; 2 ]" )
```

### Implicit argument using optional argument

To have the real overloading and write the above example as
simply `show [1; 2]`, the dispatching arguments of implicit values
should be omittable.  We can make use of OCaml's optional arguments
for this purpose:

```ocaml
let unpack = function
  | None -> assert false (* called when overloadings are not resolved *)
  | Some x -> x
let show ?imp x = unpack imp x
let () = assert ( show [1; 2] = "[ 1; 2 ]" )
```

The expression `show [1; 2]` is equivalent with `show ?imp:None [1; 2]`,
and it must be transformed to `show ~imp:[%imp Show2] [1; 2]`
by `ppx_implicits`.  But without omitting `[%imp Show2]`, how can we tell
an optional argument is for the implicit value dispatch
for some recipes `Show2`?
It requires one more trick: we transfer the information to the type of
the optional argument:

```ocaml
module Show3 = struct
  type 'a __imp__ = Packed of ('a -> string)
  let pack ~_x = Some (Packed _x)
  let unpack = function
    | None -> assert false (* overloading was not resolved *)
    | Some (Packed x) -> x
  include Show2
end

(* val show : ?imp:'a Show3.__imp__ -> 'a -> string *)
let show ?imp x = Show3.unpack imp x
```

Now `show` takes a value of `'a Show3.__imp__` as an optional argument.
Adding `ppx_implicits` another rule to transform function arguments of
the form `?l:None` where `None` has type `_ PATH.__imp__ option` to
`?l:[%imp PATH]`, `show [1; 2]` is properly transformed to
the following expression:

```ocaml
show ?imp:Show3.(pack ~_x:(list ~_x:int)) [1; 2]
```

### Recipe search space configuration

(This part is just roughly implemented in the current implementation.)

We have seen the simplest recipe search policy of `ppx_implicits`:
`[%imp PATH]`, the values defined inside a specific module path `PATH`.
It can support other recipe search policies easily,
since the implicit resolution algorithm itself is almost orthogonal
to the definitions of recipe search spaces.

For example, it is not difficult to  use `open` directive
to accumulate recipe space like Haskell's `import`
and Modular Implicits' `open implicit` declarations.
For example, `[%imp "<opened>.Show"]` is to instruct the PPX to gather
recipes from the sum-modules named `Show` under modules opened
by the `open` directive.

Another policy, which is not orthodox but probably very useful
in the current OCaml library structure, is a control based on
the module names occurring in implicit value types.
For example, if an implicit expression `[%imp "<related>.sexp_of*"]`
has a type `PATH.t -> Sexplib.Sexp.t`, then recipes are the values
whose paths match with  `PATH.sexp_of*` and `Sexplib.Sexp.sexp_of*`.
Many library functions for data types which take sub-functions for
their parameter types can be easily integrated by this algorithm.

### To Modular Implicits and type classes

Once the implicit arguments by optional arguments works, it is almost
trivial to have the same functionalities of Modular Implicits
and type classes in `ppx_implicits`: it is to generate modules
as first class values at the caller site, then to make it back
to normal modules at the callee site.

Details are omitted for this proposal but you can see some examples
at `tests/typeclass.ml` of the source code.

## Conclusion

Typeful PPX is a way to write type dependent extensions of OCaml.
This can be used not only for a fast and safe prototyping of OCaml compiler
modification but also for real use.

