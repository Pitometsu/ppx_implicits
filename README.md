# ppx_implicits, implicit arguments and type classes for OCaml via PPX

ppx_implicits provides implicit arguments: omittable function arguments
whose default values are automatically generated from their types.  
Overloading, type-classes, polymorphic value printers, etc ... 
can be defined/mimicked with implicit arguments.

ppx_implicits is NOT a compiler modification but a PPX preprocessor.
You can play type classes and etc with your official OCaml compiler (4.02.3).

# Simple overloading

Let's start with a simple example of overloaded `add` function which
can work both for `int` and `float` additions.

Here are two addition functions for `int` and `float` in OCaml
which we want to overload to one function `add`:

```
val (+)  : int   -> int   -> int
val (+.) : float -> float -> float
```

We first gather them in a single module, a namespace for overload instances:

```
module Add = struct (* You can choose other module name than Add like Num *)
  let int   = (+)   (* You can choose other variable name than int like (+). *)
  let float = (+.)  (* You can choose other variable name than float like (+.). *)
end
```

Then, define a type for overloading, the type of implicit argument for `add`:

```
type 'a add = ('a -> 'a -> 'a, [%imp_spec Add]) Ppx_implicits.t
```
In ppx_implicits, `(ty, spec) Ppx_implicits.t` is the special type
for implicit arguments: roughly equivalent with type `ty` whose default value
is determined by `spec`.  In this case, `'a add` is equivalent with
type `'a -> 'a -> 'a`, the most general anti-unifier type of the types
of `(+) : int -> int -> int` and `(+.) : float -> float -> float`,
and its default value is composed using the values defined in a module
named `Add`.

We can define the overloaded `add` function using this type:

```
let add : ?_d:'a add -> 'a -> 'a -> 'a = Ppx_implicits.imp
```
`val Ppx_implicits.imp : ?_d:('a,'spec) Ppx_implicits.t -> 'a`
is the extractor function of implicit arguments. If the optional
argument is applied then it simply gets the value of `ty` encapsulated
in `(ty, spec) Ppx_implicits.t`. If the optional argument is omitted,
the function fails, but it should not happen with ppx_implicits:
if omitted, the optional argument of type `(ty, spec) Ppx_implicits.t`
is applied automatically by ppx_implicits, using `spec`.
`add` function is just an alias of this `Ppx_implicits.imp` but with
a stricter type: if the optional argument of `add` is omitted,
it is auto-applied according to the spec `[%imp_spec Add]` which means
using the values defined in the module named `Add`.

Here is an example of such auto-application:
```
let () = assert (add 1 1 = 2)
```
Ppx_implicits converts the above code to:
```
let () = assert (add ~_d:(Ppx_implicits.embed Add.int) 1 1 = 2)
```
where `Ppx_implicits.embed` encapsulate its argument into
`(ty,spec) Ppx_implicits.t`.

Another exapmle of `add` used for `float` addition:
```
let () = assert (add 1.2 3.4 = 4.6)
```
This time, ppx_implicits converts to
```
let () = assert (add ~_d:(Ppx_implicits.embed Add.float) 1 1 = 2)
```

Here is the whole code:

```
module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp_spec Add]) Ppx_implicits.t

let add : ?_d:'a add -> 'a -> 'a -> 'a = Ppx_implicits.imp

let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
```

# Limitation

ppx_implicits does not work with OCaml toplevel (REPL).
Please use `ocamlc` or `ocamlopt`.

This is due to the limitation of PPX framework,
which cannot pass big information from preprocessing of one compilation unit
to another.
In the toplevel, the compilation unit is each toplevel expression
and `ppx_implicits` cannot share important typing information
between toplevel expressions. 
This could be fixed by keeping one PPX process running
throughout an REPL session, but it would need significant change of the REPL...

# How to build

`opam install ppx_implicits`. Probably it may be not the latest version.

The development version source code is available at
`https://bitbucket.org/camlspotter/ppx_implicits`,
but it is likely dependent on development versions of other libraries:


```shell
$ hg clone https://bitbucket.org/camlspotter/ppx_implicits
$ cd ppx_implicits
$ cp OMakeroot.in OMakeroot
$ omake
$ omake install
```

# How to use

Add `-package ppx_implicits` to your `ocamlfind` calls.

If you do not use `ocamlfind`,
add `-ppx ppx_implicits` to your compiler commands.

# Type class

Type class is one of the most complicated example of ppx_implicits
but everyone loves type classes so let's start with it.

## Class declaration

In ppx_implicits, type classes are defined like as follows:

```ocaml
module type Show = sig
  type a 
  val show : a -> string
end [@@typeclass]
```

This is almost equivalent with the following type class definition in Haskell:

```haskell
-- Haskell
class Show a where
  show :: a -> String
```

A module type definition with attribute `[@@typeclass]` defines
a module of the same name. The values declared in the signature are
available as values in the module. In the above example, `ppx_implicits`
defines a module named `Show` with a value `show`. Its signature is:

```ocaml
module Show : sig
  val show : ?_imp: 'a Show._class -> 'a -> string
end
```

Optional arguements labeled with `?_xxx` are considered
as type class constraints by ppx_implicits.
The above signature is
almost equivalent with the following Haskell signature:

```ocaml
show :: 'a Show => 'a -> string
```

## Instance declaration

Now let's define instances of `Show`:

```ocaml
module M = struct

  (* Instance for int *)
  module Int = struct
    type a = int
    let show  = string_of_int
  end [@@instance Show]

  (* Instance for float *)
  module Float = struct
    type a = float
    let show  = string_of_float
  end [@@instance Show]
end
```

A module declaration with `[@@instance PATH]` is to declare an instance
of type class `PATH`. The module must have a signature less general than
the module type `PATH`.

Haskell equivalent of the above code is like as follows:

```haskell
-- Haskell
module M where

instance Show Int where
  show = string_of_int

-- Haskell has Double instead of Float actually... never mind
instance Show Float where
  show = string_of_float
```

## Use of overloaded values

`Show.show` is now usable. Which instances should be used is controlled by
`open` statement:

```ocaml
open M
let () = assert (Show.show 1 = "1")
let () = assert (Show.show 1.2 = "1.2")
```

Here, `open M` makes the instances declarations under `M` available
for the use of `Show.show`. It is as same as `import M` controls
instance availableness in Haskell:

```haskell
-- Haskell
import M
import qualified Show

main :: IO ()
main = do
  -- Haskell overloads number literals...
  assert (Show.show (1 :: Int) = "1") $ return ()
  assert (Show.show (1.2 :: Float) = "1.2") $ return ()
```

## Overloading is first class

You can define a new overloaded value from one defined with `[@@typeclass]`.
So far, manual wiring of constraint labels is required,
either by explicit applications of dispatch `show ?_imp x`
or by an explicit type annotation:

```ocaml
(* Explicit dispatching by application *)
let show_twice : ?_imp x = show ?_imp x  ^ show ?_imp x

let () = assert (show_twice 1.2 = "1.21.2")
```

or

```ocaml
(* Explicit dispatching by type annotation *)
let show_twice' : ?_imp: 'a Show._class -> 'a -> string = fun ?_imp x ->
  show x ^ show x

let () = assert (show_twice' 1.2 = "1.21.2")
```

They are similar to the following Haskell code:

```haskell
-- Haskell
show_twice :: Show a => a -> string
show_twice x = show x ++ show x
```

This explicit wiring is unfortunate but currently necessary in ppx_implicits.

# Implicit values

Ok, now let's go back to the basics of ppx_implicits.

## `[%imp SPEC]` expression

Special expression `[%imp SPEC]` is for *implicit values*, whose definitions
are dependent on the context type of the expression
and automatically composed from the values specified by `SPEC`.

For example, the expression `[%imp Show]` is expaneded using the values defined
under module `Show`:

```ocaml
module Show = struct
  let int = string_of_int
  let float = string_of_float
end

let () = assert ([%imp Show] 1 = "1")   
(* [%imp Show] is expanded to Show.int *)

let () = assert ([%imp Show] 1.0 = "1.")
(* [%imp Show] is expanded to Show.float *)
```

The values for the composition are called *instances*.
Instances can be combined recursively:

```ocaml
module Show2 = struct
  include Show (* int and float are available *)

  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* currently a label starts with '_' is required to express instance dependencies *)
end

let () = assert ([%imp Show2] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
(* [%imp Show] is expanded to Show2.(list ~_d:(list ~_d: int)) *)
```

The special label which starts with `_` attached
to the argument of `Show2.list` denotes that the value is actually
a higher order instance.
Such labels of the form `_LABEL` or `?_LABEL` are called *constraint* labels.
If you know Haskell, constraint labels correspond with Haskell's special
arrow for type classes: `C => t`.

## Instance search policies

The spec is not a simple module path but forms a small DSL. For example, 
you can list policies by `,` to accumulate instances:

```ocaml
module Show3 = struct
  let twin ~_d (x,y) = "(" ^ _d x ^ ", " ^ _d y ^ ")" 
end

let () = assert ([%imp Show, Show3] ([ 1 ], [ 2; 3 ]) = "([ 1 ], [ 2; 3 ])")
(* [%imp Show] is expanded to Show3.list ~_d:(Show3.twin ~_d: Show.int) *)
```

You can also write `opened PATH` to specify multiple modules at once
which exist just under the opened module paths named `PATH`.
This is like Haskell's `import` to specify class instances:

```ocaml
module MInt = struct
  module Show = struct
    let int = string_of_int
  end
end

module MFloat = struct
  module Show = struct
    let float = string_of_float
  end
end

module MList = struct
  module Show = struct
    let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  end
end

open MInt
open MFloat
open MList
     
let () = assert ([%imp opened Show] [ 1 ] = "[ 1 ]") (* MInt.Show, MFloat.Show and MList.Show are the instance space *)
(* Here, [%imp opened Show] is equivalent with [%imp MInt.Show, MFloat.Show, MList.Show] *)
```

## Type dependent instance spec

With some conditions, we can simply write `[%imp]` and omit its spec.
The spec of `[%imp]` is type-dependent:
it is deduced from the type information of the expression.

* The type of `[%imp]` must be `(t1,..,tn) PATH.name`
  or `(t1,...,tn) PATH.name option` or their alias,
  so that the spec can be found in module `PATH`.
* The module `PATH` must have a special declaration `[%%imp_spec SPEC]`
for `[%imp]` expressions of a type related with `PATH`.

For example, if we have

```ocaml
module M = struct
  type 'a t = ...
  [%%imp_spec SPEC]
end
```

and if an expression `[%imp]` has a type `int M.t`,
it is equivalent with `[%imp SPEC]`.

Let's use this `[%imp]` in an actual example:

```ocaml
module M = struct
  type 'a t = Packed of 'a -> string
  [%%imp_spec opened Show]
end

let show (M.Packed x) = x

(* We use modules defined above *)
open MInt
open MFloat
open MList
     
let () = assert (show [%imp] [ 1 ] = "[ 1 ]")
(* [%imp] has the type int list M.t.
   Module M has [%%imp_spec opened Show]
   Therefore this [%imp] is equivalent with [%imp opened Show] *)
```

We cannot define the type `M.t` simply as `type 'a t = 'a -> string`,
since `M.t` must not be an alias.
This is essential to associate data types and policies together.

The form of the type of `[%imp]` is not only `(t1,...,tn) PATH.name`
but also can be `(t1,...,tn) PATH.name option`.
This is for efficient handling implicit parameters explained later.

## Default instances for type dependent `[%imp]`

If `[%imp]`'s spec is defined in a module `M`,
and if this module `M` has a module `Instances`, 
then the values defined in this `M.Instances`
are considered as instances of the implicit value.


## Deriving value implicits

You can `let`-define implicit values from other implicit values:

```ocaml
let show (M.Packed x) = x
let show_twice imp x = show imp x ^ show imp x

let () = assert (show_twice [%imp] 1 = "11")
```

`show_twice` function takes an implicit paramter `imp` and delivers it to its internal uses of `show`. The type information of the first argument of `show_twice` is as same as the one of the first argument of `show`, `'a M.t`. Therefore `show_twice [%imp] 1` works as intended using the spec defined in module `M`.

This is classic but requires explicit code of implicit value dispatch.
Actually you can let `ppx_implicits` to wire up this dispatch code more easily
by explicitly writing some types:

```ocaml
let show_twice ~_imp:(_:'a M.t) (x : 'a) = show [%imp] x ^ show [%imp] x
```

When a function takes an argument with a constraint label
(`_LABEL` or `?_LABEL`), the argument value is automatically added to
the instance search spaces for all the occurrences of `[%imp]` and
`[%imp SPEC]` in its scope.
In the above example, the argument labeled `_imp` has type `'a M.t`.
The argument value is an instance of implicit values for `'a M.t`
inside the body of this function abstraction. The uses of `[%imp]`
in the function body have the same type `'a M.t`, therefore they are
expanded to the argument value and the whole code becomes as follows: 

```ocaml
let show_twice ~_imp:(imp:'a M.t) (x : 'a) = show imp x ^ show imp x
```
which is equivalent with the first example of `show_twice`
with the explicit dispatch.



## Implicit parameters as optional parameter

Optional constraint labels `?_LABEL:` are as same as non-optional
constraint labels `~_LABEL:` but they are to provide implicit parameters.
Implicit parameters can be omitted at function applications.
If omitted, ppx_implicits applies `Some [%imp]` instead:

```ocaml
(* Assume module M, MInt, MFloat and MList defined above are available *)

let show ?_imp = match _imp with
  | None -> assert false
  | Some (M.Packed x) -> x

let () = assert (show 1 = "1")
(* ?_imp is omitted and it is equivalent with  show ?_imp:None 1
   ppx_implicits replaces it by                show ?_imp:(Some [%imp]) 1
   which is equivalent with                    show ?_imp:(Some [%imp opened Show]) 1
   finally it is expanded to                   show ?_imp:(Some Show (M.Packed (MInt.Show.int))) 1
*)

let () = assert (show 1.2 = "1.2")
(* ?_imp is omitted and it is equivalent with  show ?_imp:None 1.2
   ppx_implicits replaces it by                show ?_imp:(Some [%imp]) 1.2
   which is equivalent with                    show ?_imp:(Some [%imp opened Show]) 1.2
   finally it is expanded to                   show ?_imp:(Some Show (M.Packed (MFloat.Show.float))) 1
*)
```

Now `show` is overloaded!

## Back to type class

# Implicit policies

Expression `[%imp SPEC]` has `SPEC` parameter
to specify the instance search space for the implicit value.
`SPEC` is a comma separated list of sub-policies `p1, .., pn`

## Type dependent spec

When `[%imp]` has no written policies, its spec is deduced
from its static typing:

* `[%imp]` must have a type whose expanded form is either
  `... M.name' or `... M.name option` for some module `M`.
* Module `M` must have a declaration `[%%imp_spec SPEC]`.
Under these conditions `[%imp]` is equilvalent with `[%imp SPEC]`.

## `related` spec

`[%imp related]` gathers instances from the modules appear in its type.
For example if `[%imp related]` has a type `'a M.t N.t -> O.t option`
then its instances are obtained from module `M`, `N` and `O`.

Note that types are unaliased in the current typing environment
to get the module names for instances. For example if `M.t` is defined
as `type t = P.t * Q.t` then module `M` is not considered as instance space
but `P` and `Q` (if `P.t` and `Q.t` are not alias.).

## `aggressive(p)` spec

By default, higher order implicit values have constraint labels
`~_LABEL` and `?_LABEL` to receive instances. For example:

```ocaml
val show_list_imp : ~_imp:('a -> string)  -> 'a -> string
```
here, `~_imp` is the sole constraint of the function `show_list_imp`.

Spec `aggressive` removes this limitation: any function arrow can be
treated as constraint arrows.  This is useful to use existing functions
as implicit value instances without changing types.

Suppose we have a function `show_list` of the following type:

```ocaml
val show_list : ('a -> string) -> 'a list -> string
```

If this function `show_list` is in an `aggressive` instance space,
the function's arrows are treated as constraint arrows: the value is
treated as if it had the following types:

```ocaml
val show_list : ~_imp:('a -> string) -> 'a list -> string
val show_list : ~_imp:('a -> string) -> ~_imp2:'a list -> string
```

Note that one value can provide more than one instances.
For example, `show_list` has two ways to be used as intances.
The first one is useful to provide implicit `show` function combining
other `show_xxx` functions, but the second one is probably useless.

## `name "rex" p` spec

Spec `name "rex" p` filters the instances obtained from the spec `p`
by the regular expression `"rex"`. The regular expression must be one of PCRE.
`name` is useful to restrict instances obtained by `related` which tends to
collect undesired values.

# Recursion limit

To assure the type inference of implicit values, recursive uses of instances
are limited: if one instance `x` is composed like `x ~_imp:x ...`,
then the type of the internal use must be strictly smaller than the one of the outer use.
