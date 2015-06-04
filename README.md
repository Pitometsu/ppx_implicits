# NOTE

The basic tools of `ppx_implicits` are done, but their compositions are still updating.
This document can change often and can be obsolete. Check files under `tests` directory which are
sure to be working and up-to-date.

# Value Implicits

`ppx_implicits` is an OCaml PPX preprocessor 
for implicit values, implicit parameters, modular implicits and type classes.

You can enjoy overloading with your official OCaml compiler (4.02.1), today!!
No compiler patching required. 

## How to build

Sorry, `ppx_implicits` is not yet OPAM available.

```shell
$ opam install omake ppx_tools
$ hg clone https://bitbucket.org/camlspotter/ppx_implicits  # You need Mercurial
$ cd ppx_implicits
$ cp OMakeroot.in OMakeroot
$ omake
```

`omake` should build `ppx/ppx_implicits` then test files under `tests/`.

## Oh, it does not work with `ocaml`.

`ppx_implicits` does not work with `ocaml` toplevel. Please use `ocamlc` or `ocamlopt`.
This is due to the limitation of PPX framework, which cannot pass big information from
preprocessing of one compilation unit to another. In the toplevel, the compilation unit
is each toplevel expression and `ppx_implicits` cannot share important typing information
between toplevel expressions. 

## Implicit values `[%imp POLICY]`

Special expression `[%imp POLICY]` is for *implicit values*, whose definitions
are dependent on the context type of the expression
and automatically composed from the values specified by `POLICY`.

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

The values for the composition are called *instances*. Instances can be combined recursively:

```ocaml
module Show2 = struct
  include Show (* int and float are available *)

  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* currently a label starts with '_' is required to express instance dependencies *)
end

let () = assert ([%imp Show2] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
(* [%imp Show] is expanded to Show2.(list ~_d:(list ~_d: int)) *)
```

The special label which starts with `_` attached to the argument of `Show2.list` denotes that the value is actually a higher order instance.
Such labels of the form `_LABEL` or `?_LABEL` are called *constraint* labels.
If you know Haskell, constraint labels correspond with Haskell's special
arrow for type classes: `t1 => t2`.


## Instance search policies

The policy is not a simple module path but forms a small DSL. For example, 
you can list policies by `,` to accumulate instances:

```ocaml
module Show3 = struct
  let twin ~_d (x,y) = "(" ^ _d x ^ ", " ^ _d y ^ ")" 
end

let () = assert ([%imp Show, Show3] ([ 1 ], [ 2; 3 ]) = "([ 1 ], [ 2; 3 ])")
(* [%imp Show] is expanded to Show3.list ~_d:(Show3.twin ~_d: Show.int) *)
```

You can also write `opened PATH` to specify multiple modules with name `PATH` 
which exist just under the opened module paths. 
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
     
let () = assert ([%imp opened Show] [ 1 ] = "[ 1 ]")
(* Here, [%imp opened Show] is equivalent with [%imp MInt.Show, MFloat.Show, MList.Show] *)
```

## Type dependent instance policy

With some conditions, we can simly write `[%imp]` and omit its policy.
The policy of `[%imp]` is type-dependent:
it is deduced from the type information of the expression.

First of all, the type of `[%imp]` must be
`(t1,..,tn) PATH.name` or `(t1,...,tn) PATH.name option` or their alias,
so that the policy can be found in module `PATH`.

The module `PATH` must have a special declaration `[%%imp_policy POLICY]`
for `[%imp]` expressions of a type related with `PATH`.

For example, if we have

```ocaml
module M = struct
  type 'a t = ...
  [%%imp_policy POLICY]
end
```

then `[%imp]` of type `int M.t` is equivalent with `[%imp POLICY]`.

Let's use this `[%imp]` in an actual example:

```ocaml
module M = struct
  type 'a t = Packed of 'a -> string
  [%%imp_policy opened Show]
end

let show (M.Packed x) = x

(* We use modules defined above *)
open MInt
open MFloat
open MList
     
let () = assert (show [%imp] [ 1 ] = "[ 1 ]") (* [%imp] is expanded to [%imp opened Show] *)
```

We cannot define the type `M.t` simply as `type 'a t = 'a -> string`,
since `M.t` must not be an alias.
This is essential to associate data types and policies together.

The form of the type of `[%imp]` is not only `(t1,...,tn) PATH.name`
but also can be `(t1,...,tn) PATH.name option`.
This is for efficient handling implicit parameters explained later.






## Deriving value implicits

You can define type dependent values from other type dependent values:

```ocaml
let show (M.Packed x) = x
let show_twice imp x = show imp x ^ show imp x

let () = assert (show_twice [%imp] 1 = "11")
```

`show_twice` function takes an implicit paramter `imp` and delivers it to its internal uses of `show`. The type information of the first argument of `show_twice` is as same as the one of the first argument of `show`, `'a M.t`. Therefore `show_twice [%imp] 1` works as intended using the policy defined in module `M`.

This is classic but requires explicit code of implicit value dispatch.
Actually you can let `ppx_implicits` to wire up this dispatch code:

```ocaml
let show_twice ~_imp:(_:'a M.t) (x : 'a) = show [%imp] x ^ show [%imp] x
```

When a function takes an argument with a constraint label
(`_LABEL` or `?_LABEL`), the argument value is automatically added to
the instance search spaces for all the occurrences of `[%imp]` and
`[%imp POLICY]` in its scope.
In the above example, the argument labeled `_imp` has type `'a M.t`.
The argument value is an instance of implicit values for `'a M.t`
inside the body of this function abstraction. The uses of `[%imp]`
in the function body have the same type `'a M.t`, therefore they are
expanded to the argument value and the whole code becomes as follows: 

```ocaml
let show_twice ~_imp:((_:'a M.t) as imp) (x : 'a) = show imp x ^ show imp x
```
which is equivalent with the first example of `show_twice`
with the explicit dispatch.

As we have seen, instead of writing dispatch code by hand,
auto generation of implicit value dispatch often requires
type constraints for the hint of wiring.




## Implicit parameters as optional parameter

Optional constraint labels `?_LABEL:` are as same as non-optional
constraint labels `~_LABEL:` but they are to provide implicit parameters.
Implicit parameters can be omitted at function applications.
If omitted, these values are decided type dependent.


We can define `show` function which takes an implicit value:

```ocaml
let show imp x = imp x

let () = assert (show [%imp opened Show] 1.2 = "1.2")
let () = assert (show [%imp opened Show] [ 1 ] = "[ 1 ]")
```

### Optional for Implicit

`show` works like an overloaded function... but it requires explicit application
of an impliciti value.  For the real overloading, we want to omit this *dispatch* code.
We can use OCaml's optional arguments:

```ocaml
let show ?imp x = match imp with
  | None -> assert false
  | Some imp -> imp x

let () = assert (show 1.2 = "1.2")    (* it does not work! *)
```

Now we can omit the implicit value argument but it does not work lacking 
the way to specify the default.  The optional parameter's default value:
`?(_imp=v)` does not help here since it is not type dependent.
In addition, by omitting `[%imp POLICY]` argument, we have no information about
which instance search policy  should be used for `show`. 
Here we need to transfer the policy information from `[%imp POLICY]` to 
the type world and associate `show` with it somehow.

### Implicit parameters by optional parameter + poilcy by type

For optional arguments, we introduce a new expansion rule: 
if an optional argument of label `?_LABEL` is omitted at application,
it is expanded to `?_LABEL:[%imp]`. For example, if we have

```ocaml
let show ?_imp x = match imp with
  | Some imp -> imp x
  | None -> assert fasle
```

then `show 1` which is a sugar of `show ?_imp:None 1` is expanded to `show ?_imp:[%imp] 1`.

Combining this and `[%imp]`'s type depednent policy, now we have:

```ocaml
module ShowClass = struct
  type 'a t = Packed of 'a -> string
  [%%imp_policy opened Show]

  let unpack = function None -> assert false | Some (Packed x) -> x  
end

let show ?_imp x = ShowClass.unpack _imp x

module ShowBase = struct
  module Show = struct
    let pack ~_d = Show.Packed _d
    let pack_opt ~_d = Some (Show.Packed _d)
  end
end

open ShowBase (* to make use of ShowBase.ShowInstance.pack as an instance *)

(* We use modules defined above *)
open MInt
open MFloat
open MList
```

With this settings, `Show.show 1` now properly works getting a proper instance value
for its implicit parameter:
 
```
let ()  = assert (Show.show [ 1 ] = "[ 1 ]")
(* is a sugar of              Show.show ?_imp:None [ 1 ]
   is replaced by             Show.show ?_imp:[%imp] [ 1 ]
   which is equivalent with   Show.show ?_imp:[%imp opened Show] [ 1 ]
   which is equivalent with   Show.show ?_imp:[%imp ShowBase.Show, MInt.Show, MFloat.Show, MList.Show] [ 1 ]
   and is finally expanded to Show.show ?_imp:(ShowBase.Show.pack_opt ~_d:(MList.Show.list ~_d:MInt.Show.int)) [ 1 ]
*)
```

Thing to do: `open ShowBase` is always required to create a `t Show.t option` value from other instances. 
We should have a way to omit this `open`.






## Type classes or modular implicits via value implicits

To be written.
