# Ppx_typeclass

Ppx_typeclass is to provide typeclass/modular implicit to OCaml 
via ppx ( [link](http://whitequark.org/blog/2014/04/16/a-guide-to-extension-points-in-ocaml/) or [link](https://blogs.janestreet.com/extension-points-or-how-ocaml-is-becoming-more-like-lisp/) ) framework.

## Highlights

### Simple

Ppx_typeclass is a pre-processor solution: **no need of compiler patching**. 
You can use typeclasses with the vanilla OCaml compiler just now.

The implementation of ppx_typeclass is pretty small: 
it uses the result of the vanilla OCaml type checker 
and just replaces the uses of overloaded values by their proper instances.

### Safety

Ppx_typeclass is a ppx: it output is OCaml source code then it is type-checked
again by the vanilla OCaml compiler. The output does not add any `Obj.magic`
trick therefore type safety of its output is assured.

## Current status

This is still at a "proof of concept" stage.

## Ideas

You need some knowledge of Haskell typeclasses.

### Type class declaration = module type declaration

A type class declaration in Haskell like

```haskell
class Show a where
  show :: a -> String
```

is translated to a module type:

```ocaml
module type Show = sig
  type a
  val show : a -> string
end
```

### Type class instance = module implementation

A type class instance (or dictionary implementation) in Haskell like

```haskell
instance Show Int where
  show = string_of_int

instance Show Float where
  show = string_of_float
```

is translated to a module implementation:

```ocaml
module Int = struct
  type a = int
  let show = string_of_int
end

module Float = struct
  type a = float
  let show = string_of_float
end
```

Of course the instance module must be an instance of the module type
of type class, here `Int` must be an instance of `Show` and actually
its module type is `Show with type a = int`.

### Implicit dictionary dispatching via optional arguments

Each member of type classes must have its entry point
and it is implemented as a function which takes a first class module value
for the dictionary dispatching. This argument for the dispatching is optional
so that the users can omit it. If the dispatch argument is omitted, 
ppx_typeclass automatically fills it using the inferred type of the argument.
The optional label for the dispatching must have a special form
`_[A-z]*`, in order to be distinguished from the ordinaly optional arguments.

For example, the entry point of `show` has the following type:

```ocaml
(* Need to write by yourself but it can be automatically generated in future. *)
type 'a show = (module Show with type a = 'a) 
val show : ?_d:'a show -> 'a -> string   (* ?_ indicates it is for dispatching *)
```

which looks like the type of `show` in Haskell:

```haskell
show :: Show a => a -> string
```

The implementation of `show` entry point is as follows:

```ocaml
(* Need to write by yourself but it can be automatically generated in future. *)
let show : ?_d:(module Show with type a = 'a) -> 'a -> string = 
    | None -> assert false
    | Some _d -> let module D = (val (_d : a t)) in D.show
```

It looks a bit complicated because of the use of the first class module
but actually simple. `show` is given by the dictionary module argument of 
the label `?_d`.  If the optional argument is omitted, it is an error,
but usually ppx_typeclass fills omitted dictionary dispatching automatically.
For example,

```ocaml
show 1
```

is syntactically as same as 

```ocaml
show ?_d:None 1
```

and by passing it to ppx_typeclass, translated equivalent to

```ocaml
show ?_d:(Some (module Int : int show)) 1
```

### Instance search space

**This part can be likely changed in the future release.**

If ppx_typeclass finds a use of optional argument `?_d:None`
where `None`'s type is `t option` for some `t`, it tries to build a value
of type `t` from the available instances.  But what are the available instances?
Where are they searched from?

We cannot allow the entire of the current scope, since it is huge and even worse, not really fixed: there are tons of modules available in the library path... Therefore we must restrict the search space somehow.

Currently, the overloaded instances are searched in the module named `Instance` in the scope of the resolving overloading. There, each instance module must be converted to a first class module value:

```ocaml
module Instance = struct
  (* You have to do it manually now but this conversion of modules to values can be automatic. *)
  let int : int show = (module Int)
  let float : float show = (module Float)
end
```

The module `Instance` is searched recursively inside its submodules, therefore merging sets of instances is easily done:

```ocaml
module Instance = struct
  module X = X.Instance
  module Y = Y.Instance
end
```

### Derived overloading

You can define derived overloaded values, whose overloading is derived from the uses of other overloaded values inside the definition:

```ocaml
let show_twice ?_d x = show ?_d x ^ show ?_d x
```

Unfortunately, we need explicit coding of dictionary dispatching for derived overloading. `let show_twice x = show x ^ show x` is not right: the overloadings of the 2 `show`'s are resolved locally, and result in ambiguous errors.

### Constrained instances as functors

A constrained instance of a type class is coded as a functor which takes
a module for a dependent overloading. Here is an instance of `show` for 'a list`:

```ocaml
module List(A : Show.Show) = struct
  type a = A.a list
  let show xs = "[ " ^ String.concat "; " (List.map A.show xs) ^ " ]"
end
```

Note that `A.show` is not overloaded.  
You can also write as follows, if you want to use the overloaded `Show.show` 
to define `List.show`:

```ocaml
module List(A : Show.Show) = struct
  module Instance = struct
    (* Need to extend the search space with A *)
    let a : A.a show = (module A)
  end
  type a = A.a list
  (* Need type constraint so that the internal use of Show.show can be resovled *)
  let show (xs : a) = "[ " ^ String.concat "; " (List.map Show.show xs) ^ " ]"
end
```

Registering a constrained instance in the instance search space is bit lousy for this moment:

```ocaml
module Instance
  include Instance (* for Int.show and Float.show *)

  (* This is complex... We want to write module List = List ... *)
  let list (type a) ~_d:(d: a show) : a list show =
    let module A = (val d) in
    (module List( (val d) ))
end
```

**This part can be likely changed in the future release.**

A functor of a constrained instance of module type 

```ocaml
functor(A:X) -> Y
```

must be converted to a function of type

```ocaml
_d:(module X with type a = 'a) -> y
```

where `y` is the conversion for `Y`. The label for the argument `_d`
is to inform ppx_typeclass that it is for type class constraint.

## Limitations

These are the limitations from the design and hard to resolve:

* The use of optional arguments makes hard to provide non function overloading:
  `one : ?_d:(module Num with type a = 'a) -> 'a`
* This does not work with toplevel. Ppx only works against one compilation unit and it is each toplevel phrase in toplevel. Ppx has a framework to pass some states from one toplevel pharse to another, but it is very limited and almost impossible to carry type environments ppx_typeclass requires.
* You have to write dispatching code explicitly as we have seen at `show_twice`.To make it implicit we require some changes in the type inference algorithm, which is too much for our ppx based simple hack approach.
