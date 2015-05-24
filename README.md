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

## Implicit values `[%imp POLICY]`

Special expression `[%imp POLICY]` is for *implicit values*, whose definitions are
dependent on the context type of the expression and automatically composed from 
the values specified by `POLICY`.

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
  include Show

  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* currently a label starts with '_' is required to express instance dependencies *)
end

let () = assert ([%imp Show2] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
(* [%imp Show] is expanded to Show2.(list ~_d:(list ~_d: int)) *)
```

The special label which starts with `_` attached to the argument of `Show2.list`
denotes that the value is actually a higher order instance: `list` depends on another `instance`.


## Instance search policies

The policy is not a simple module path but forms a small DSL. For example, 
you can list policies by `,` to accumulate instances:

```ocaml
module Show3 = struct
  let list = Show2.list
end

let () = assert ([%imp Show, Show3] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
(* [%imp Show] is expanded to Show3.list ~_d:(Show3.list ~_d: Show.int) *)
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

## Implicit parameters for overloading

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
`?(imp=v)` does not help here since it is not type dependent.
In addition, by omitting `[%imp POLICY]` argument, we have no information about
which instance search policy  should be used for `show`. 
Here we need to transfer the policy information from `[%imp POLICY]` to 
the type world and associate `show` with it somehow.

### Instance search policy by type

We first introduce a special type name `__imp__`. 
We introduce a special expansion rule when an optional argument of type

```ocaml
?l: t PATH.__imp__ ->
```

is omitted at applications: the omitted argument is replaced by 
an explicit application of `?l:[%imp]`.

`[%imp]`'s instance search policy is dependent on its type.  If `[%imp]`
has a type `t PATH.__imp__`, then its policy is expected to be declared in
module `PATH` using `[%%imp_policy POLICY]` declaration 
(otherwise it is handled as an error). For example, if we have:

```ocaml
module ShowClass = struct
  type 'a __imp__ = ...
  [%%imp_policy opened ShowInstance]
  ...
end
```

then `[%imp] : t ShowClass.__imp__` is equivalent with `[%imp opened Show]`.

### Implicit parameters by optional parameter + poilcy by type

Combining the above two ideas, now we have:

```ocaml
module Show = struct
  type 'a __imp__ = Packed of 'a
  [%%imp_policy opened ShowInstance]

  let unpack = function None -> assert false | Some (Packed x) -> x  
  let show ?imp x = unpack imp x
end

module ShowBase = struct
  module ShowInstance = struct
    let pack ~_d = Show.Packed _d
    let pack_opt ~_d = Some (Show.Packed _d)
  end
end

module Int = struct
  module ShowInstance = struct
    let int = string_of_int
  end
end

open ShowBase (* to make use of ShowBase.ShowInstance.pack as an instance *)
open Int      (* to make use of Int.ShowInstance.int as an instance *)
```

With this settings, `Show.show 1` now properly works getting a proper instance value
for its implicit parameter:
 
```
let ()  = assert (Show.show 1 = "1")
(* is a sugar of              Show.show ?imp:None 1
   is replaced by             Show.show ?imp:[%imp] 1
   which is equivalent with   Show.show ?imp:[%imp opened ShowInstance] 1
   which is equivalent with   Show.show ?imp:[%imp ShowBase.ShowInstance, Int.ShowInstance] 1
   and is finally expanded to Show.show ?imp:(ShowBase.ShowInstance.pack_opt ~_d:Int.ShowInstance.int) 1
*)
```

The type `'a __imp__` is almost equilvalent with `'a` but uses a variant 
in order to prevent `t __imp__` from being expanded to `t`. 
Otherwise, the type dependent policy management would not work correctly.
Packing of instance value to `__imp__` type is done by `ShowBase.ShowInstance.pack_opt`.
Interestingly, this function can be used as an instance and the overload resolution
automatically selects it to provide `__imp__` values.


## Deriving value implicits

You can define type dependent values from other type dependent values:

```ocaml
let show_twice ?imp x = Show.show ?imp x ^ Show.show ?imp x

let () = assert (show_twice 1 = "11")
```


## Type classes or modular implicits via value implicits

To be written.
