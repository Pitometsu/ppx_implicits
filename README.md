# Value Implicits

`ppx_implicits` is an OCaml PPX preprocessor 
for implicit values, implicit parameters, modular implicits and type classes.

You can enjoy overloading with your official OCaml compiler (4.02.1), today!!

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

let () = assert ([%imp Show] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
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
an explicit application of `~l:[%imp]`.

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
   is replaced by             Show.show ~imp:[%imp] 1
   which is equivalent with   Show.show ~imp:[%imp opened ShowInstance] 1
   which is equivalent with   Show.show ~imp:[%imp ShowBase.ShowInstance, Int.ShowInstance] 1
   and is finally expanded to Show.show ~imp:(ShowBase.ShowInstance.pack ~_d:Int.ShowInstance.int) 1
*)
  
```

The type `'a __imp__` is almost equilvalent with `'a` but uses a variant 
in order to prevent `t __imp__` from being expanded to `t`. 
Otherwise, the type dependent policy management would not work correctly.
Packing of instance value to `__imp__` type is done by `ShowBase.ShowInstance.pack`.
Interestingly, this function can be used as an instance and the overload resolution
automatically selects it to provide `__imp__` values.


## Deriving value implicits
  
You can define type dependent values using other implicit values.
The above overloaded value `show` is already sucn an example.





Deriving value implicits.  You can defined higher order functions which take
implicit values from the outer scope:

```ocaml
let show_twice imp x = imp x ^ imp x

let () = assert (show_twice [%imp Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")
(* is equivalent with *)
let () = assert (show_twice Show.(list ~_d:int) [1;2] = "[ 1; 2 ][ 1; 2 ]")
```

If we put this idea of higher order functions taking implicit values 
back to the first show example, we get:

```ocaml
let show (f : 'a -> string) = f
(* 'a -> string  is the most general anti-unifier of the instances *)

let () = assert (show [%imp Show] 1 = "1")
let () = assert (show [%imp Show] 1.0 = "1.")
let () = assert (show [%imp Show] [1;2] = "[ 1; 2 ]")
```

It looks like overloading, but with explicit dispatch.

## Instance space policy

`[%imp M]` searches the sub-modules of `M` for the instances:
  
```ocaml
module Show' = struct
  module Show = Show
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice [%imp Show'] (1,1.2) = "(1, 1.2)(1, 1.2)")
(* is equilvalent with *)
let () = assert (show_twice (Show'.tuple Show'.Show.int Show'.Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")
```

We may also be able to list multiple modules `[%imp M1, .., Mn ]`: 

```ocaml
module Show'' = struct
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice [%imp Show, Show''] [1;2] = "[ 1; 2 ][ 1; 2 ]")
(* is equilvalent with *)
let () = assert (show_twice (Show''.tuple Show.int Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")
```

### `[%imp opened M]` extension by open

Composing modules by include and module aliases are bit inefficient 
and listing more modules is bit boring. Any good idea to facilitate this?

Haskell uses module import to define the search space for type class instances
and we can borrow the idea: make use of `open M`.
`[%imp opened Show]` seeks child module `Show` defined 
in explicitly opened modules in the current context: if we have `open X`
and `X.Show` exists, `X.Show` is searched for `[%imp opened Show]`.

```ocaml
module X = struct
  module Show = Show
end

module Y = struct
  module Show = Show''
end

open X
open Y

let () = assert (show_twice [%imp opened Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")
(* is equivalent with *)
let () = assert (show_twice [%imp X.Show, Y.Show] [1;2] = "[ 1; 2 ][ 1; 2 ]") 
(* is equivalent with *)
let () = assert (show_twice (X.Show.list ~_d:X.Show.int) [1;2] = "[ 1; 2 ][ 1; 2 ]") 
```

## Search space by type: `[%imp]` 

`show` and `show_twice` normally use `[%imp Show]` or `[%imp opened Show]`, so writing 
`Show` everytime is boring. It would be better if we can omit writing `Show` like:

```ocaml
show [%imp] 1
show_twice [%imp] 1
```

This requires shift of the information of the search space name `Show` from
`[%imp PATH]` to type:  functions `show` and `show_twice` must have
types which are associated with the module name `"Show"`:

```ocaml
module Z = struct
  module Show = struct
    type 'a __imp__ = Packed of 'a
	[%%imp_policy opened Show]  (* [%imp] with this __imp__ type uses  opened Show  policy *)
    let pack ~_d = = Packed _d 
    let unpack (Pack d) = d
  end
end
    
let show = unpack

open X
open Y
open Z

let () = assert (show [%imp]) 1 = "1")
```

The last line is equivalent with:

```ocaml
let () = assert (show [%imp opened Show] 1 = "1")
```

since `[%imp]` has a type `int Z.Show.__imp__` and module `Z.Show`
has a policy declaration `[%imp_policy opened Show]`.
This code is equivalent with:

```
let () = assert (show (Z.Show.pack ~_d:X.Show.int) 1 = "1")
```

## Implicit applicaiton of `[%imp]` by the optional parameters

Writing `[%imp]` is now boring, but vanilla OCaml provides
no way of omitting code... except the optional arguments!

```ocaml
module Z' = struct
  module Show = struct
    type 'a __imp__ = Packed of 'a
	[%%imp_policy opened Show]
    let pack ~_d = = Packed _d 
    let pack_opt ~_d = = Some (Packed _d)
    let unpack = function
      | None -> assert false
      | Some (Packed d) -> d
  end
end

let show ?imp x = Z'.Show.unpack x

let () = assert (show 1 = "1")
```

Here, `show 1` is equivalent with `show ?imp:None 1`.

We now introduce a rule: `?label:None`  where `None` has
type `_ PATH.__imp__`and module `PATH` has a policy `POLICY`,
it is replaced by `?label:[%imp]`.

Using this rule the above is replaced as:

```ocaml
let () = assert (show ?imp:[%imp] 1 = "1")
(* is equivalent with *)
let () = assert (show ?imp:[%imp opened Show] 1 = "1")
(* is equivalent with *)
let () = assert (show ?imp:(Z'.Show.pack_opt ~_d:X.Show.int) 1 = "1")
```

## Type classes or modular implicits via value implicits

To be written.
