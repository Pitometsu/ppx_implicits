(** Implicit instance specification DSL *)

open Types

type t = t2 list 
    (** Syntax: [<t2>, .., <t2>].
        Sum of specs.
    *)

and t2 = 
  | Opened of [`In | `Just] * Longident.t
    (** Syntax: [opened <module-path>] or [opened (just <module-path>)].

        [opened P] gets the values defined under Q.P and its submodules
        where [Q] is opened by [open Q].
        
        [opened (just P)] only gets the values defined under Q.P
        where [Q] is opened.  No values of submodules of [Q.P] are taken.

        Example:

        open Int
        open Float
        
        [%imp opened Show] 42 = "42"

        In the above code, the values defined in [Int.Show] and [Float.Show]
        and their sub-modules are taken into account.
    *)
      
  | Direct of [`In | `Just] * Longident.t * Path.t option
    (** Syntax: [<module-path>] or [just <module-path>]. 

        [P] is for the values defined under module [P] and [P]'s sub-modules. 
        [just P] is for values defined just under module [P] and values defined 
        in its sub-modules are not considered. 

        Example:

        [%imp Int.Show, Float.Show, List.Show] [42] = "[42]"

        In the above code, the values defined in [Int.Show], [Float.Show]
        and [List.Show] and their sub-modules are taken into account.
    *)
      
  | Aggressive of t2 
    (** Syntax: [aggressive <t2>]. 

        Turn on aggressive resolution: some of non constraint labeled arrows of 
        instances from [<t2>] are treated as if they were constraints.

        Let t is the type of an instance value from [<t2>].  The value is
        handled as if it has (possibly multiple) types of A(t) defined 
        as follows:

        * t is in A(t)
        * If t is l1:t1 -> t2 where t1 has a type variable, 
          _l1:t1 -> t2' is in A(t) where t2' is in A(t2).
          Note that _l1:_ is constraint labeled.
        
        For example, a value of type [('a -> string) -> ('b -> string) -> ('a * 'b) list -> string] is handled in three ways:

        * An instance of [('a -> string) -> ('b -> string) -> ('a * 'b) list -> string]
        * An instance of [_x:('a -> string) -> ('b -> string) -> ('a * 'b) list -> string], with one constraint arrow labeled [_x]
        * An instance of [_x:('a -> string) -> _y:('b -> string) -> ('a * 'b) list -> string], with two constraint arrows labeled [_x] and [_y]

        [aggressive <t2>] is very useful with the conjunction of ppx_deriving.
        The functions generated by ppx_deriving can be easily made "implicit"
        using [aggressive <t2>].
    *)
      
  | Related 
    (** Syntax: [related]. 

        If a target type [t] contains data type defined in module [P],
        the values defined in [P] is taken into account of instance candidates.

        For example, [[%imp related]] has type [X.Y.Z.data -> P.Q.result],
        then the values defined at [X.Y.Z] and [P.Q] become the instance 
        candidates of this [[%imp related]].

        Note that [related] considers alias expanded types. If [M.t] is an
        alias [N.t] and this alias is visible at the use of [related],
        only [N] is considered. The values of [M] are not candidates.
    *)
      
  | Name of string * Re.re * t2
    (** Syntax: [name <regexp> <t2>]. 

        Constraint values only to those whose names match with 
        the regular expression (PCRE).

        Example: [name "show" related] denotes the values obtained by [related]
        but restricted to those with ["show"] in its name, for example,
        [show_int], [float_show].
    *)

  | Has_type of Parsetree.core_type * type_expr option 
    (** Spec introduced by [[@@typeclass]] for typeclass style resolution. 

        <currently the specification is not really stable>
    *)
      
  | Deriving of Longident.t
    (** Syntax: [deriving <module-path>]. 

        Tuples, objects and polymorphic variants are not inductively defined:
        i.e. n-tuple are not defined using (n-1)-tuple. It makes it hard to
        define deriving implicit function over them.

        [deriving M] introduces auto-generated functions for tuples, objects
        and polymorphic variants. Module [M] may define values [M.tuple],
        [M.object_] and [M.polymorphic_variant] of types:

        * [val tuple : ~_d:Obj.t list -> ty['a]]
        * [val object_ : ~_d:(string * int * Obj.t) list -> ty['a]]
        * [val polymorphic_variant : ~_d:(string * int * Obj.t option) list -> ty['a]]

        As [Obj.t] in the types of these functions suggests, these functions 
        are essentially untyped. See tests/deriving.ml for detailed example.
    *)

  | PPXDerive of Parsetree.expression * Parsetree.core_type * type_expr option
    (** Syntax: [ppxderive (<expr> : <type>)]. 

        Yet another spec to bridge deriving(ppx_deriving) and implicits,
        which should be easier to use.

        In [ppxderive (e : t['a])], the type [t['a]] must have exactly 
        one type variable ['a].  If the context type matches with [t['a]]
        with a substitution S, [ppxderive (e : t['a])] adds an expression
        [e'] to instance candidates where [e'] is obtained from [e] by
        replacing type expression [_] by S(['a]).

        For example, [[%imp ppxderive ([%derive.show: _] : 'a -> string)] 42],
        the expected type of [[%imp ...]] is [int -> 'b] and ['a] matches
        with [int].  By replacing [_] in [[%derive.show: _]], [[%imp ...]]
        adds [[%derive.show: int]] to the candidate space.
    *)

val is_static : t2 -> bool
(** [true]  : Instance space can be computed statically.

    [false] : Instance space is highly dependent on the target type
              and should be computed dynamically. (Static computation is
              simply impossible since it may be infinite without restriction
              of the target type.)
*)

val to_string : t -> string
(** Convert [t] to its string representation for [[%%imp_spec ...]] *)

(** Compute the instance space *)
val candidates 
  : Env.t 
    -> Location.t 
    -> t 
    -> type_expr 
    -> Candidate.t list
