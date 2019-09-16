module Show = struct
  let tuple ~_ds:(ds : Obj.t list) : 'a -> string = fun vs ->
    let vs = Obj.repr vs in
    assert (Obj.is_block vs);
    let s = List.length ds in
    let s' = Obj.size vs in
    assert (s = s');
    let f n d =
      let fn : 'a -> string = Obj.obj d in
      let v : 'a = Obj.obj (Obj.field vs n) in
      fn v
    in
    "(" ^ String.concat "," (List.mapi f ds) ^ ")"

  let polymorphic_variant ~_ds:(ds : (string * int * Obj.t option) list) : 'a -> string = fun v ->
    let v = Obj.repr v in
    if Obj.is_int v then
      let h : int = Obj.magic v in
      match
        try
          List.find (fun (_,h',_) -> h = h') ds
        with
        | Not_found -> assert false
      with
      | l, _, None -> "`" ^ l
      | _ -> assert false
    else
      let h : int = Obj.magic @@ Obj.field v 0 in
      match
        try
          List.find (fun (_,h',_) -> h = h') ds
        with
        | Not_found -> assert false
      with
      | _, _, None -> assert false
      | l, _, Some d ->
          let d : Obj.t -> string = Obj.magic d in
          let a = Obj.field v 1 in
          "`" ^ l ^ " (" ^ d a ^ ")"

  let object_ ~_ds:(_ds : (string * int * Obj.t) list) : 'a -> string = fun _v ->
    "<obj>"
end

module Base = struct
  let show_of_int = string_of_int
end

let eqcheck s s' =
  if s <> s' then raise (Failure (Printf.sprintf "Expected %s but got %s" s' s))
    
let () = eqcheck ([%imp aggressive(name "show" related), Base] 42) "42"
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] (42,42)) "(42,42)"
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] (42,42,(42,42))) "(42,42,(42,42))"
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] `Foo) "`Foo"
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] (`Foo 1)) "`Foo (1)"
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] (`Foo (1, 2))) "`Foo ((1,2))" (* parens are not cool but this is how it works *)
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] (`Foo (1, `Bar) )) "`Foo ((1,`Bar))"
let () = eqcheck ([%imp aggressive(name "show" related), deriving Show, Base] (object method x = 1 end)) "<obj>"
