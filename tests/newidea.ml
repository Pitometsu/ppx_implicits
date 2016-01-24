module X = struct
  type 'a t = private 'a
  [%%imp_spec Plus]
  external make : 'a -> 'a t = "%identity"
end

module Plus = struct
  let plus_int = X.make (+)
  let plus_float = X.make (+.)
end

let () = assert ([%imp] 1 2 = 3)
  
