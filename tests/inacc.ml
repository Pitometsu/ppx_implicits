module X = struct
  module Show = struct
    let int = string_of_int
  end
end

open X (* to make Show searchable *)

module Y = struct
  module X = struct
    (* This makes X.Show.int inaccessible *)
  end 
  let () = assert ([%imp2 Show] 1 = "1")
end



