module X = struct
  module Show = struct
    let int = string_of_int
  end
end

open X

module Y = struct
  module Show = struct
    (* This makes X.Show.int inaccessible *)
  end 
  module X = struct
  end
  let () = assert ([%imp opened Show] 1 = "1")
end



