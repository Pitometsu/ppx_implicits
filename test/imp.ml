module I = struct
  let __imp__ () = assert false
  let int = 1
  let (+) = (+)
  let (+.) = (+.)
end

let x : int = I.__imp__
let () = assert ((I.__imp__ ()) 1 2 = 3)
let () = assert ((I.__imp__ ()) 1.2 3.4 = 4.6)

