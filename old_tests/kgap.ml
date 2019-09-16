module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  let tuple ~_a:show_a (a, b) ~_b:show_b = "( " ^ show_a a ^ ", " ^ show_b b ^ " )"
end

let () =assert ([%imp Show] ([1;2],3) = "( [ 1; 2 ], 3 )")
