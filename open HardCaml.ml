open HardCaml

let half_adder (a : Signal.t) (b : Signal.t) : Signal.t * Signal.t =
  let open Signal in
  let sum = a ^: b in
  let carry = a &: b in
  sum, carry

let () =
  let a = Signal.input "a" 1 in
  let b = Signal.input "b" 1 in
  let sum, carry = half_adder a b in

  let circuit = Circuit.create_exn ~name:"half_adder" [a; b] [sum; carry] in
  let verilog_code = Circuit.to_verilog circuit in

  print_endline verilog_code