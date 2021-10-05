(* --- Options for Decompiler with TacTok --- *)

let default_beam_width = 10
let opt_beam_width = ref default_beam_width
let _ = Goptions.declare_int_option {
  Goptions.optdepr = false;
  Goptions.optname = "Beam width";
  Goptions.optkey = ["beam"];
  Goptions.optread = (fun () -> get_beam_width ());
  Goptions.optwrite = (fun o ->
                   let wid = o in
                   set_beam_width wid) }

let set_beam_width = (:=) opt_beam_width
let get_beam_width () = !opt_beam_width