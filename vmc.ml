open Batteries

(* signature for a language together with its abstract machine *)
module type S = sig
  module Language : sig
    type ast (* the abstract syntax tree of the language *)
    val show_ast : ast -> string
    val parse: string -> ast
  end

  module Machine : sig
    type instr (* vm instructions *)
    val show_instr : instr -> string
    val code: Language.ast -> instr list
  end
end

(* imperative: C / CMa *)
(* functional: PuF / MaMa *)
(* logic based: Proll / WiM *)

let () =
  let help () =
    print_endline @@ "Usage: vmc [<command>] <path>";
    print_endline "Available commands:";
    print_endline "\tcode\tGenerate code (default if no command is given).";
    print_endline "\tast\tPrint the abstract syntax tree.";
    print_endline "The target machine is determined by the file-extension: *.c -> CMa, *.ml -> MaMa, *.pl -> WiM.";
  in
  let exec command path =
    let module X = (val match Filename.extension path with
      | ".c" -> (module C : S)
      | _ -> print_endline "Unsupported file-extension. See help."; exit 1)
    in
    let ast = X.Language.parse (input_file path) in
    match command with
    | `AST -> print_endline (X.Language.show_ast ast)
    | `Code ->
        let is = X.Machine.code ast in
        List.iter (print_endline % X.Machine.show_instr) is
  in
  match Sys.argv |> Array.to_list |> List.tl with
  | ["code"; path] | [path] -> exec `Code path
  | ["ast"; path] -> exec `AST path
  | _ -> help ()
