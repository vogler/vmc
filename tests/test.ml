open Batteries

module C = struct
  module Language = Vmc.C.Language
  module Machine = Vmc.C.Machine

  let parse () =
    let open Language in
    let open Language.Parser in
    test "int" int string_of_int ["123"; " 123 "; "abc"; "0123"; "0xFF"];
    test "identifier" identifier identity ["abc"; " _abc "; "a b"; "a1"; "1a"; "a1b2c3"];
    test "typ" typ show_typ ["int foo"; "int * foo"; "int ** foo"; "int[13]"; "int [ 2 ] "];
    test "lval" lval show_lval ["foo"; "*foo"; "foo.bar"; "foo[13]"; "*foo.bar[13]"];
    test "expr" expr show_expr ["13"; "(12)"; "foo"; "&*foo.bar[2]"; "1+2"; "1+2*3+4"; "foo = bar"; "f()"; "f(a, 2)"];
    test "stmt" stmt show_stmt [" ;"; "13;"; "break;"; "return;"; "return 1+2;"; "{}"; "{ ; }"; "{;;}"; "{1;2;}"; "if ( 1 < 2 ) 3;"; "if ( 1 < 2 ) 3; else { 4*5; 6; }"; "while (1) { 2; }"; "for(i=0; i<3; i=i+1){ 4; }"; "int foo;"; "int[5] a;"];
    test "stmt" stmt show_stmt [" ;"; "13;"; "break;"; "return;"; "return 1+2;"; "{}"; "{ ; }"; "{;;}"; "{1;2;}"; "if ( 1 < 2 ) 3;"; "if ( 1 < 2 ) 3; else { 4*5; 6; }"; "while (1) { 2; }"; "for(i=0; i<3; i=i+1){ 4; }"; "int foo;"; "int[5] a;"];
    test "ast" ast show_ast ["int a; int f(int x, int* y){ int c; return a;} int* g(){{}} int b;"]

  let codegen () =
    let test file =
      let path = "tests/"^file in
      let input = input_file path in
      print_endline @@ "# Input ("^path^"):";
      print_endline input;
      print_endline "# AST:";
      let ast = Language.parse input in
      print_endline (Language.show_ast ast);
      print_endline "\n# Machine instructions:";
      let is = Machine.code ast in
      List.iter (print_endline % Machine.show_instr') is;
      print_endline ""
    in
    test "empty.c";
    test "array.c"

  let () =
    parse ();
    codegen ()
end
