open Batteries

module C = struct
  let parse () =
    let open C.Language in
    let open C.Language.Parser in
    (* test "int" int string_of_int ["123"; " 123 "; "abc"; "0123"; "0xFF"]; *)
    test "identifier" identifier identity ["abc", "abc"; " _abc ", "_abc"; "a b", "a"; "a1", "a1"; "a1b2c3", "a1b2c3"] ["1a"];
    (* test "typ" typ show_typ ["int foo"; "int * foo"; "int ** foo"; "int[13]"; "int [ 2 ] "]; *)
    (* test "lval" lval show_lval ["foo"; "*foo"; "foo.bar"; "foo[13]"; "*foo.bar[13]"]; *)
    test "expr" expr show_expr ["13", Const 13; "(12)", Const 12; "foo", Lval (Var "foo"); "&*foo.bar[2]", Addr (Deref (Lval (Index (Field (Var "foo", "bar"), Const 2)))); "1+2", Binop (Add, Const 1, Const 2); "1+2*3+4", Binop (Add, (Binop (Add, (Const 1), (Binop (Mul, (Const 2), (Const 3))))),
   (Const 4)); "foo = bar", Asn ((Var "foo"), (Lval (Var "bar"))); "f()", Call ((Lval (Var "f")), []); "f(a, 2)", Call ((Lval (Var "f")), [(Lval (Var "a")); (Const 2)])] [];
    test "expr" expr show_expr [
      "1+2+3", Binop (Add, (Binop (Add, (Const 1), (Const 2))), (Const 3));
      "1+2*3", Binop (Add, (Const 1), (Binop (Mul, (Const 2), (Const 3))));
      "(1 > 2 + 3 && 4)", Binop (And, (Binop (Gr, (Const 1), (Binop (Add, (Const 2), (Const 3))))), (Const 4)); (* (1 > (2 + 3)) && 4 *)
      "1 == 2 != 3", Binop (Neq, Binop (Eq, Const 1, Const 2), Const 3); (* (1 == 2) != 3 *)
      "x = y = 3", Asn ((Var "x"), (Asn ((Var "y"), (Const 3)))); (* x = (y = 3) *)
      "x[0] = 1", Asn ((Index ((Var "x"), (Const 0))), (Const 1));
      "a[2] = *(a+1) + 4", Asn ((Index ((Var "a"), (Const 2))), (Binop (Add, (Lval (Deref (Binop (Add, (Lval (Var "a")), (Const 1))))), (Const 4))));
    ] [];
    (* test "stmt" stmt show_stmt [" ;"; "13;"; "break;"; "return;"; "return 1+2;"; "{}"; "{ ; }"; "{;;}"; "{1;2;}"; "if ( 1 < 2 ) 3;"; "if ( 1 < 2 ) 3; else { 4*5; 6; }"; "while (1) { 2; }"; "for(i=0; i<3; i=i+1){ 4; }"; "int foo;"; "int[5] a;"]; *)
    (* test "stmt" stmt show_stmt [" ;"; "13;"; "break;"; "return;"; "return 1+2;"; "{}"; "{ ; }"; "{;;}"; "{1;2;}"; "if ( 1 < 2 ) 3;"; "if ( 1 < 2 ) 3; else { 4*5; 6; }"; "while (1) { 2; }"; "for(i=0; i<3; i=i+1){ 4; }"; "int foo;"; "int[5] a;"]; *)
    (* test "ast" ast show_ast ["int a; int f(int x, int* y){ int c; return a;} int* g(){{}} int b;"]; *)
    ()

  let codegen () =
    let test file =
      let path = "tests/"^file in
      let input = input_file path in
      print_endline @@ "# Input ("^path^"):";
      print_endline input;
      print_endline "# AST:";
      let ast = C.Language.parse input in
      print_endline (C.Language.show_ast ast);
      print_endline "\n# Machine instructions:";
      let is = C.Machine.code ast in
      List.iter (print_endline % C.Machine.show_instr) is;
      print_endline ""
    in
    test "empty.c";
    test "array.c"

  let () =
    parse ();
    (* codegen () *)
end
