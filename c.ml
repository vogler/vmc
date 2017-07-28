open Batteries
open MyBatteries

module Language = struct
  type ast = gdecl list
  and gdecl = (* global declaration *)
    | Global of decl
    | FunDef of typ * string * (typ * string) list * stmt list (* this includes its definition; pretty ugly... *)
  and decl = typ * string
  and typ =
    | Int
    (* TODO Void *)
    | Ptr    of typ
    | Arr    of int * typ (* length is only known for stack allocated arrays *)
    | Struct of string * (typ * string) list (* missing decl. of prev. def. struct type *)
    | Fun    of typ * typ list (* do we need names here? *)
  and lval =
    | Var   of string (* variable *)
    | Deref of expr (* pointer dereference *)
    | Field of lval * string (* struct offset *)
    | Index of lval * expr (* array offset *)
  and unop =
    | Neg | Not (* unary operations *)
  and binop =
    | Add | Sub | Mul | Div | Mod (* binary arithmetic operations *)
    | And | Or (* binary logical operations *)
    | Eq | Neq | Leq | Le | Geq | Gr (* binary comparisons *)
    | Comma
  and expr =
    | Const of int
    | Lval  of lval
    | Addr  of lval
    | Unop  of unop * expr
    | Binop of binop * expr * expr
    | Asn   of lval * expr
    | Call  of expr * expr list (* why not lval for the function? *)
  and stmt =
    | Nop
    | Continue
    | Break
    | Local      of decl (* C99: we don't require declarations to be at the beginning of a block *)
    | Expr       of expr
    | Block      of stmt list
    | IfThen     of expr * stmt
    | IfThenElse of expr * stmt * stmt
    | While      of expr * stmt
    | DoWhile    of stmt * expr
    | For        of expr * expr * expr * stmt (* initial declaration is syntax introduced with C99 *)
    | Return     of expr option
  [@@deriving show {with_path = false}] (* (show, mapper, folder) *)

  module Parser = struct
    include Parsers
    let typ = lrec
      (string "int" *>> Int)
      (fun ff -> choice [
        char '*' *> ff (fun t -> Ptr t);
        brackets int >>= (fun n -> ff (fun t -> Arr (n, t))); (* TODO array size optional if used with initializer. also, we expect the brackets after the type, not after the identifier of a declaration. *)
      ]) <?> "typ"
    let decl = lift2 Tuple2.make typ identifier <?> "decl"
    let unop  = from char ['-', Neg; '!', Not] <?> "unop"
    let binop = from string [
        "+", Add; "-", Sub; "*", Mul; "/", Div; "%", Mod;
        "&&", And; "||", Or;
        "==", Eq; "!=", Neq; "<=", Leq; "<", Le; ">=", Geq; ">", Gr;
      ] <?> "binop"
    let lval expr = lrec
      (choice [
        identifier >>| (fun id -> Var id);
        char '*' *> expr >>| (fun e -> Deref e);
      ])
      (fun ff -> choice [
        char '.' *> identifier >>= (fun field -> ff (fun l -> Field (l, field)));
        brackets expr >>= (fun index -> ff (fun l -> Index (l, index)));
      ]) <?> "lval"
    let expr lval = fix (fun expr -> lrec
        (choice [
          int >>| (fun i -> Const i);
          parens expr;
          char '&' *> lval >>| (fun l -> Addr l);
          string "++" *> lval >>| (fun l -> Asn (l, Binop (Add, Lval l, Const 1)));
          string "--" *> lval >>| (fun l -> Asn (l, Binop (Sub, Lval l, Const 1)));
          unop >>= (fun op -> expr >>| (fun e -> Unop (op, e)));
          lval >>= (fun l -> string "++" *>> Binop (Comma, Asn (l, Binop (Add, Lval l, Const 1)), Binop (Sub, Lval l, Const 1)));
          lval >>= (fun l -> string "--" *>> Binop (Comma, Asn (l, Binop (Sub, Lval l, Const 1)), Binop (Add, Lval l, Const 1)));
          lval >>= (fun l -> char '=' *> expr >>| (fun e -> Asn (l, e)));
          lval >>| (fun l -> Lval l);
        ])
        (fun ff -> choice [
          binop >>= (fun op -> expr >>= (fun e2 -> ff (fun e1 -> Binop (op, e1, e2))));
          parens (sep_by (char ',') expr) >>= (fun args -> ff (fun f -> Call (f, args)));
        ])) <?> "expr"
    let lval, expr = tie lval expr
    let stmt = fix (fun stmt ->
        choice [
          string "continue" *>> Continue;
          string "break" *>> Break;
          string "return" *> some expr >>| (fun e -> Return e);
          string "do" *> stmt >>= (fun s -> parens expr >>| fun e -> DoWhile (s, e));
          decl >>| (fun d -> Local d);
          expr >>| (fun e -> Expr e);
          ws *>> Nop;
        ] <* char ';' <|>
        choice [ (* right-recursive ones need not be ';'-terminated *)
          curly (many stmt) >>| (fun ss -> Block ss);
          string "if" *> parens expr >>= (fun e -> stmt >>= fun s1 -> string "else" *> stmt >>| fun s2 -> IfThenElse (e, s1, s2));
          string "if" *> parens expr >>= (fun e -> stmt >>| fun s -> IfThen (e, s));
          string "while" *> parens expr >>= (fun e -> stmt >>| fun s -> While (e, s));
          string "for" *> parens (lift3 Tuple3.make (expr<*char ';') (expr<*char ';') expr) >>= (fun (e1,e2,e3) -> stmt >>| fun s -> For(e1,e2,e3,s));
        ]) <?> "stmt"
    let gdecl =
      let global = decl <* char ';' >>| fun d -> Global d in
      let fundef = typ >>= fun t -> identifier >>= fun name -> parens (sep_by (char ',') decl) >>= fun args -> curly (many stmt) >>| fun ss -> FunDef (t, name, args, ss) in
      global <|> fundef
    let ast = many gdecl
  end
  let parse = Parsers.parse Parser.ast
end

module Machine = struct
  type instr =
    | Loadc of const
    | Add | Sub | Mul | Div | Mod (* bin. arith. *)
    | And | Or (* bin. log. *)
    | Eq | Neq | Leq | Le | Geq | Gr (* bin. comp. *)
    | Neg | Not (* un. *)
    | Load | Store
    | Loada of int | Storea of int
    | Pop
    | Jump of label | Jumpz of label
    | Dup
    | Halt
    | New
    | Move of int
    | Mark | Call | Slide of int
    | Enter of int | Alloc of int | Return
    | Loadrc of int | Loadr of int | Storer of int
    | Label of label (* this is not an instruction, but interleaving labels is much easier :) *)
    | Loadcl of label (* used instead of Loadc for functions since we want to see the label instead of some number *)
  and const = int
  and label = string
  [@@deriving show {with_path = false}]
  let show_instr = function
    | Label l -> l^":"
    | Loadcl l -> "loadc "^l
    | x -> show_instr x |> String.filter (function '('|')' -> false | _ -> true) |> String.lowercase

  (* pulled out of Type since we don't want to bother with rec. modules (needed in Environment) *)
  let rec size_of_type = let open Language in function
    | Int | Ptr _ | Fun _ (* ? *) -> 1
    | Arr (n,t) -> n * size_of_type t
    | Struct (_, fields) -> List.(sum (map (size_of_type % fst) fields))

  module Environment = struct
    type scope = Global | Local
    type 'a m = (string, 'a) Map.t
    type addr = int
    type rho = {
      var: (scope * addr * Language.typ) m;
      field: addr m;
      label: string m;
      addr: addr; (* next free address for the current scope *)
    }
    let empty = { var = Map.empty; field = Map.empty; label = Map.empty; addr = 1 }
    let find = Map.find
    let type_of x rho = Tuple3.third (find x rho.var)
    let decl scope (typ, name) rho =
      { rho with var = Map.add name (scope, rho.addr, typ) rho.var; addr = rho.addr + size_of_type typ }
    let add_label k v rho = { rho with label = Map.add k v rho.label }
    let continue_break continue break rho = add_label "continue" continue (add_label "break" break rho)
  end

  module Type = struct
    open Language
    (* we can statically determine the type and therefore size of every expression *)
    let rec of_expr rho = function
      | Binop (Comma, e1, e2) -> of_expr rho e2 (* mh, ugly *)
      | Const _ | Unop _ | Binop _ -> Int (* TODO whay about pointers? *)
      | Lval l -> of_lval rho l
      | Addr l -> Ptr (of_lval rho l)
      | Asn (l, e) -> of_expr rho e
      | Call (f, _) ->
          (match of_expr rho f with
          | Fun (tr, _) -> tr
          | _ -> failwith "Type error")
    and of_lval rho = function
      | Var v -> Environment.type_of v rho
      | Deref e -> of_expr rho e
      | Field (l, o) ->
          (match of_lval rho l with
          | Struct (_, fields) -> List.assoc_inv o fields
          | _ -> failwith "Type error")
      | Index (l, o) ->
          (match of_lval rho l with
          | Arr (_, t) -> t
          | _ -> failwith "Type error")
    let size_of = size_of_type
    let size_of_expr rho = size_of % of_expr rho
    let size_of_elem rho l = match of_lval rho l with
      | Ptr t | Arr (_, t) -> size_of_type t
      | _ -> failwith "Type error"
  end

  (* shortcuts *)
  module L = Language
  module E = Environment

  let unop = function
    | L.Neg -> Neg | L.Not -> Not
  let binop = function
    | L.Add -> Add | L.Sub -> Sub | L.Mul -> Mul | L.Div -> Div | L.Mod -> Mod | L.And -> And | L.Or -> Or | L.Eq -> Eq | L.Neq -> Neq | L.Leq -> Leq | L.Le -> Le | L.Geq -> Geq | L.Gr -> Gr | L.Comma -> Slide 1

  let rec codeR rho = function
    | L.Const q -> [Loadc q]
    | L.Lval l -> codeL rho l @ (match Type.of_lval rho l with L.Arr _ | L.Fun _ -> [] | _ -> [Load])
    | L.Addr l -> codeL rho l
    | L.Unop (op, e1) -> codeR rho e1 @ [unop op]
    | L.Binop (op, e1, e2) -> codeR rho e1 @ codeR rho e2 @ [binop op]
    | L.Asn (l, e) -> codeR rho e @ codeL rho l @ [Store]
    | L.Call (f, args) -> (* TODO malloc and other built-ins? *)
        (* different from slides: Since we reuse the argument cells for return, we need to make sure that there are enough. TODO For now, we only support returning values of size 1... *)
        let m = max 1 List.(sum (map (Type.size_of_expr rho) args)) in
        (* we wouldn't need the alloc 1 if f didn't return anything, but we can't say since it's an expr... *)
        if args=[] then [Alloc 1] else List.(concat (rev_map (codeR rho) args))
        @ [Mark]
        @ codeR rho f
        @ [Call]
        @ [Slide (m-1)]

  and codeL rho = function
    | L.Var x -> (match E.find x rho.E.var with
        | E.Global, _, L.Fun _ -> [Loadcl ("_"^x)] (* we want to see the label for functions *)
        | E.Global, j, _ -> [Loadc j]
        | E.Local, j, _ -> [Loadrc j])
    | L.Deref e -> codeR rho e
    | L.Field (l, f) -> codeL rho l @ [Loadc (E.find f rho.E.field); Add]
    | L.Index (l, e) -> codeL rho l @ codeR rho e @ [Loadc (Type.size_of_elem rho l); Mul; Add]

  (* simple statements *)
  let rec codeS'' rho = function
    | L.Nop -> []
    | L.Continue -> [Jump (E.find "continue" rho.E.label)]
    | L.Break -> [Jump (E.find "break" rho.E.label)]
    | L.Expr e -> codeR rho e @ [Pop] (* TODO pop n, where n = Type.size_of_expr e, if we want to support returning structs *)
    | L.Block ss -> snd (codeS_fold (rho, []) ss)
    | L.Return None -> [Return]
    | L.Return (Some e) -> codeR rho e @ [Storer (-3); Return]
    | L.Local _ -> [] (* single decl. has no effect if there are no succ. stmts *)
    | _ -> assert false
  (* statements that need labels *)
  and codeS' rho =
    let label = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n in (* generates a fresh label *)
    function
    | L.IfThen (e, s') ->
        let a = label () in
        codeR rho e @ [Jumpz a] @ codeS' rho s' @ [Label a]
    | L.IfThenElse (e, s1, s2) ->
        let a = label () in let b = label () in
        codeR rho e @ [Jumpz a] @ codeS' rho s1 @ [Jump b; Label a] @ codeS' rho s2 @ [Label b]
    | L.While (e, s') ->
        let a = label () in let b = label () in
        [Label a] @ codeR rho e @ [Jumpz b] @ codeS' (E.continue_break a b rho) s' @ [Jump a; Label b]
    | L.DoWhile (s', e) ->
        let a = label () in let b = label () in
        [Label a] @ codeS' (E.continue_break a b rho) s' @ codeR rho e @ [Not; Jumpz a; Label b]
    | L.For (e1, e2, e3, s') ->
        let a = label () in let b = label () in let c = label () in
        codeR rho e1 @ [Pop; Label a] @ codeR rho e2 @ [Jumpz b] @ codeS' (E.continue_break c b rho) s' @ [Label c] @ codeR rho e3 @ [Pop; Jump a; Label b]
    | s -> codeS'' rho s
  (* statements that change rho *)
  and codeS rho = function
    | L.Local d -> E.(decl Local d rho), []
    | s -> rho, codeS' rho s
  and codeS_fold (rho,a) ss = List.fold_left (fun (rho,a) s -> let rho', i = codeS rho s in rho', a @ i) (rho,a) ss

  let codeG (rho, a) = function
    | L.Global d -> E.(decl Global d rho), a
    | L.FunDef (t, name, args, ss) ->
        (* let types_of_stmt s = let f = L.identity_folder in f.fold_stmt { f with dispatch_stmt = { f.dispatch_stmt with fold_Local = fun _ (t,_) a -> t::a } } s [] in *)
        let rec types_of_stmt = function L.Local (t,_) -> [t] | L.Block ss -> List.flat_map types_of_stmt ss | _ -> [] in
        let k = List.sum @@ List.map Type.size_of @@ List.flat_map types_of_stmt ss in
        let rho', is = codeS_fold (rho, []) ss in
        let sp_max =
          let sp_change = function
            | Neg | Not | Load | Jump _ | Halt | New | Call | Enter _ | Return | Storea _ | Storer _ | Label _ -> 0
            | Loadc _ | Loada _ | Loadrc _ | Loadr _ | Loadcl _ | Dup -> 1
            | Add | Sub | Mul | Div | Mod | And | Or | Eq | Neq | Leq | Le | Geq | Gr | Store | Pop | Jumpz _ -> (-1)
            | Mark -> 2
            | Alloc k -> k
            | Slide k -> -k
            | Move k -> k-1
          in
          (* List.(inits is |> map (sum % map sp_change) |> max) *)
          fst @@ List.fold_left (fun (m,a) i -> let r = a + (sp_change i) in max m r, r) (0, 0) is
        in
        let q = k + sp_max in
        rho', a @ [Label ("_"^name); Enter q; Alloc k] @ is @ [Return]

  let code ast =
    let k = List.sum @@ List.filter_map (function L.Global (t,_) -> Some (Type.size_of t) | _ -> None) ast in
    [Enter (k+4); Alloc (k+1); Mark; Loadcl "_main"; Call; Slide k; Halt] @
    snd @@ List.fold_left codeG (E.empty, []) ast
end
