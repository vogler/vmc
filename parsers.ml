open Batteries
include Angstrom
(* custom combinators *)
let from p xs = choice (List.map (fun (a,b) -> p a *> return b) xs)
let ( *>> ) p v = p *> return v
let (|>>) pa pf = pa >>= fun a -> pf >>| fun f -> f a
let (^^) = lift2 (^)
let some p = p >>| (fun x -> Some x) <|> return None
(* common parsers *)
let ws = skip_while (function '\x20' | '\x0a' | '\x0d' | '\x09' -> true | _ -> false)
let sws p i = ws *> p i <* ws (* we always skip whitespace... *)
let sws0 p = ws *> p <* ws (* for parsers w/o arguments *)
let char = sws char
let string = sws string
let surround a b p = char a *> p <* char b
let parens p = surround '(' ')' p
let brackets p = surround '[' ']' p
let curly p = surround '{' '}' p
let (|||) f g x = f x || g x
let p_digit = function '0'..'9' -> true | _ -> false
let p_nondigit = function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false
let digit = take_while1 p_digit <?> "digit"
let nondigit = take_while1 p_nondigit <?> "nondigit"
let int = sws0 digit >>| int_of_string <?> "int"

let identifier = sws0 @@ take_while1 p_nondigit ^^ take_while (p_digit ||| p_nondigit) <?> "identifier"

let lrec hd tl = (* hd is the base case parser, tl is the left-recursive parser *)
  hd |>> (fix (fun r -> tl (fun f -> r >>| (fun rf -> rf % f)) <|> return identity))

let chain f lp = fix (fun rp -> f lp rp <|> lp)

let unopr op lp = (* right-associative unop *)
    lp >>= fun l -> (op <*> (return l)) <|> return l

let chainl op lp = (* left-associative binop *)
  let rest = fix (fun rest -> return (fun x -> (rest >>= fun r -> (op <*> (return x) <*> lp) >>= fun f -> r f) <|> return x)) in
  lp >>= fun l -> rest >>= fun r -> r l

(* let chainl op lp = (* left-associative binop *) *)
(*   let rest = fix (fun rest -> return (fun x -> (rest <*> (op <*> (return x) <*> lp)) <|> return x)) in *)
(*   rest <*> lp >>= fun x -> x *)

let chainr op lp = (* right-associative binop *)
  fix (fun rp ->
    lp >>= fun l -> (op <*> (return l) <*> rp) <|> return l)

(* can't use let rec here... *)
let tie f g = fix (fun f' -> f (g f')), fix (fun g' -> g (f g'))

let parse p input = parse_only p (`String input)
let parse_exc p input = match parse p input with
  | Ok v      -> v
  | Error msg -> failwith msg

let test name p show tests_ok tests_fail =
  print_endline ("Tests for parser " ^ name ^ ":");
  let passed = ref 0 in
  let clear_line () = print_string ("\027[2K\r") in
  let tests = List.map (Tuple2.map2 Option.some) tests_ok @ List.map (fun x -> Tuple2.make x None) tests_fail in
  let test_one (input, expected) =
    print_string @@ "Testing "^input;
    match parse p input, expected with
    | Ok a, Some b ->
        if a = b then (
          incr passed;
          clear_line ()
        ) else (
          print_endline @@ " -> expected " ^ show b ^ ", but got " ^ show a
        )
    | Ok a, None ->
        print_endline @@ " -> expected fail, but got " ^ show a
    | Error msg, Some b ->
        print_endline @@ " -> expected " ^ show b ^ ", but parser failed with " ^ msg
    | Error msg, None ->
        incr passed;
        clear_line ()
  in
  List.iter test_one tests;
  let passed = !passed in
  let total = List.length tests in
  let green s = "\027[0;32m" ^ s ^ "\027[0;0;00m" in
  let red s = "\027[0;31m" ^ s ^ "\027[0;0;00m" in
  Printf.printf "-> %s (passed %i/%i tests)\n\n" (if passed = total then green "OK" else red "FAIL") passed total;
