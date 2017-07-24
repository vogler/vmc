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

(* can't use let rec here... *)
let tie f g = fix (fun f' -> f (g f')), fix (fun g' -> g (f g'))

let parse p input = match parse_only p (`String input) with
  | Ok v      -> v
  | Error msg -> failwith msg

let test name p show inputs =
  print_endline ("Parser " ^ name ^ ":");
  List.iter (fun input -> print_endline (input ^ " -> " ^ try show (parse p input) with Failure s -> "fail:" ^ s)) inputs;
  print_endline ""
