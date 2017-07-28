open Batteries
(* fix some annoying stuff... *)
module List = struct
  include List
  let flat_map f xs = flatten (map f xs)
  let sum xs = if xs=[] then 0 else sum xs
  let rec inits = function [] -> [[]] | x::xs -> [] :: map (cons x) (inits xs)
end
(* include Batteries without List... *)
(* include (Batteries : module type of Batteries with module List := Batteries.List) *)
