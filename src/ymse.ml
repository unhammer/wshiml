
module List = struct
  include List

  let take_rev n l =
    let rec aux acc = function
      | 0, [] -> [] (* trying to take more than what's there: give nothing *)
      | n, [] -> acc
      | 0, _ -> acc
      | n, hd::tl -> aux (hd::acc) ((n-1),tl)
    in
    aux [] (n, l)
end

module Cset = Set.Make(Char)

module Bytes = struct
  include Bytes

  let split p s =
    let slice i j = sub s i (j-i) in
    let rec aux i j acc =
      if j < length s then
        begin
          let elt = get s j in
          let j1 = j + 1 in
          if p elt then
            aux j1 j1 (slice i j::acc)
          else
          aux i j1 acc
        end
      else (slice i j::acc)
    in
    aux 0 0 []
    |> List.filter (fun s -> length s > 0)
    |> List.rev

  let to_set bytes =
    let rec aux i acc =
      if i < length bytes then
        aux (i+1) (Cset.add (get bytes i) acc)
      else acc
    in
    aux 0 Cset.empty
end

module Couple : sig
  (* A "sorted pair" / two-element set, so (a,b)=(b,a) *)
  type 'a t [@@deriving show]
  val create : 'a * 'a -> 'a t
  val get: 'a t -> 'a * 'a
end = struct
  type 'a t = 'a * 'a [@@deriving show]
  let create (a, b) = if a < b then (a,b) else (b,a)
  let get a = a
end


module DisjointSet : sig
  (* From https://www.cs.cornell.edu/courses/CS3110/2012fa/recitations/rec14.html *)

  (* type node *)
  type node = { mutable parent : int; mutable rank : int }
  type t = node array

  val union : t -> int * int -> unit
  val init : int -> t
  val find : t -> int -> int

  val to_lists : t -> int list list

end = struct

  type node = { mutable parent : int; mutable rank : int }
  type t = node array

  let empty_node parent = { parent; rank = 0 }

  let init size = Array.init size empty_node

  let rec find s e =
    let n = s.(e) in
    if n.parent = e then e
    else
      (n.parent <- (find s n.parent);
       n.parent)

  let union s (e1, e2) =
    let r1 = find s e1 and r2 = find s e2 in
    let n1 = s.(r1) and n2 = s.(r2) in
    if r1 != r2 then
      if n1.rank < n2.rank then
        n1.parent <- r2
      else if n1.rank > n2.rank then
        n2.parent <- r1
      else
        (n2.parent <- r1; n1.rank <- n1.rank + 1)

  (* A little helper to turn DisjointSet's into lists of lists *)
  module DSMap = struct
    include Map.Make(struct type t = int let compare = compare end)
    let append k v map =
      try
        let old = find k map in add k (v::old) map
      with
        Not_found -> add k [v] map
  end

  let to_lists s =
    let rec aux i map =
      if i >= Array.length s then map
      else
        let map = DSMap.append (find s i) i map in
        aux (i+1) map
    in
    aux 0 DSMap.empty |> DSMap.bindings |> List.map snd

end
