open Ymse

module Bset = Set.Make(Bytes)
module Hset = Set.Make(struct type t = int let compare = compare end)

module PiMap = struct
  include Map.Make(struct type t = int * int let compare = compare end)
  let append k v map =
    try
      let old = find k map in add k (v::old) map
    with
      Not_found -> add k [v] map
end

module DupeMap = struct
  include Map.Make(struct type t = int Couple.t let compare = compare end)
  let bump map k =
    try
      let old = find k map in add k (old+1) map
    with
      Not_found -> add k 1 map
end


let simple_tokenise_on =
  Bytes.to_set " \n\t.-;:?+,()[]{}<>\\/$%&#@*~_«»"

let simple_tokenise doc =
  Bytes.split (fun c -> Cset.mem c simple_tokenise_on) doc


let jacquard s1 s2 =
  let f_len set = Bset.cardinal set |> float_of_int in
  let l_inter = f_len (Bset.inter s1 s2) in
  let l_union = f_len (Bset.union s1 s2) in
  l_inter /. l_union

let jacquard_hash s1 s2 =
  let f_len set = Hset.cardinal set |> float_of_int in
  let l_inter = f_len (Hset.inter s1 s2) in
  let l_union = f_len (Hset.union s1 s2) in
  l_inter /. l_union

let shingles n l =
  let rec aux todo acc =
    if List.length todo <= n then
      todo::acc
    else let shingle = List.take n todo in
      aux (List.tl todo) (shingle::acc)
  in
  aux l []

let shingleset_of_doc ?(tokenise=simple_tokenise) ?(n=4) doc =
  tokenise doc
  |> shingles n
  |> List.map (Bytes.concat " ")
  |> Bset.of_list

let compare_docs ?(tokenise=simple_tokenise) ?(n=4) d1 d2 =
  if d1 = d2 then 1.0
  else
    let prep d = shingleset_of_doc ~tokenise ~n d in
    jacquard (prep d1) (prep d2)

(* If we want to extract potential duplicates from many documents,
   comparing the cross product of all documents gets real slow.
   Instead, we do a probabilistic comparison of the hashes of the
   shingles. *)
let hashes_of_doc ?(tokenise=simple_tokenise) ?(n=4) doc =
  tokenise doc
  |> shingles n
  |> List.map Hashtbl.hash

(* If π is a random permutation from hashest to hashes, then Π(d) is
   the set of permuted hashes in H(d); so for each h ∈ H(d) there is
   a corresponding π(h) ∈ Π(d).

   Since we can have several random permutations, an actual π is
   parametrised by random number `r` *)
let n_permutations = 200
let randoms =
  Random.self_init ();
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (Hashtbl.hash (Random.int64 (Int64.max_int))::acc) (n-1)
    (* Call .hash on the random to make sure it's in the same range as
       other .hash values *)
  in
  aux [] n_permutations
(* TODO: randoms should probably be parametrised somehow (at the very
   least it should be possible to give a seed, for testing), but it's
   important that the same list is reused for each document in order
   for the comparisons to make sense. Need a safe way to ensure caller
   doesn't mess that up. *)

let pi r hash = r lxor hash

(* Randomiser and the minimal element of the random-permuted hash *)
type minpi = int * int

let min_pi hashes r =
  let min_elt =
    List.fold_left (fun min_elt hash ->
        let elt = pi r hash in
        if elt < min_elt then elt else min_elt)
      max_int
      hashes
  in
  (r, min_elt)

(* The probability of two documents having the same minimal element
   in Π for some r, is the same as the probability of them having
   the same Jacquard coefficient. Thus we compute Π repeatedly (for
   200 values of `r`), and pick the minimal element xr₁ from each
   set; this is the sketch φ(d) of d. Our estimate of the Jacquard
   coefficient is now |φ₁ ∩ φ₂|/200. *)
type sketch = minpi list
type tokeniser = bytes -> bytes list
let sketch_of_doc ?(tokenise=simple_tokenise) ?(n=4) doc =
  let hashes = hashes_of_doc ~tokenise ~n doc in
  List.map (min_pi hashes) randoms

type sketchdocs = (sketch * int) list

let id x = x

let sketch_docs ?(tokenise=simple_tokenise) ?(n=4) ?(slurp_file=id) docs =
  List.mapi
    (fun i_d path -> (sketch_of_doc ~tokenise ~n (slurp_file path), i_d))
    docs

(* TODO: For speedup: only compare those documents that have a
   _supershingle_ in common. *)
let supershingles ?(n=4) sketch =
  List.sort compare sketch |> shingles n

let hashes_of_sketch ?(n=4) sketch =
  supershingles ~n sketch
  |> List.map Hashtbl.hash

let supersketch ?(n=4) sketch =
  let hashes = hashes_of_sketch ~n sketch in
  List.map (min_pi hashes) randoms

let supersketches ?(n=4) sketches =
  List.map (fun (sketch,i_d) -> supersketch sketch, i_d) sketches

(* Now we create a map from minimal elements xr₁ to the documents
   (indices) that contain this element: *)
let pimap_of_sketches sketches =
  List.fold_left
    (fun map (s, i_d) ->
       List.fold_left (fun m' minpi -> PiMap.append minpi i_d m')
         map
         s)
    PiMap.empty
    sketches

(* … and then we reverse that to a map from document-pairs to the
   number of shingles they have in common:*)
let dupescore_of_pimap pimap =
  PiMap.fold (fun minpi docs map ->
      List.allpairs docs
      |> List.map Couple.create
      |> List.fold_left DupeMap.bump map
    ) pimap DupeMap.empty
  |> DupeMap.bindings
  |> List.sort (fun (_,c1) (_,c2) -> compare c2 c1)

(* This should let us start clustering those documents that have more
   than a certain threshold of elements in common. *)
type scoreddocs = ((int * int) * int) list

let score_sketches ?(threshold=0.8) sketches =
  let threshold = float_of_int n_permutations *. threshold |> int_of_float in
  pimap_of_sketches sketches
  |> dupescore_of_pimap
  |> List.filter (fun (_,s) -> s > threshold)
  |> List.map (fun (k,s) -> (Couple.get k, s))

type clusters = int list list

let top_doc_ind scores =
  List.map fst scores |> List.fold_left (fun top (a,b) -> max top (max a b)) 0

let cluster_scores ?(ndocs=0) scores =
  let ndocs = if ndocs > 0 then ndocs else top_doc_ind scores in
  let ds = DisjointSet.init ndocs in
  List.map fst scores (* ignore scores! anything surviving threshold is A-ok *)
  |> List.iter (DisjointSet.union ds);
  DisjointSet.to_lists ds
