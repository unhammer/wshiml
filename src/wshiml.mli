(** Word shingling for near duplicate document detection. *)

(** A sketch of one document. A sketch is the set of minimal elements
    that result from xor-ing the hash of shingles with a list of
    random hashes. *)
type sketch

(** List of sketched documents – the second part of the tuple is a
    document ID. *)
type sketchdocs = (sketch * int) list

(** The functions taking documents let you specify your own tokeniser.
    If a tokeniser is not provided, a very simple
    whitespace/punctuation tokeniser is used. *)
type tokeniser = bytes -> bytes list

(** The [n] in the below functions is the number of shingles to use;
    defaults to 4. *)

(** Compares two documents "exactly", by taking the Jacquard
    coefficient of the shingles themselves. *)
val compare_docs : ?tokenise:tokeniser -> ?n:int -> bytes -> bytes -> float

val sketch_of_doc : ?tokenise:tokeniser -> ?n:int -> bytes -> sketch

(** @param slurp_file If provided, called on each document before processing (lets you pass a list of filenames instead of a list of the full documents). *)
val sketch_docs : ?tokenise:tokeniser -> ?n:int -> ?slurp_file:(bytes -> bytes) -> bytes list -> sketchdocs


val supersketches : ?n:int -> sketchdocs -> sketchdocs


(** Document-pair (as indices) to score. *)
type scoreddocs = ((int * int) * int) list

val score_sketches : ?threshold:float -> sketchdocs -> scoreddocs

(** List of clusters; documents referred to by their index in the
    original list. *)
type clusters = int list list

(** @param ndocs Number of documents. If not provided, uses the highest index in [scoreddocs]. *)
val cluster_scores : ?ndocs:int -> scoreddocs -> clusters
