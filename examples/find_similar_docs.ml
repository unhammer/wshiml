let slurp_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  let rl = try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      !lines
  in
  List.rev rl |> Bytes.concat "\n"

let print_clusters (files: bytes array) (clusters: Wshiml.clusters) =
  List.filter (function a::b::_ -> true | _ -> false) clusters
  |> List.map (fun l -> List.map (Array.get files) l |> Bytes.concat "\t")
  |> Bytes.concat "\n"
  |> print_endline

let print_scores =
  List.iter (fun ((d1,d2),score) -> Printf.printf "%d & %d = %d\n" d1 d2 score)

let warn_on_dir =
  List.iter (fun (f) -> if Sys.is_directory f then (
    Printf.eprintf "find-similar-docs: '%s' is a directory, expected files.\nTry e.g. \n    find . -type f -print0 | xargs -0 find-similar-docs -s\n" f;
    exit(1)))

let find_similar_docs threshold super files =
  warn_on_dir files;
  let files_a = Array.of_list files in
  let ndocs = Array.length files_a in
  Wshiml.sketch_docs ~slurp_file files
  |> (fun sketch -> if super then Wshiml.supersketches sketch else sketch)
  |> Wshiml.score_sketches ~threshold
  (* DEBUG: *)
  (* |> (fun scores -> print_scores scores; print_endline ""; scores) *)
  |> Wshiml.cluster_scores ~ndocs
  |> print_clusters files_a


open Cmdliner

let super =
  let doc = "Create a super-shingle (shingle of shingles) before clustering. May speed up clustering." in
  Arg.(value & flag & info ["s"; "super"] ~docv:"SUPER" ~doc)

let threshold =
  let doc = "The overlap threshold for considering two documents similar." in
  Arg.(value & opt float 0.8 & info ["t"; "threshold"] ~docv:"THRESHOLD" ~doc)

let files =
  let doc = "The documents to compare." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

let fnd_t = Term.(pure find_similar_docs $ threshold $ super $ files)

let info =
  let doc = "find similar or near duplicate documents" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) uses word shingling to find near duplicate or similar
        documents among files specified on the command line.";
    `P "The output is tab-separated, one cluster per line.";
    `P "Note that the comparison is probabilistic, and results may
        differ between runs.";
    `S "EXAMPLES";
    `P "find . -type f -size -50k -print0 | xargs -0 $(tname) -s";
    `P "Find similar documents among small files under the current
        directory; use super-shingling to speed things up.";
    `S "BUGS";
    `P "Email bug reports to <unhammer at fsfe.org>.";
  ] in
  Term.info "find-similar-docs" ~version:"0.1.0" ~doc ~man

let () =
  match Term.eval (fnd_t, info) with `Error _ -> exit 1 | _ -> exit 0
