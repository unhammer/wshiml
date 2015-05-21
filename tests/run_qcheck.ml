
let rem_some threshold =
  let open QCheck.Arbitrary in
  let rec aux acc =
    function
    | [] -> return (List.rev acc)
    | hd::tl ->
      float 1.0 >>= (fun f ->
          if f<threshold then aux acc tl
          else aux (hd::acc) tl)
  in aux []

let pair_rem_some threshold l =
  let open QCheck.Arbitrary in
  rem_some threshold l >>= (fun l2 -> return (l,l2))

let document = QCheck.Arbitrary.(list ~len:(int_range ~start:700 ~stop:1500) string)

let compare_score blists =
  let docs = List.map (Bytes.concat " ") blists in
  Wshiml.sketch_docs ~n:3 docs |> Wshiml.score_sketches

let tests = [ QCheck.mk_test ~n:100
                ~name:"random_docs_differ"
                QCheck.Arbitrary.(list_repeat 5 document)
                (fun blists ->
                   let scores = compare_score blists in
                   List.length scores = 0)
              ;
              QCheck.mk_test ~n:100
                ~name:"still_similar_after_some_removals"
                QCheck.Arbitrary.(document >>= pair_rem_some 0.01)
                (fun (l1,l2) ->
                   let scores = compare_score [l1; l2] in
                   List.length scores = 1)
            ]


let () =
  QCheck.run_tests tests |> ignore
