let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed
let seed =
   let res = Array.make (String.length seed / 2) 0 in
   for i = 0 to (String.length seed / 2) - 1
   do res.(i) <- (Char.code seed.[i * 2] lsl 8) lor (Char.code seed.[i * 2 + 1]) done ;
   res

let () =
  let random_seed = seed in
  Random.full_init random_seed

let random length =
  let get _ =
    match Random.int (10 + 26 + 26) with
    | n when n < 10 -> Char.(chr (code '0' + n))
    | n when n < 10 + 26 -> Char.(chr (code 'a' + n - 10))
    | n -> Char.(chr (code 'A' + n - 10 - 26)) in
  String.init length get

open Bechamel
open Toolkit

let hash_eq_0 = random 4096
let hash_eq_1 = Bytes.to_string (Bytes.of_string hash_eq_0)

let hash_neq_0 = random 4096
let hash_neq_1 =
  let rec go limit =
    if limit <= 0 then failwith "Impossible to generate different hashes." ;
    let res = random 4096 in
    if res = hash_neq_0 then go (pred limit) else res in
  go 10

(* let ( <.> ) *)
(* function equal done*)
let test_equal0 =
  Test.make ~name:"eqaf equal equal" (Staged.stage @@ fun () -> Eqaf.equal hash_eq_0 hash_eq_1)
let test_equal1 =
  Test.make ~name:"eqaf equal not equal" (Staged.stage @@ fun () -> Eqaf.equal hash_neq_0 hash_neq_1)

let test_equal2 =
  Test.make ~name:"string equal equal" (Staged.stage @@ fun () -> String.equal hash_eq_0 hash_eq_1)

let test_equal3 =
  Test.make ~name:"string equal not equal" (Staged.stage @@ fun () -> String.equal hash_neq_0 hash_neq_1)

(* function compare done*)
let test_compare0 =
  Test.make ~name:"eqaf compare equal" (Staged.stage @@ fun () -> Eqaf.compare_be hash_eq_0 hash_eq_1)
let test_compare1 =
  Test.make ~name:"eqaf compare not equal" (Staged.stage @@ fun () -> Eqaf.compare_be hash_neq_0 hash_neq_1)

let test_compare2 =
  Test.make ~name:"string compare equal" (Staged.stage @@ fun () -> String.compare hash_eq_0 hash_eq_1)

let test_compare3 =
  Test.make ~name:"string compare not equal" (Staged.stage @@ fun () -> String.compare hash_neq_0 hash_neq_1)


let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in

  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 0.5)
      ~kde:(Some 1000) ()
  in
  let test_equal = Test.make_grouped ~name:"equal" ~fmt:"%s %s" [ test_equal0; test_equal1; test_equal2; test_equal3 ] 
  in 
  let test_compare = Test.make_grouped ~name:"compare" ~fmt:"%s %s" [ test_compare0; test_compare1; test_compare2; test_compare3 ] 
  in
  let raw_results =
    Benchmark.all cfg instances
    (Test.make_grouped ~name:"equal" ~fmt:"%s %s" [ test_equal; test_compare ])
  in  
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let nothing _ = Ok ()

let () =
  let results = benchmark () in
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) nothing ~compare:String.compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      results
  in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err