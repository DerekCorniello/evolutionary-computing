(*
Assignment 3 ES: Parameter Tuning for Evolution Strategy on DeJong's Test Suite Function 2
*)

module Rand = Util.Rand
module Out = Util.Output

(* Configuration parameters *)
let search_min = -5.12
let search_max = 5.11
let es_fixed_generations = 1000
let num_runs = 5

(* DeJong's Test Suite Function 2: Generalized Rosenbrock *)
let dejong_f2 (x : float array) : float =
  let n = Array.length x in
  let sum = ref 0.0 in
  for i = 0 to n - 2 do
    let xi = x.(i) in
    let xi_plus_1 = x.(i + 1) in
    sum := !sum +. (100.0 *. ((xi_plus_1 -. (xi ** 2.0)) ** 2.0)) +. ((xi -. 1.0) ** 2.0)
  done;
  !sum

(* ES Implementation *)
type es_genome_t = {
    es_fitness : float;
    position : float array;
    std_devs : float array;
    tau : float;  (* Self-adaptation parameter *)
}

type es_population_t = { size : int; [@warning "-69"] members : es_genome_t array }

let es_genome_init dimensions rng_state base_sigma =
  let pos = Array.init dimensions (fun _ -> Rand.uniform_range rng_state search_min search_max) in
  let std_devs = Array.make dimensions base_sigma in
  let tau = 1.0 /. sqrt (float_of_int dimensions) in  (* Self-adaptation parameter *)
  let genome = { es_fitness = 0.0; position = pos; std_devs = std_devs; tau = tau } in
  { genome with es_fitness = dejong_f2 genome.position }

let es_population_init dimensions rng_state mu base_sigma =
  { size = mu; members = Array.init mu (fun _ -> es_genome_init dimensions rng_state base_sigma) }

let es_champion_fitness pop =
  Array.fold_left (fun acc g -> min acc g.es_fitness) infinity pop.members

let gaussian_mutation dimensions rng_state genome =
  (* Self-adaptive mutation: evolve mutation strengths with bounds *)
  let tau_factor = exp (min 1.0 (max (-1.0) (genome.tau *. Rand.gaussian rng_state 0.0 1.0))) in
  let new_tau = genome.tau *. tau_factor in

  let new_std_devs = Array.map (fun sigma ->
    let factor = exp (min 1.0 (max (-1.0) (genome.tau *. Rand.gaussian rng_state 0.0 1.0))) in
    max 1e-10 (min 10.0 (sigma *. factor))  (* Bound sigma between 1e-10 and 10.0 *)
  ) genome.std_devs in

  (* Mutate position using adapted mutation strengths *)
  let new_pos = Array.init dimensions (fun i ->
    let mutated = genome.position.(i) +. Rand.gaussian rng_state 0.0 (new_std_devs.(i) ** 2.0) in
    max search_min (min search_max mutated)
  ) in

  { es_fitness = dejong_f2 new_pos;
    position = new_pos;
    std_devs = new_std_devs;
    tau = new_tau }

let intermediate_recombination dimensions rng_state genome1 genome2 =
  let new_pos = Array.init dimensions (fun i ->
    Rand.gaussian_between rng_state genome1.position.(i) genome2.position.(i)
  ) in
  let new_std_devs = Array.init dimensions (fun i ->
    (genome1.std_devs.(i) +. genome2.std_devs.(i)) /. 2.0
  ) in
  let new_tau = (genome1.tau +. genome2.tau) /. 2.0 in
  { es_fitness = dejong_f2 new_pos;
    position = new_pos;
    std_devs = new_std_devs;
    tau = new_tau }

(* Run single ES experiment *)
let run_es_experiment dimensions mu lambda sigma seed =
  let rng_state = Rand.create_with_seed seed in
  let population = es_population_init dimensions rng_state mu sigma in

  let final_pop = ref population in
  for _ = 0 to es_fixed_generations - 1 do
    let offspring = Array.init lambda (fun _ ->
      let p1 = (!final_pop).members.(Rand.int rng_state 0 (mu - 1)) in
      let p2 = (!final_pop).members.(Rand.int rng_state 0 (mu - 1)) in
      let recombined = intermediate_recombination dimensions rng_state p1 p2 in
      gaussian_mutation dimensions rng_state recombined
    ) in
    let combined = Array.append (!final_pop).members offspring in
    Array.sort (fun a b -> compare a.es_fitness b.es_fitness) combined;
    let new_members = Array.sub combined 0 mu in
    final_pop := { size = mu; members = new_members }
  done;
  es_champion_fitness !final_pop

(* Main experiment runner *)
let () =
  Out.set_both_output "assignment3_es_results.txt";
  let _ = Out.open_output "assignment3_es_results.txt" in

  Out.printf "# Assignment 3 ES Parameter Tuning Results\n";
  Out.printf "# Format: ES dimensions mu lambda sigma utility\n";

  let dimensions_list = [2; 3; 5; 10; 20] in
  let mu_values = [10; 15; 20] in
  let lambda_values = [50; 100; 150] in
  let sigma_values = [0.1; 0.5; 1.0; 2.0] in

  List.iter (fun dimensions ->
    List.iter (fun mu ->
      List.iter (fun lambda ->
        List.iter (fun sigma ->
          let utilities = List.init num_runs (fun run ->
            run_es_experiment dimensions mu lambda sigma (run + 1)
          ) in
          let avg_utility = (List.fold_left (+.) 0.0 utilities) /. float_of_int num_runs in
          Out.printf "ES %d %d %d %g %g\n" dimensions mu lambda sigma avg_utility
        ) sigma_values
      ) lambda_values
    ) mu_values
  ) dimensions_list;

  Out.close_output ()