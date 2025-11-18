(*
Assignment 3 SGA: Parameter Tuning for Simple Genetic Algorithm on DeJong's Test Suite Function 2
*)

module Rand = Util.Rand
module Out = Util.Output

(* Configuration parameters *)
let search_min = -5.12
let search_max = 5.11
let bits_per_dimension = 16
let sga_fixed_generations = 10000
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

(* SGA Implementation *)
type sga_genome_t = {
    length : int;
    mutable fitness : float;
    string : bool array;
}

type sga_population_t = { size : int; members : sga_genome_t array }

(* Decode entire SGA genome into float array *)
let sga_decode_genome_full genome dimensions =
  Array.init dimensions (fun i ->
    let start_bit = i * bits_per_dimension in
    let end_bit = (i + 1) * bits_per_dimension - 1 in
    let decimal_value = ref 0.0 in
    let max_decimal_value = ref 0.0 in
    let bit_count = ref 0 in
    for bit_pos = end_bit downto start_bit do
      if genome.string.(bit_pos) then
        decimal_value := !decimal_value +. (2.0 ** float_of_int !bit_count);
      max_decimal_value := !max_decimal_value +. (2.0 ** float_of_int !bit_count);
      incr bit_count
    done;
    search_min +. (!decimal_value /. !max_decimal_value *. (search_max -. search_min))
  )

let sga_fitness genome dimensions =
  let x = sga_decode_genome_full genome dimensions in
  dejong_f2 x

let sga_genome_init size rng_state fitness_fn =
  let bits = Array.init size (fun _ -> Rand.bool rng_state) in
  let genome = { length = size; fitness = 0.0; string = bits } in
  { genome with fitness = fitness_fn genome }

let sga_population_init member_count member_length rng_state fitness_fn =
  {
    size = member_count;
    members = Array.init member_count (fun _ -> sga_genome_init member_length rng_state fitness_fn);
  }

let sga_copy_genome g = { length = g.length; fitness = g.fitness; string = Array.copy g.string }

let sga_genome_mutate genome mutation_rate rng_state fitness_fn =
  for i = 0 to genome.length - 1 do
    if Rand.uniform rng_state < mutation_rate then
      genome.string.(i) <- not genome.string.(i)
  done;
  genome.fitness <- fitness_fn genome

let sga_genome_1P_crossover genome1 genome2 rng_state fitness_fn =
  let child1 = Array.copy genome1.string in
  let child2 = Array.copy genome2.string in
  let split_index = int_of_float (floor (Rand.uniform_range rng_state 1.0 (float_of_int genome1.length -. 1.0))) in
  for i = split_index to genome1.length - 1 do
    child1.(i) <- genome2.string.(i);
    child2.(i) <- genome1.string.(i)
  done;
  let g1 = { length = genome1.length; fitness = 0.0; string = child1 } in
  let g2 = { length = genome2.length; fitness = 0.0; string = child2 } in
  ({ g1 with fitness = fitness_fn g1 }, { g2 with fitness = fitness_fn g2 })

(* Tournament selection for better performance in high dimensions *)
let tournament_select members pop_size tournament_size rng_state =
  let tournament = Array.init tournament_size (fun _ ->
    let idx = Rand.int rng_state 0 (pop_size - 1) in
    members.(idx)
  ) in
  (* For minimization: find the individual with lowest fitness *)
  Array.fold_left (fun best g -> if g.fitness < best.fitness then g else best) tournament.(0) tournament

let sga_make_new_generation curr_pop mutation_rate crossover_rate rng_state fitness_fn =
  let pop_size = curr_pop.size in
  let members = curr_pop.members in
  let tournament_size = 5 in

  (* Elitism: preserve the best individual *)
  let sorted_members = Array.copy members in
  Array.sort (fun a b -> compare a.fitness b.fitness) sorted_members;
  let elite = sga_copy_genome sorted_members.(0) in

  let new_members = Array.make pop_size { length = 0; fitness = 0.0; string = [||] } in

  (* Elitism: keep the best individual *)
  new_members.(0) <- elite;

  (* Generate rest of population using tournament selection *)
  for i = 1 to pop_size - 1 do
    let parent1 = tournament_select members pop_size tournament_size rng_state in
    let parent2 = tournament_select members pop_size tournament_size rng_state in

    let child = if Rand.uniform rng_state < crossover_rate then
      let c1, c2 = sga_genome_1P_crossover parent1 parent2 rng_state fitness_fn in
      if Rand.uniform rng_state < 0.5 then c1 else c2
    else
      sga_copy_genome (if Rand.uniform rng_state < 0.5 then parent1 else parent2)
    in

    (* Apply mutation *)
    sga_genome_mutate child mutation_rate rng_state fitness_fn;

    new_members.(i) <- child
  done;

  { size = pop_size; members = new_members }

let sga_champion_fitness pop =
  Array.fold_left (fun acc g -> min acc g.fitness) infinity pop.members

(* Run single SGA experiment with dimension-adaptive parameters *)
let run_sga_experiment dimensions pop_size base_mutation_rate base_crossover_rate seed =
  let rng_state = Rand.create_with_seed seed in
  let genome_length = dimensions * bits_per_dimension in
  let fitness_fn = fun genome -> sga_fitness genome dimensions in
  let population = sga_population_init pop_size genome_length rng_state fitness_fn in

  (* Scale mutation rate with dimensions: lower mutation for higher dimensions *)
  let mutation_rate = base_mutation_rate /. (sqrt (float_of_int dimensions)) in
  (* Slightly increase crossover for higher dimensions *)
  let crossover_rate = min 0.9 (base_crossover_rate +. (float_of_int dimensions -. 2.0) *. 0.05) in

  let rec evolve pop gen =
    if gen >= sga_fixed_generations then pop
    else
      let new_pop = sga_make_new_generation pop mutation_rate crossover_rate rng_state fitness_fn in
      evolve new_pop (gen + 1)
  in

  let final_pop = evolve population 0 in
  sga_champion_fitness final_pop

(* Main experiment runner *)
let () =
  Out.set_both_output "assignment3_sga_results.txt";
  let _ = Out.open_output "assignment3_sga_results.txt" in

  Out.printf "# Assignment 3 SGA Parameter Tuning Results\n";
  Out.printf "# Format: SGA dimensions pop_size mutation_rate crossover_rate utility\n";

  let dimensions_list = [2; 3; 5; 10; 20] in
  let pop_sizes = [100; 500; 1000; 2000] in
  let mutation_rates = [0.05; 0.1; 0.2; 0.3] in
  let crossover_rates = [0.5; 0.7; 0.8; 0.9] in

  List.iter (fun dimensions ->
    List.iter (fun pop_size ->
      List.iter (fun mutation_rate ->
        List.iter (fun crossover_rate ->
          let utilities = List.init num_runs (fun run ->
            run_sga_experiment dimensions pop_size mutation_rate crossover_rate (run + 1)
          ) in
          let avg_utility = (List.fold_left (+.) 0.0 utilities) /. float_of_int num_runs in
          Out.printf "SGA %d %d %g %g %g\n" dimensions pop_size mutation_rate crossover_rate avg_utility
        ) crossover_rates
      ) mutation_rates
    ) pop_sizes
  ) dimensions_list;

  Out.close_output ()