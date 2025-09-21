module SGA = Task1
module Rand = Util.Rand

(* this is how OCaml does their main func *)
let () =
  (* ------------------ predefined parameters ------------------ *)
  let problem_name = "maxones" in
  let population_size = 75 in
  let genome_length = 36 in
  let mutation_rate = 0.001 in
  let crossover_rate = 0.6 in
  let max_generations = 2_000_000 in

  (* convergence parameters *)
  let convergence_window = 1000 in
  let no_improve_threshold = 1e-6 in
  let high_fitness_ratio = 0.95 in

  (* ------------------ initialize random state ------------------ *)
  let rng_state = Rand.create () in

  (* ------------------ initialize population using a fitness function ------------------ *)
  let fitness_fn = SGA.max_ones_fitness in
  let population = SGA.population_init population_size genome_length rng_state fitness_fn in

  (* ------------------ print one-time header ------------------ *)
  Printf.printf "%s  %d  %d  %g  %g\n"
    problem_name population_size genome_length mutation_rate crossover_rate;

  (* ------------------ helper functions ------------------ *)

  let average_fitness (pop : SGA.population_t) =
    Array.fold_left (fun acc (g : SGA.genome_t) -> acc +. g.fitness) 0.0 pop.members
    /. float_of_int pop.size
  in

  let champion_fitness (pop : SGA.population_t) =
    Array.fold_left (fun acc (g : SGA.genome_t) -> max acc g.fitness) neg_infinity pop.members
  in

  let same_fitness_ratio (pop : SGA.population_t) =
    let freq = Hashtbl.create pop.size in
    Array.iter (fun (g : SGA.genome_t) ->
      let f = g.fitness in
      Hashtbl.replace freq f ((try Hashtbl.find freq f with Not_found -> 0) + 1)
    ) pop.members;
    let max_count =
      Hashtbl.fold (fun _ v acc -> max v acc) freq 0
    in
    float_of_int max_count /. float_of_int pop.size
  in

  (* ------------------ evolution loop ------------------ *)

  let final_population, termination_reason =
    let rec evolve current_pop generation last_avg_fitness stagnant_generations =
      if generation > max_generations then (current_pop, "safety limit exceeded")
      else
        let new_pop =
          SGA.make_new_generation_with current_pop mutation_rate crossover_rate rng_state fitness_fn
        in

        let avg_fit = average_fitness new_pop in
        let champ_fit = champion_fitness new_pop in
        let ratio = same_fitness_ratio new_pop in

        (* print per-generation statistics *)
        Printf.printf "%d %f %f %f\n" generation champ_fit avg_fit (ratio *. 100.0);

        let stagnant_generations =
          if abs_float (avg_fit -. last_avg_fitness) < no_improve_threshold then
            stagnant_generations + 1
          else 0
        in

        if stagnant_generations >= convergence_window || ratio >= high_fitness_ratio then
          (new_pop, "population converged")
        else
          evolve new_pop (generation + 1) avg_fit stagnant_generations
    in
    evolve population 1 (average_fitness population) 0
  in

  (* ------------------ final output ------------------ *)
  Printf.printf "%s\n" termination_reason;
  SGA.print_population final_population
