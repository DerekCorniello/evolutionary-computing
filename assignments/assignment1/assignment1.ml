module SGA = Task1
module Rand = Util.Rand

(* define the fitness function types and names for easier printing,
   yes, I know it looks dumb to have all of these, but it makes the
   usage of this much easier and readable*)

(* this weird @warning thing is for suppressing warnings if the
   type is not constructed. we expect this because we will only use 
   one of these at a time. *)
type fitness_type =
    | HalfOnes [@warning "-37"]
    | QuarterOnes [@warning "-37"]
    | MaxOnes [@warning "-37"]
    | Rosenbrock [@warning "-37"]

let fitness_name = function
    | HalfOnes -> "half-ones"
    | QuarterOnes -> "quarter-ones"
    | MaxOnes -> "max-ones"
    | Rosenbrock -> "rosenbrock"

(* get the actual function from the variant *)
let get_fitness_fn = function
    | HalfOnes -> SGA.half_ones_fitness
    | QuarterOnes -> SGA.quarter_ones_fitness
    | MaxOnes -> SGA.max_ones_fitness
    | Rosenbrock -> SGA.rosenbrock_fitness

(* this is how OCaml does their main func *)
let () =
    (* ------------------ predefined parameters ------------------ *)
    let population_size = 1000 in
    let genome_length = 32 in
    let mutation_rate = 0.01 in
    let crossover_rate = 0.5 in
    let max_generations = 2_000_000 in

    (* convergence parameters *)
    let convergence_window = 500 in
    let no_improve_threshold = 1e-4 in
    let significant_improve_threshold = 1e-3 in
    let high_fitness_ratio = 0.95 in
    (* stop if 95% of population has same fitness *)

    (* ------------------ initialize random state ------------------ *)
    let rng_state = Rand.create () in

    (* NOTE: change this line to use different fitness functions:
        HalfOnes | QuarterOnes | MaxOnes | Rosenbrock *)
    let fitness_type = HalfOnes in
    let fitness_fn = get_fitness_fn fitness_type in

    (* ------------------ initialize population using a fitness function ------------------ *)
    let population =
        SGA.population_init population_size genome_length rng_state fitness_fn
    in
        (* ------------------ print header ------------------ *)
        Printf.printf
          "%s  %d  %d  %g  %g\n"
          (fitness_name fitness_type)
          population_size
          genome_length
          mutation_rate
          crossover_rate;

        (* ------------------ helper functions ------------------ *)
        (* calculate average fitness across all genomes in population *)
        let average_fitness (pop : SGA.population_t) =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> acc +. g.fitness)
                (* sum all fitness values *)
              0.0
              pop.members
            /. float_of_int pop.size (* divide by population size for average *)
        in

        (* find the highest fitness value in the population *)
        let champion_fitness (pop : SGA.population_t) =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> max acc g.fitness)
                (* keep track of maximum *)
              neg_infinity (* start with negative infinity as baseline *)
              pop.members
        in

        (* calculate what fraction of population has the most common fitness *)
        let same_fitness_ratio (pop : SGA.population_t) =
            let freq = Hashtbl.create pop.size in
                (* hash table to count frequencies *)
                Array.iter
                  (fun (g : SGA.genome_t) ->
                    let f = g.fitness in
                        (* increment count for this fitness value *)
                        Hashtbl.replace
                          freq
                          f
                          ((try Hashtbl.find freq f with Not_found -> 0) + 1))
                  pop.members;
                (* find the fitness value that appears most often *)
                let max_count =
                    Hashtbl.fold (fun _ v acc -> max v acc) freq 0
                in
                    (* return ratio of most common fitness to total population *)
                    float_of_int max_count /. float_of_int pop.size
        in

        (* ------------------ evolution loop ------------------ *)
        let final_population, termination_reason, generations =
            let rec evolve current_pop generation last_avg_fitness
                stagnant_generations =
                (* safety check to prevent infinite loops *)
                if generation > max_generations then
                  (current_pop, "reached maximum generation limit", generation)
                else
                  (* create next generation through selection, crossover, and mutation *)
                  let new_pop =
                      SGA.make_new_generation_with
                        current_pop
                        mutation_rate
                        crossover_rate
                        rng_state
                        fitness_fn
                  in

                  (* calculate statistics for this generation *)
                  let avg_fit = average_fitness new_pop in
                  let champ_fit = champion_fitness new_pop in
                  let ratio = same_fitness_ratio new_pop in

                  Printf.printf
                    "%d %f %f %f\n"
                    generation
                    champ_fit
                    avg_fit
                    (ratio *. 100.0);

                  (* stagnation detection *)
                  let improvement = avg_fit -. last_avg_fitness in
                  let stagnant_generations =
                      if improvement < no_improve_threshold then
                        stagnant_generations + 1
                      else if improvement > significant_improve_threshold then 0
                      else max 0 (stagnant_generations - 1)
                  in

                  (* check various termination conditions *)
                  if stagnant_generations >= convergence_window then
                    ( new_pop,
                      Printf.sprintf "stagnated for %d generations" convergence_window,
                      generation )
                  else if ratio >= high_fitness_ratio then
                    let common_fitness = 
                      let freq = Hashtbl.create new_pop.size in
                      Array.iter (fun (g : SGA.genome_t) -> 
                        let f = g.fitness in
                        Hashtbl.replace freq f ((try Hashtbl.find freq f with Not_found -> 0) + 1)
                      ) new_pop.members;
                      fst (Hashtbl.fold (fun f count (max_f, max_count) -> 
                        if count > max_count then (f, count) else (max_f, max_count)
                      ) freq (0.0, 0))
                    in
                    ( new_pop,
                      Printf.sprintf "%.1f%% of population converged to fitness %.6f" (ratio *. 100.0) common_fitness,
                      generation )
                  else
                    (* continue evolving to next generation *)
                    evolve new_pop (generation + 1) avg_fit stagnant_generations
            in
                (* start evolution from generation 1 *)
                evolve population 1 (average_fitness population) 0
        in

        (* ------------------ final output ------------------ *)
        SGA.print_population final_population;

        Printf.printf "---- Final Stats ----\n";
        Printf.printf "Total generations : %d\n" generations;
        Printf.printf "Average fitness   : %.4f\n" (average_fitness final_population);
        Printf.printf "Convergence       : %.2f%%\n" ((same_fitness_ratio final_population) *. 100.0);
        Printf.printf "%s\n" termination_reason
