module SGA = Task1
module Rand = Util.Rand
module Out = Util.Output

type fitness_type = MaxOnes
let fitness_name = function MaxOnes -> "max-ones"
let get_fitness_fn = function MaxOnes -> SGA.max_ones_fitness
let () =
    (* ------------------ predefined parameters ------------------ *)
    let population_size = 100 in
    let genome_length = 32 in
    let mutation_rate = 0.01 in
    let crossover_rate = 0.5 in
    let max_generations = 2_000_000 in

    (* convergence parameters *)
    let convergence_window = 500 in
    let no_improve_threshold = 1e-4 in
    let significant_improve_threshold = 1e-3 in
    let high_fitness_ratio = 0.95 in

    let rng_state = Rand.create () in
    let fitness_type = MaxOnes in
    let fitness_fn = get_fitness_fn fitness_type in

    let population =
        SGA.population_init population_size genome_length rng_state fitness_fn
    in

    let start_time = Sys.time () in
    (* create dynamic output filename *)
    let output_filename =
        Printf.sprintf
          "corniedj-%s-%d-%d-%g-%g"
          (fitness_name fitness_type)
          population_size
          genome_length
          mutation_rate
          crossover_rate
        |> String.map (function '.' -> '_' | c -> c)
    in
        Out.set_both_output output_filename;

        Out.printf
          "%s  %d  %d  %g  %g\n"
          (fitness_name fitness_type)
          population_size
          genome_length
          mutation_rate
          crossover_rate;

        (* ------------------ helper functions ------------------ *)
        let average_fitness (pop : SGA.population_t) =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> acc +. g.fitness)
              0.0
              pop.members
            /. float_of_int pop.size
        in

        (* find the highest fitness value in the population *)
        let champion_fitness (pop : SGA.population_t) =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> max acc g.fitness)
              neg_infinity
              pop.members
        in

        (* calculate what fraction of population has the most common fitness *)
        let same_fitness_ratio (pop : SGA.population_t) =
            let freq = Hashtbl.create pop.size in
                Array.iter
                  (fun (g : SGA.genome_t) ->
                    let f = g.fitness in
                        Hashtbl.replace
                          freq
                          f
                          ((try Hashtbl.find freq f with Not_found -> 0) + 1))
                  pop.members;
                let max_count =
                    Hashtbl.fold (fun _ v acc -> max v acc) freq 0
                in
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
                  (* create next generation *)
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

                  Out.printf
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
                      Printf.sprintf
                        "stagnated for %d generations"
                        convergence_window,
                      generation )
                  else if ratio >= high_fitness_ratio then
                    let common_fitness =
                        let freq = Hashtbl.create new_pop.size in
                            Array.iter
                              (fun (g : SGA.genome_t) ->
                                let f = g.fitness in
                                    Hashtbl.replace
                                      freq
                                      f
                                      ((try Hashtbl.find freq f
                                        with Not_found -> 0)
                                      + 1))
                              new_pop.members;
                            fst
                              (Hashtbl.fold
                                 (fun f count (max_f, max_count) ->
                                   if count > max_count then (f, count)
                                   else (max_f, max_count))
                                 freq
                                 (0.0, 0))
                    in
                        ( new_pop,
                          Printf.sprintf
                            "%.1f%%%% of population converged to fitness %.6f"
                            (ratio *. 100.0)
                            common_fitness,
                          generation )
                  else if champ_fit = float_of_int genome_length then
                    (new_pop, "found optimal solution", generation)
                  else
                    evolve new_pop (generation + 1) avg_fit stagnant_generations
            in
                evolve population 1 (average_fitness population) 0
        in

        (* ------------------ final output ------------------ *)
        SGA.print_population final_population;

        let end_time = Sys.time () in
        let elapsed = end_time -. start_time in

        Out.printf "---- Final Stats ----\n";
        Out.printf "Total generations : %d\n" generations;
        Out.printf
          "Average fitness   : %.4f\n"
          (average_fitness final_population);
        Out.printf
          "Convergence       : %.2f%%%%\n"
          (same_fitness_ratio final_population *. 100.0);
        Out.printf "Termination reason: %s\n" termination_reason;
        Out.printf "Execution time    : %.2f seconds\n" elapsed
