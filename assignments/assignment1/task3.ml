module SGA = Task1
module Rand = Util.Rand
module Out = Util.Output

type fitness_type = Rosenbrock
let fitness_name = function Rosenbrock -> "rosenbrock"
let get_fitness_fn = function Rosenbrock -> SGA.rosenbrock_fitness
let decode_genome (genome : SGA.genome_t) =
    let half = genome.SGA.length / 2 in
    let max_val = float_of_int ((1 lsl half) - 1) in

    let decode_slice start_idx =
        let value = ref 0 in
            for i = 0 to half - 1 do
              if genome.SGA.string.(start_idx + i) then
                value := !value lor (1 lsl i)
            done;
            (* scale to range [-2.048, 2.048] which contains the minimum at (1,1) *)
            (4.096 *. (float_of_int !value /. max_val)) -. 2.048
    in

    let x = decode_slice 0 in
    let y = decode_slice half in
        (x, y)

let () =
    (* ------------------ predefined parameters ------------------ *)
    let population_size = 500 in
    let bits_per_variable = 20 in
    let genome_length = bits_per_variable * 2 in
    let mutation_rate = 0.001 in
    let crossover_rate = 0.7 in
    let max_generations = 10_000 in

    (* convergence parameters *)
    let convergence_window = 500 in
    let no_improve_threshold = 1e-6 in
    let significant_improve_threshold = 1e-5 in
    let high_fitness_ratio = 0.95 in

    (* ------------------ initialize random state ------------------ *)
    let rng_state = Rand.create () in

    let fitness_type = Rosenbrock in
    let fitness_fn = get_fitness_fn fitness_type in

    (* ------------------ initialize population ------------------ *)
    let population =
        SGA.population_init population_size genome_length rng_state fitness_fn
    in

    (* ------------------ print header ------------------ *)
    let start_time = Sys.time () in

    (* create dynamic output filename *)
    let output_filename =
        Printf.sprintf
          "corniedj-%s-%d-%d-%.3f-%.2f"
          (fitness_name fitness_type)
          population_size
          genome_length
          mutation_rate
          crossover_rate
        |> String.map (function '.' -> '_' | c -> c)
        (* replace dots with underscores *)
    in
        Out.set_both_output output_filename;

        Out.printf
          "%s  %d  %d  %g  %g\n"
          (fitness_name fitness_type)
          population_size
          genome_length
          mutation_rate
          crossover_rate;

        let best_fitness = ref infinity in
        let best_individual = ref None in

        (* ------------------ helper functions ------------------ *)
        (* calculate average fitness across all genomes in population *)
        let average_fitness (pop : SGA.population_t) =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> acc +. g.SGA.fitness)
              0.0
              pop.SGA.members
            /. float_of_int pop.SGA.size
        in

        (* find the lowest fitness value in the population (for minimization) *)
        let champion_fitness (pop : SGA.population_t) =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> min acc g.SGA.fitness)
              infinity
              pop.SGA.members
        in

        (* calculate what fraction of population has the most common fitness *)
        let same_fitness_ratio (pop : SGA.population_t) =
            let freq = Hashtbl.create pop.SGA.size in
                Array.iter
                  (fun (g : SGA.genome_t) ->
                    let f = g.SGA.fitness in
                        Hashtbl.replace
                          freq
                          f
                          ((try Hashtbl.find freq f with Not_found -> 0) + 1))
                  pop.SGA.members;
                let max_count =
                    Hashtbl.fold (fun _ v acc -> max v acc) freq 0
                in
                    float_of_int max_count /. float_of_int pop.SGA.size
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

                  (* Update best individual *)
                  Array.iter
                    (fun (g : SGA.genome_t) ->
                      if g.SGA.fitness < !best_fitness then (
                        best_fitness := g.SGA.fitness;
                        best_individual := Some g))
                    new_pop.SGA.members;

                  (* calculate statistics for this generation *)
                  let avg_fit = average_fitness new_pop in
                  let champ_fit = champion_fitness new_pop in
                  let ratio = same_fitness_ratio new_pop in

                  let best_x, best_y =
                      match !best_individual with
                      | Some g -> decode_genome g
                      | None -> (0.0, 0.0)
                  in
                      Out.printf
                        "%d %f %f %f %f %f\n"
                        generation
                        champ_fit
                        avg_fit
                        (ratio *. 100.0)
                        best_x
                        best_y;
                      Out.print_string "" (* Force flush *);

                      (* stagnation detection: for minimization, improvement is when fitness decreases *)
                      let improvement = last_avg_fitness -. avg_fit in
                      let stagnant_generations =
                          if improvement <= no_improve_threshold then
                            stagnant_generations + 1
                          else if improvement > significant_improve_threshold
                          then 0
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
                            let freq = Hashtbl.create new_pop.SGA.size in
                                Array.iter
                                  (fun (g : SGA.genome_t) ->
                                    let f = g.SGA.fitness in
                                        Hashtbl.replace
                                          freq
                                          f
                                          ((try Hashtbl.find freq f
                                            with Not_found -> 0)
                                          + 1))
                                  new_pop.SGA.members;
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
                                "%.1f%%%% of population converged to fitness \
                                 %.6f"
                                (ratio *. 100.0)
                                common_fitness,
                              generation )
                      else
                        evolve
                          new_pop
                          (generation + 1)
                          avg_fit
                          stagnant_generations
            in
            let initial_avg = average_fitness population in
                evolve population 1 initial_avg 0
        in

        (* ------------------ final output ------------------ *)
        SGA.print_population final_population;

        let end_time = Sys.time () in
        let elapsed = end_time -. start_time in

        Out.printf "---- Final Results ----\n";
        (match !best_individual with
        | Some best ->
            let x, y = decode_genome best in
                Out.printf "Best solution found at (%.6f, %.6f)\n" x y;
                Out.printf "Function value: %.10f\n" !best_fitness
        | None -> Out.printf "No solution found\n");
        Out.printf "Total generations : %d\n" generations;
        Out.printf
          "Average fitness   : %.4f\n"
          (average_fitness final_population);
        Out.printf
          "Convergence       : %.2f%%%%\n"
          (same_fitness_ratio final_population *. 100.0);
        Out.printf "Termination reason: %s\n" termination_reason;
        Out.printf "Execution time    : %.2f seconds\n" elapsed
