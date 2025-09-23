module SGA = Task1
module Rand = Util.Rand
module Out = Util.Output

let uniform_crossover (parent1 : SGA.genome_t) (parent2 : SGA.genome_t)
    (rng_state : Rand.rng_state) (fitness_fn : SGA.genome_t -> float) :
    SGA.genome_t * SGA.genome_t =
    if parent1.SGA.length <> parent2.SGA.length then
      failwith "Parent genomes must be of equal length";

    let length = parent1.SGA.length in
    let child1 = Array.copy parent1.SGA.string in
    let child2 = Array.copy parent2.SGA.string in

    (* perform uniform crossover, each bit has 50% chance of being swapped *)
    for i = 0 to length - 1 do
      if Rand.bool rng_state then (
        let temp = child1.(i) in
            child1.(i) <- child2.(i);
            child2.(i) <- temp)
    done;

    (* create new genome records with updated fitness *)
    let make_genome bits =
        let genome = { SGA.length; SGA.fitness = 0.0; SGA.string = bits } in
            { genome with SGA.fitness = fitness_fn genome }
    in

    (make_genome child1, make_genome child2)

type fitness_type = Rosenbrock

let fitness_name = function Rosenbrock -> "rosenbrock"
let get_fitness_fn = function Rosenbrock -> SGA.rosenbrock_fitness

let decode_genome (genome : SGA.genome_t) =
    let half = genome.SGA.length / 2 in
    (* for 16 bits, max_val = 2^16 - 1 = 65535 *)
    let max_val = float_of_int ((1 lsl half) - 1) in

    let decode_slice start_idx =
        let value = ref 0 in
            (* read bits in big-endian order *)
            for i = 0 to half - 1 do
              if genome.SGA.string.(start_idx + i) then
                value := !value lor (1 lsl (half - 1 - i))
            done;
            (* scale to range [-2.0, 2.0] to make 1.0 exactly representable *)
            let min_val = -2.0 in
            let max_val_range = 2.0 in
            let range = max_val_range -. min_val in
            (* 4.0 *)
            let scaled = min_val +. (float_of_int !value *. range /. max_val) in
                scaled
    in

    let x = decode_slice 0 in
    let y = decode_slice half in
        (x, y)

(* custom function to create new generation with uniform crossover,
   this is different than the implementations in task1 *)
let make_new_generation_with_uniform (pop : SGA.population_t)
    (mutation_rate : float) (crossover_rate : float)
    (rng_state : Rand.rng_state) (fitness_fn : SGA.genome_t -> float) :
    SGA.population_t =
    let pop_size = pop.SGA.size in
    let members = Array.copy pop.SGA.members in

    (* sort by fitness (ascending order for minimization) *)
    Array.sort (fun a b -> compare a.SGA.fitness b.SGA.fitness) members;
    let new_members = Array.make pop_size members.(0) in

    (* keep the best individual (elitism) *)
    new_members.(0) <-
      {
        (SGA.copy_genome members.(0)) with
        SGA.fitness = members.(0).SGA.fitness;
      };

    (* create rest of the population using tournament selection and uniform crossover *)
    for i = 1 to pop_size - 1 do
      let tournament_size = 3 in
      let tournament =
          Array.init tournament_size (fun _ ->
              let idx =
                  int_of_float
                    (Rand.uniform_range
                       rng_state
                       0.0
                       (float_of_int pop_size -. 0.1))
              in
                  members.(idx))
      in
      (* for minimization, we want the individual with the lowest fitness *)
      let parent1 =
          Array.fold_left
            (fun best g -> if g.SGA.fitness < best.SGA.fitness then g else best)
            tournament.(0)
            tournament
      in

      (* get second parent from a new tournament to ensure we get a new parent *)
      let tournament =
          Array.init tournament_size (fun _ ->
              let idx =
                  int_of_float
                    (Rand.uniform_range
                       rng_state
                       0.0
                       (float_of_int pop_size -. 0.1))
              in
                  members.(idx))
      in
      let parent2 =
          Array.fold_left
            (fun best g -> if g.SGA.fitness < best.SGA.fitness then g else best)
            tournament.(0)
            tournament
      in

      let child1, child2 =
          if Rand.uniform rng_state < crossover_rate then
            uniform_crossover parent1 parent2 rng_state fitness_fn
          else (SGA.copy_genome parent1, SGA.copy_genome parent2)
      in

      SGA.genome_mutate child1 mutation_rate rng_state fitness_fn;
      SGA.genome_mutate child2 mutation_rate rng_state fitness_fn;
      if i < pop_size then new_members.(i) <- child1;
      if i + 1 < pop_size then new_members.(i + 1) <- child2
    done;
    { SGA.size = pop_size; SGA.members = new_members }

let () =
    (* predefined parameters *)
    let population_size = 1000 in
    let bits_per_variable = 32 in
    let genome_length = bits_per_variable * 2 in
    let mutation_rate = 0.001 in
    let crossover_rate = 0.6 in
    let max_generations = 1_000 in

    (* convergence parameters *)
    let convergence_window = 500 in
    let no_improve_threshold = 1e-8 in
    let significant_improve_threshold = 1e-9 in
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
        let _ = Out.open_output output_filename in

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
        let average_fitness (pop : SGA.population_t) : float =
            Array.fold_left
              (fun acc (g : SGA.genome_t) -> acc +. g.SGA.fitness)
              0.0
              pop.SGA.members
            /. float_of_int pop.SGA.size
        in

        let champion_fitness (pop : SGA.population_t) : float =
            Array.fold_left
              (fun acc (g : SGA.genome_t) ->
                (* for minimization, we want the smallest fitness value *)
                min acc g.SGA.fitness)
              infinity
              pop.SGA.members
        in

        let same_fitness_ratio (pop : SGA.population_t) : float =
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
                if generation >= max_generations then
                  ( current_pop,
                    "reached maximum generation limit",
                    max_generations )
                else
                  (* create next generation *)
                  let new_pop =
                      make_new_generation_with_uniform
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

                      (* for minimization, improvement is when the average fitness decreases *)
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
                                "%.1f of population converged to fitness %.6f"
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
          "Convergence       : %.2f\n"
          (same_fitness_ratio final_population *. 100.0);
        Out.printf "Termination reason: %s\n" termination_reason;
        Out.printf "Actual execution time    : %.2f seconds\n" elapsed;
        Out.close_output ()
