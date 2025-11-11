(* code written by Derek Corniello *)

module Rand = Util.Rand
module Out = Util.Output
open Util.Plot
open Assignment2

type genome_t = {
    length : int;
    mutable fitness : float;
    string : bool array; (* arrays are mutable by default *)
  }

type population_t = { size : int; members : genome_t array }

let print_genome (genome : genome_t) : unit =
    Array.iter
      (fun bit -> Out.print_string (if bit then "1" else "0"))
      genome.string;
    Out.printf " (%.3f)" genome.fitness;
    Out.print_newline ()

let print_population (pop : population_t) : unit =
    Out.print_string ("population with size " ^ string_of_int pop.size ^ ":\n");
    Array.iter print_genome pop.members;
    Out.print_newline ()

(* initialize a genome with random bits and compute fitness *)
let genome_init (size : int) (rng_state : Rand.rng_state)
    (fitness_fn : genome_t -> float) : genome_t =
    (* create array of random booleans *)
    let bits = Array.init size (fun _ -> Rand.bool rng_state) in
    let genome = { length = size; fitness = 0.0; string = bits } in
        { genome with fitness = fitness_fn genome }

(* initialize population with random genomes *)
let population_init (member_count : int) (member_length : int)
    (rng_state : Rand.rng_state) (fitness_fn : genome_t -> float) : population_t
    =
    (* create array of genomes *)
    {
      size = member_count;
      members =
        Array.init member_count (fun _ ->
            genome_init member_length rng_state fitness_fn);
    }

(* make a deep copy of a genome including bits and fitness *)
let copy_genome (g : genome_t) : genome_t =
    { length = g.length; fitness = g.fitness; string = Array.copy g.string }

(* mutate genome in place based on mutation rate *)
let genome_mutate (genome : genome_t) (mutation_rate : float)
    (rng_state : Rand.rng_state) (fitness_fn : genome_t -> float) : unit =
    for i = 0 to genome.length - 1 do
      if Rand.uniform rng_state < mutation_rate then
        genome.string.(i) <- not genome.string.(i)
    done;

    (* update fitness after mutation *)
    genome.fitness <- fitness_fn genome

(* genome 1p crossover *)
let genome_1P_crossover (genome1 : genome_t) (genome2 : genome_t)
    (rng_state : Rand.rng_state) (fitness_fn : genome_t -> float) :
    genome_t * genome_t =
    if genome1.length <> genome2.length then
      failwith "genome1 and genome2 are not the same size";

    (* copy bit arrays to create children *)
    let child1 = Array.copy genome1.string in
    let child2 = Array.copy genome2.string in

    (* select crossover point *)
    let split_index =
        int_of_float
          (floor
             (Rand.uniform_range
                rng_state
                1.0
                (float_of_int genome1.length -. 1.0)))
    in
        for i = split_index to genome1.length - 1 do
          child1.(i) <- genome2.string.(i);
          child2.(i) <- genome1.string.(i)
        done;

        (* recompute fitness for children *)
        ( ( { length = genome1.length; fitness = 0.0; string = child1 }
          |> fun g -> { g with fitness = fitness_fn g } ),
          { length = genome2.length; fitness = 0.0; string = child2 }
          |> fun g -> { g with fitness = fitness_fn g } )

type fitness_type = Himmelblau

(* decode genome *)
let decode_genome (genome : genome_t) =
    let half = genome.length / 2 in
    (* for 10 bits, max_val = 2^10 - 1 = 1023 *)
    let max_val = float_of_int ((1 lsl half) - 1) in

    let decode_slice start_idx =
        let value = ref 0 in
            (* read bits in big-endian order *)
            for i = 0 to half - 1 do
              if genome.string.(start_idx + i) then
                value := !value lor (1 lsl (half - 1 - i))
            done;
            (* scale to range [-10.0, 10.0] as per assignment requirements *)
            let min_val = -10.0 in
            let max_val_range = 10.0 in
            let range = max_val_range -. min_val in
            (* 20.0 *)
            let scaled = min_val +. (float_of_int !value *. range /. max_val) in
                scaled
    in

    let x = decode_slice 0 in
    let y = decode_slice half in
        (x, y)

let himmelblau_fitness (genome : genome_t) : float =
    let x, y = decode_genome genome in
        himmelblau x y

let fitness_name = function Himmelblau -> "himmelblau"
let get_fitness_fn = function Himmelblau -> himmelblau_fitness

(* make new generation with *)
let make_new_generation_with (curr_pop : population_t) (mutation_rate : float)
    (crossover_rate : float) (rng_state : Rand.rng_state)
    (fitness_fn : genome_t -> float) : population_t =
    let pop_size = curr_pop.size in
    let members = curr_pop.members in

    (* Precompute all random numbers needed for parent selection at once *)
    let num_parents = if pop_size mod 2 = 0 then pop_size else pop_size + 1 in
    let parent_targets =
        Array.init num_parents (fun _ -> Rand.uniform rng_state)
    in

    (* Single pass to compute min fitness, max fitness, and sum *)
    let min_fitness = ref max_float in
    let max_fitness = ref min_float in
    let sum_fitness = ref 0.0 in

    Array.iter
      (fun m ->
        min_fitness := min !min_fitness m.fitness;
        max_fitness := max !max_fitness m.fitness;
        sum_fitness := !sum_fitness +. m.fitness)
      members;

    (* Build roulette wheel with a single pass *)
    let roulette_wheel = Array.make pop_size 0.0 in
    let max_fitness_val = !max_fitness in
    let sum = ref 0.0 in

    for i = 0 to pop_size - 1 do
      let adjusted = max_fitness_val -. members.(i).fitness +. 1.0 in
          sum := !sum +. adjusted;
          roulette_wheel.(i) <- !sum
    done;

    (* Normalize in place *)
    let inv_total = 1.0 /. !sum in
        for i = 0 to pop_size - 1 do
          roulette_wheel.(i) <- roulette_wheel.(i) *. inv_total
        done;

        (* Precompute all parent indices at once *)
        let parent_indices = Array.make num_parents 0 in
            for p = 0 to num_parents - 1 do
              let target = parent_targets.(p) in
              let rec find_idx i =
                  if i >= pop_size then pop_size - 1
                  else if roulette_wheel.(i) >= target then i
                  else find_idx (i + 1)
              in
                  parent_indices.(p) <- find_idx 0
            done;

            (* Create new population *)
            let new_members =
                Array.make pop_size { length = 0; fitness = 0.0; string = [||] }
            in
            let parent_idx = ref 0 in

            for i = 0 to (pop_size lsr 1) - 1 do
              let p1 = parent_indices.(!parent_idx) in
              let p2 = parent_indices.(!parent_idx + 1) in
                  parent_idx := !parent_idx + 2;

                  let child1 = copy_genome members.(p1) in
                  let child2 = copy_genome members.(p2) in

                  if Rand.uniform rng_state < crossover_rate then (
                    let c1, c2 =
                        genome_1P_crossover child1 child2 rng_state fitness_fn
                    in
                        new_members.(i * 2) <- c1;
                        new_members.((i * 2) + 1) <- c2)
                  else (
                    new_members.(i * 2) <- child1;
                    new_members.((i * 2) + 1) <- child2)
            done;

            if pop_size land 1 = 1 then
              new_members.(pop_size - 1) <-
                copy_genome members.(parent_indices.(!parent_idx));

            (* apply mutations, but skip the elite at index 0 to preserve it *)
            (* ensure true elite is preserved, sort new_members and replace index 0 with best from previous generation *)
            Array.iteri
              (fun i m ->
                if i > 0 && Rand.uniform rng_state < mutation_rate then
                  genome_mutate m mutation_rate rng_state fitness_fn;
                new_members.(i) <- m)
              new_members;

            Array.sort (fun a b -> compare a.fitness b.fitness) new_members;
            let elite_fitness =
                Array.fold_left
                  (fun min g -> if g.fitness < min then g.fitness else min)
                  infinity
                  curr_pop.members
            in
            let elite = ref None in
                Array.iter
                  (fun g -> if g.fitness = elite_fitness then elite := Some g)
                  curr_pop.members;
                let elite =
                    match !elite with
                    | Some e -> e
                    | None -> failwith "Elite not found"
                in
                    new_members.(0) <- copy_genome elite;

                    { size = pop_size; members = new_members }

let () =
    (* predefined parameters *)
    let population_size = 100 in
    let bits_per_variable = 10 in
    let genome_length = bits_per_variable * 2 in
    (* 0.001 low for precision, suits 20-bit genome *)
    let mutation_rate = 0.001 in
    let crossover_rate = 0.6 in
    let max_generations = 1_000 in

    (* convergence parameters *)
    (* 500 gens stagnation allows thorough search without infinite loops *)
    let convergence_window = 500 in
    let no_improve_threshold = 1e-8 in
    let significant_improve_threshold = 1e-9 in
    let high_fitness_ratio = 0.95 in

    let rng_state = Rand.create_random () in
    (*
    NOTE:comment this out to run seeded:
    let rng_state = Rand.create_with_seed 1 in
    *)

    let fitness_type = Himmelblau in
    let fitness_fn = get_fitness_fn fitness_type in

    let population =
        population_init population_size genome_length rng_state fitness_fn
    in

    (* ------------------ print initial output line ------------------ *)
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
        Out.set_output_overwrite output_filename;
        let _ = Out.open_output output_filename in

        (* output format, complies with the grading rubric for producing output as defined *)
        let initial_avg =
            average_fitness_generic population.members (fun g -> g.fitness)
        in
        let initial_best =
            champion_fitness_generic population.members (fun g -> g.fitness)
        in
        let initial_diversity =
            compute_diversity_generic population.members decode_genome
        in
            Out.printf
              "%s GA %d %d %g %g %d %d %f %f %f\n"
              (fitness_name fitness_type)
              population_size
              population_size
              mutation_rate
              crossover_rate
              0
              0
              initial_best
              initial_avg
              initial_diversity;

            let best_fitness = ref infinity in
            let best_individual = ref None in

            (* same fitness ratio *)
            let same_fitness_ratio (pop : population_t) : float =
                let freq = Hashtbl.create pop.size in
                    Array.iter
                      (fun (g : genome_t) ->
                        let f = g.fitness in
                            Hashtbl.replace
                              freq
                              f
                              ((try Hashtbl.find freq f with Not_found -> 0)
                              + 1))
                      pop.members;
                    let max_count =
                        Hashtbl.fold (fun _ v acc -> max v acc) freq 0
                    in
                        float_of_int max_count /. float_of_int pop.size
            in

            (* data collection for plotting, similar to task2.ml *)
            let evaluations = ref [] in
            let avg_fitnesses = ref [] in
            let best_fitnesses = ref [] in
            let diversities = ref [] in

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
                          make_new_generation_with
                            current_pop
                            mutation_rate
                            crossover_rate
                            rng_state
                            fitness_fn
                      in

                      (* Update best individual *)
                      Array.iter
                        (fun (g : genome_t) ->
                          if g.fitness < !best_fitness then (
                            best_fitness := g.fitness;
                            best_individual := Some g))
                        new_pop.members;

                      (* calculate statistics for this generation *)
                      let avg_fit =
                          average_fitness_generic new_pop.members (fun g ->
                              g.fitness)
                      in
                      let champ_fit =
                          champion_fitness_generic new_pop.members (fun g ->
                              g.fitness)
                      in
                      let ratio = same_fitness_ratio new_pop in

                      let diversity =
                          compute_diversity_generic
                            new_pop.members
                            decode_genome
                      in

                      (* collect data for plotting *)
                      evaluations :=
                        (float generation *. float population_size)
                        :: !evaluations;
                      avg_fitnesses := avg_fit :: !avg_fitnesses;
                      best_fitnesses := champ_fit :: !best_fitnesses;
                      diversities := diversity :: !diversities;

                      (* periodic output, complying with assignment format, provides intermediate stats as allowed *)
                      Out.printf
                        "%s GA %d %d %g %g %d %d %f %f %f\n"
                        (fitness_name fitness_type)
                        population_size
                        population_size
                        mutation_rate
                        crossover_rate
                        generation
                        (generation * population_size)
                        champ_fit
                        avg_fit
                        diversity;
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
                            let freq = Hashtbl.create new_pop.size in
                                Array.iter
                                  (fun (g : genome_t) ->
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
                                "%.1f%% of population converged to fitness %.6f"
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
                let initial_avg =
                    average_fitness_generic population.members (fun g ->
                        g.fitness)
                in
                    evolve population 1 initial_avg 0
            in

            print_population final_population;

            let end_time = Sys.time () in
            let elapsed = end_time -. start_time in

            (* final output, matches the initial line format and provides end of run stats *)
            let final_avg =
                average_fitness_generic final_population.members (fun g ->
                    g.fitness)
            in
            let final_best =
                champion_fitness_generic final_population.members (fun g ->
                    g.fitness)
            in
            let final_diversity =
                compute_diversity_generic final_population.members decode_genome
            in
                Out.printf
                  "%s GA %d %d %g %g %d %d %f %f %f\n"
                  (fitness_name fitness_type)
                  population_size
                  population_size
                  mutation_rate
                  crossover_rate
                  generations
                  (generations * population_size)
                  final_best
                  final_avg
                  final_diversity;

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
                  (average_fitness_generic final_population.members (fun g ->
                       g.fitness));
                Out.printf
                  "Convergence       : %.2f%%\n"
                  (same_fitness_ratio final_population *. 100.0);
                Out.printf "Termination reason: %s\n" termination_reason;
                Out.printf "Actual execution time    : %.2f seconds\n" elapsed;
                Out.close_output ();

                (* plot the performance graphs, reusing from task2.ml *)
                plot_performance
                  "ga_"
                  (List.rev !evaluations)
                  (List.rev !avg_fitnesses)
                  (List.rev !best_fitnesses)
                  (List.rev !diversities);

                ()
