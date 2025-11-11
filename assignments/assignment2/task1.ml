(* code written by Derek Corniello *)

open Assignment2
module Out = Util.Output

(*
intermediate recombination, i chose it for continuous space, allows smooth blending
above that, seems like the obvious choice, especially for floating repr
*)
let intermediate_recombination (rng_state : Rand.rng_state)
    (fitness_fn : genome_t -> float) (genome_1 : genome_t) (genome_2 : genome_t)
    : genome_t =
    let x1, y1 = genome_1.string in
    let x2, y2 = genome_2.string in
    let inter_x = Rand.gaussian_between rng_state x1 x2 in
    let inter_y = Rand.gaussian_between rng_state y1 y2 in
    let genome =
        {
          fitness = 0.0;
          string = (inter_x, inter_y);
          x_std_dev = sigma_x;
          y_std_dev = sigma_y;
        }
    in
        { genome with fitness = fitness_fn genome }

(* main flow here *)
let () : unit =
    let rng_state = Rand.create_random () in
    (*
    NOTE:comment this out to run seeded:
    let rng_state = Rand.create_with_seed 1 in
    *)

    (*
    NOTE: here is where you can change out the fitness function
    *)
    let fitness_fn = himmelblau_function in
    let population = population_init rng_state fitness_fn lambda in

    (* create dynamic output filename *)
    let output_filename =
        Printf.sprintf
          "corniedj-%s-%d-%d-%s-%d"
          "himmelblau"
          mu
          lambda
          (string_of_float sigma_x ^ "_" ^ string_of_float sigma_y |> String.map (function '.' -> '_' | c -> c))
          0
        |> String.map (function '.' -> '_' | c -> c)
        (* replace dots with underscores *)
    in
        Out.set_both_output output_filename;
        let _ = Out.open_output output_filename in

    (* output format as described in the assignment, complies with the grading rubric for producing output as defined *)
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
          "himmelblau ES %d %d %s %d %d %d %f %f %f\n"
          mu
          lambda
          (string_of_float sigma_x ^ "," ^ string_of_float sigma_y)
          0
          0
          0
          initial_best
          initial_avg
          initial_diversity;

        let max_generations = 100000 in

        (*
     mu plus lambda survivor selection, i chose it for balancing exploration and exploitation
     retaining parents for stability while allowing offspring innovation

     since this function has a few different minima, its a good decision to allow for more exploration

     todo if i find out later we are trying to optimize for one best answer, then it may be worth changing to mu lambda
     *)
        let rec evolve gen pop =
            if gen >= max_generations then (pop, gen)
            else
              (*
            check convergence for at least 80% of population have the same fitnesss
            i chose it to balance early stop and exploration
            NOTE: maybe this should be higher? hard to say with such a small population
          *)
              let first_fitness = pop.members.(0).fitness in
              let count =
                  Array.fold_left
                    (fun acc g ->
                      if g.fitness = first_fitness then acc + 1 else acc)
                    0
                    pop.members
              in
              let converged = count >= 12 in
                  if converged then (pop, gen)
                  else
                    let offspring =
                        Array.init lambda (fun _ ->
                            let idx1 =
                                int_of_float
                                  (Rand.uniform_range
                                     rng_state
                                     0.0
                                     (float pop.size))
                            in
                            let idx2 =
                                int_of_float
                                  (Rand.uniform_range
                                     rng_state
                                     0.0
                                     (float pop.size))
                            in
                            let p1 = pop.members.(idx1) in
                            let p2 = pop.members.(idx2) in
                            let recombined =
                                intermediate_recombination
                                  rng_state
                                  fitness_fn
                                  p1
                                  p2
                            in
                                gaussian_perturbation
                                  rng_state
                                  fitness_fn
                                  recombined)
                    in
                    (* combine parents and offspring for mu + lambda*)
                    let combined = Array.append pop.members offspring in
                        (* sort combined by fitness ascending and take top mu as new pop *)
                        Array.sort
                          (fun a b -> compare a.fitness b.fitness)
                          combined;
                        let new_members = Array.sub combined 0 mu in
                        let new_pop = { size = mu; members = new_members } in

                        (* periodic output, complying with assignment format, provides intermediate stats as allowed *)
                        let avg_fit =
                            average_fitness_generic new_pop.members (fun g ->
                                g.fitness)
                        in
                        let champ_fit =
                            champion_fitness_generic new_pop.members (fun g ->
                                g.fitness)
                        in
                        let diversity =
                            compute_diversity_generic
                              new_pop.members
                              decode_genome
                        in
                            Out.printf
                              "himmelblau ES %d %d %s %d %d %d %f %f %f\n"
                              mu
                              lambda
                              (string_of_float sigma_x ^ ","
                             ^ string_of_float sigma_y)
                              0
                              (gen + 1)
                              ((gen + 1) * lambda)
                              champ_fit
                              avg_fit
                              diversity;

                            evolve (gen + 1) new_pop
        in

        let final_pop, generations = evolve 0 population in

        (* final output, matches the initial line format and provides end of run stats *)
        let final_avg =
            average_fitness_generic final_pop.members (fun g -> g.fitness)
        in
        let final_best =
            champion_fitness_generic final_pop.members (fun g -> g.fitness)
        in
        let final_diversity =
            compute_diversity_generic final_pop.members decode_genome
        in
            Out.printf
              "himmelblau ES %d %d %s %d %d %d %f %f %f\n"
              mu
              lambda
              (string_of_float sigma_x ^ "," ^ string_of_float sigma_y)
              0
              generations
              (generations * lambda)
              final_best
              final_avg
              final_diversity;

            Out.close_output ();

            (* final population output *)
            Output.printf "Final population:\n";
            Array.iter
              (fun g ->
                let x, y = g.string in
                    Output.printf "x=%.6f y=%.6f fitness=%.6f\n" x y g.fitness)
              final_pop.members;

            ()
