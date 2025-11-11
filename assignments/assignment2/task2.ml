(* code written by Derek Corniello *)

module Out = Util.Output
open Assignment2
open Util.Plot

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

    (* set up file output for es logs *)
    Out.set_output_overwrite "es_output.txt";

    (*
    note here is where you can change out the fitness function
    *)
    let fitness_fn = himmelblau_function in
    let population = population_init rng_state fitness_fn lambda in

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

        let max_generations = 10000 in

        (* data collection for plotting *)
        let evaluations = ref [] in
        let avg_fitnesses = ref [] in
        let best_fitnesses = ref [] in
        let diversities = ref [] in

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
              HACK: maybe this should be higher? hard to say with such a small population
           *)
              (* convergence at 80 percent, i chose it to balance early stop and exploration *)
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

                        (* Collect data *)
                        let evals = float (gen * lambda) in
                        let avg =
                            average_fitness_generic new_pop.members (fun g ->
                                g.fitness)
                        in
                        let best =
                            champion_fitness_generic new_pop.members (fun g ->
                                g.fitness)
                        in
                        let diversity =
                            compute_diversity_generic
                              new_pop.members
                              decode_genome
                        in
                            evaluations := evals :: !evaluations;
                            avg_fitnesses := avg :: !avg_fitnesses;
                            best_fitnesses := best :: !best_fitnesses;
                            diversities := diversity :: !diversities;

                            (* periodic output, complying with assignment format, provides intermediate stats as allowed *)
                            Out.printf
                              "himmelblau ES %d %d %s %d %d %d %f %f %f\n"
                              mu
                              lambda
                              (string_of_float sigma_x ^ ","
                             ^ string_of_float sigma_y)
                              0
                              (gen + 1)
                              ((gen + 1) * lambda)
                              best
                              avg
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

            (* spec plot the performance graphs *)
            plot_performance
              "es_"
              (List.rev !evaluations)
              (List.rev !avg_fitnesses)
              (List.rev !best_fitnesses)
              (List.rev !diversities);

            ()
