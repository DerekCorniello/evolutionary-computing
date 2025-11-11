(* Shared module for Assignment 2 *)

module Rand = Util.Rand
module Output = Util.Output

(* we are only worried about finding one, so this is better for exploitation*)
let sigma_x = 1.0
let sigma_y = 0.5
let mu = 15
let lambda = 100

type genome_t = {
    fitness : float;
    string : float * float; (* for this ES, we will just use a tuple of floats*)
    x_std_dev : float; (* which is denoted by `float * float` *)
    y_std_dev : float;
  }

type population_t = { size : int; members : genome_t array }

let genome_init (rng_state : Rand.rng_state) (fitness_fn : genome_t -> float) =
    (* this syntax wrapping for negatives is weird... *)
    (* assignment says we can use search space of -10 to 10 in both directions
       so that is why we do this here*)
    let x = Rand.uniform_range rng_state (-10.0) 10.0 in
    let y = Rand.uniform_range rng_state (-10.0) 10.0 in
    let genome =
        {
          fitness = 0.0;
          string = (x, y);
          x_std_dev = sigma_x;
          y_std_dev = sigma_y;
        }
    in
        { genome with fitness = fitness_fn genome }

let population_init (rng_state : Rand.rng_state)
    (fitness_fn : genome_t -> float) (member_count : int) : population_t =
    {
      size = member_count;
      members =
        Array.init member_count (fun _ -> genome_init rng_state fitness_fn);
    }

(* this is silly, but ok lol *)
let decode_genome (genome : genome_t) : float * float = genome.string

let himmelblau_function (genome : genome_t) : float =
    let x, y = decode_genome genome in
        (((x ** 2.0) +. y -. 11.0) ** 2.0) +. ((x +. (y ** 2.0) -. 7.0) ** 2.0)

(* helper function to compute population diversity as max Euclidean distance *)
let compute_diversity (pop : population_t) : float =
    let max_dist = ref 0.0 in
        for i = 0 to pop.size - 1 do
          for j = i + 1 to pop.size - 1 do
            let x1, y1 = decode_genome pop.members.(i) in
            let x2, y2 = decode_genome pop.members.(j) in
            let dist = sqrt (((x1 -. x2) ** 2.0) +. ((y1 -. y2) ** 2.0)) in
                if dist > !max_dist then max_dist := dist
          done
        done;
        !max_dist

(* gaussian perturbation, always occurs, no rate *)
let gaussian_perturbation (rng_state : Rand.rng_state)
    (fitness_fn : genome_t -> float) (genome : genome_t) : genome_t =
    let mutate (v : float) (std_dev : float) : float =
        (* Square std_dev to get variance, as Rand.gaussian expects variance parameter *)
        v +. Rand.gaussian rng_state 0.0 (std_dev ** 2.0)
    in
    (*
     i am doing standard adaptation, i think this is fine because the function itself
     does not have a lot of intricacy, i think standard will be fine.

     also, i think that x_std_dev should be larger than y_std_dev because the function's
     zeroes are spread out more on the x axis than the y.
     *)
    let new_x = mutate (fst genome.string) genome.x_std_dev in
    let new_y = mutate (snd genome.string) genome.y_std_dev in
    (* Clamp to search bounds [-10, 10] *)
    let clamped_x = max (-10.0) (min 10.0 new_x) in
    let clamped_y = max (-10.0) (min 10.0 new_y) in
    let genome =
        {
          fitness = 0.0;
          string = (clamped_x, clamped_y);
          x_std_dev = genome.x_std_dev;
          y_std_dev = genome.y_std_dev;
        }
    in
        { genome with fitness = fitness_fn genome }

(* helper functions *)
let average_fitness (pop : population_t) : float =
    Array.fold_left (fun acc (g : genome_t) -> acc +. g.fitness) 0.0 pop.members
    /. float_of_int pop.size

let champion_fitness (pop : population_t) : float =
    Array.fold_left
      (fun acc (g : genome_t) ->
        (* for minimization, we want the smallest fitness value *)
        min acc g.fitness)
      infinity
      pop.members

(* Generic helpers *)
let himmelblau x y =
    (((x ** 2.0) +. y -. 11.0) ** 2.0) +. ((x +. (y ** 2.0) -. 7.0) ** 2.0)

let average_fitness_generic members fitness_fn =
    Array.fold_left (fun acc g -> acc +. fitness_fn g) 0.0 members
    /. float_of_int (Array.length members)

let champion_fitness_generic members fitness_fn =
    Array.fold_left (fun acc g -> min acc (fitness_fn g)) infinity members

let compute_diversity_generic members decode_fn =
    let max_dist = ref 0.0 in
        for i = 0 to Array.length members - 1 do
          for j = i + 1 to Array.length members - 1 do
            let x1, y1 = decode_fn members.(i) in
            let x2, y2 = decode_fn members.(j) in
            let dist = sqrt (((x1 -. x2) ** 2.0) +. ((y1 -. y2) ** 2.0)) in
                if dist > !max_dist then max_dist := dist
          done
        done;
        !max_dist
