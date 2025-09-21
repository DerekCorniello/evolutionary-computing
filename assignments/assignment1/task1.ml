(* 
    code written by Derek Corniello

    I have likely overcommented this, partly because I
    am using OCaml for the first time, also because I wanna
    make sure the grading can be done properly if you are
    not super familiar with the functional style of programming

    while OCaml is not purely functional and has some procedural
    parts, it is _mostly_ functional

    if you have any questions, please feel free to reach out to me,
    i am more than willing to describe any of this :)
*)

module Rand = Util.Rand

(* ------------------ types ------------------ *)
type genome_t = {
    length : int;
    mutable fitness : float;
    string : bool array; (* arrays are mutable by default *)
  }

type population_t = { size : int; members : genome_t array }

(* ------------------ fitness functions ------------------ *)

(* calculate fitness based on how close the genome has half ones *)
let half_ones_fitness (genome : genome_t) : float =
    let one_count =
        Array.fold_left
          (fun acc bit -> if bit then acc +. 1.0 else acc)
          0.0
          genome.string
    in
    let ratio = one_count /. float_of_int genome.length in
        -.abs_float (0.5 -. ratio)

(* calculate fitness based on how close the genome has quarter ones *)
let quarter_ones_fitness (genome : genome_t) : float =
    let one_count =
        Array.fold_left
          (fun acc bit -> if bit then acc +. 1.0 else acc)
          0.0
          genome.string
    in
    let ratio = one_count /. float_of_int genome.length in
        -.abs_float (0.25 -. ratio)

(* calculate fitness as number of ones in the genome *)
let max_ones_fitness (genome : genome_t) : float =
    Array.fold_left
      (fun acc bit -> if bit then acc +. 1.0 else acc)
      0.0
      genome.string

(* decode a segment of the genome into a float between min_val and max_val *)
let decode_genome (genome : genome_t) (start_idx : int) (end_idx : int)
    (min_val : float) (max_val : float) : float =
    let decimal_value = ref 0.0 in
    let max_decimal_value = ref 0.0 in
    let bit_count = ref 0 in
        (* loop over bits from end to start treating them as binary digits *)
        for bit_pos = end_idx downto start_idx do
          if genome.string.(bit_pos) then
            decimal_value := !decimal_value +. (2.0 ** float_of_int !bit_count);
          max_decimal_value :=
            !max_decimal_value +. (2.0 ** float_of_int !bit_count);
          incr bit_count
        done;

        (* scale decimal_value to the range min_val to max_val *)
        min_val +. (!decimal_value /. !max_decimal_value *. (max_val -. min_val))

let rosenbrock_fitness (genome : genome_t) : float =
    (* divide genome into two halves for x and y *)
    let n = genome.length in
    let var_len = n / 2 in
    let x = decode_genome genome 0 (var_len - 1) (-2.0) 2.0 in
    let y = decode_genome genome var_len (n - 1) (-2.0) 2.0 in
        (* negate because genetic algorithm maximizes fitness *)
        -.(((1.0 -. x) ** 2.) +. (100.0 *. ((y -. (x ** 2.)) ** 2.)))

(* ------------------ printing ------------------ *)

let print_genome (genome : genome_t) : unit =
    Array.iter
      (fun bit -> print_string (if bit then "1" else "0"))
      genome.string;
    print_string ("(" ^ string_of_float genome.fitness ^ ")")

let print_population (pop : population_t) : unit =
    print_string ("population with size " ^ string_of_int pop.size ^ ":\n");
    Array.iter print_genome pop.members;
    print_newline ()

(* ------------------ initialization ------------------ *)

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

(* ------------------ mutation and crossover ------------------ *)

(* mutate genome in place based on mutation rate *)
let genome_mutate (genome : genome_t) (mutation_rate : float)
    (rng_state : Rand.rng_state) (fitness_fn : genome_t -> float) : unit =
    for i = 0 to genome.length - 1 do
      if Rand.uniform rng_state < mutation_rate then
        genome.string.(i) <- not genome.string.(i)
    done;

    (* update fitness after mutation *)
    genome.fitness <- fitness_fn genome

let population_mutate (population : population_t) (mutation_rate : float)
    (rng_state : Rand.rng_state) (fitness_fn : genome_t -> float) : unit =
    Array.iter
      (fun member -> genome_mutate member mutation_rate rng_state fitness_fn)
      population.members

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

(* ------------------ generation creation ------------------ *)

let make_new_generation_with (curr_pop : population_t) (mutation_rate : float)
    (crossover_rate : float) (rng_state : Rand.rng_state)
    (fitness_fn : genome_t -> float) : population_t =
    let fitness_scores =
        Array.map (fun member -> member.fitness) curr_pop.members
    in
    (* find minimum and sum of fitness in one swoop B) *)
    let min_fitness, sum_fitness =
        Array.fold_left
          (fun (min_acc, sum_acc) score ->
            (min min_acc score, sum_acc +. score))
          (fitness_scores.(0), fitness_scores.(0))
          fitness_scores
    in

    (* scale and normalize fitness scores to create probability distribution *)
    Array.iteri
      (fun i s -> fitness_scores.(i) <- (s -. min_fitness +. 1.) /. sum_fitness)
      fitness_scores;
    for i = 1 to Array.length fitness_scores - 1 do
      fitness_scores.(i) <- fitness_scores.(i) +. fitness_scores.(i - 1)
    done;

    (* oh yeah, closure time, since we got all of the params we need*)
    let select_parent () =
        let target = Rand.uniform rng_state in
        let rec find_index i =
            if i >= Array.length fitness_scores then
              Array.length fitness_scores - 1
            else if fitness_scores.(i) >= target then i
            else find_index (i + 1)
        in
            curr_pop.members.(find_index 0)
    in

    let new_pop =
        {
          size = curr_pop.size;
          members =
            Array.make
              curr_pop.size
              { length = 0; fitness = 0.0; string = [||] };
        }
    in
        for i = 0 to curr_pop.size - 2 do
          let child1 = copy_genome (select_parent ()) in
          let child2 = copy_genome (select_parent ()) in

          genome_mutate child1 mutation_rate rng_state fitness_fn;
          genome_mutate child2 mutation_rate rng_state fitness_fn;

          let child1, child2 =
              if Rand.uniform rng_state < crossover_rate then
                genome_1P_crossover child1 child2 rng_state fitness_fn
              else (child1, child2)
          in

          new_pop.members.(i) <- child1;
          new_pop.members.(i + 1) <- child2
        done;

        new_pop
