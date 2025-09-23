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
module Output = Util.Output

(* ------------------ types ------------------ *)
type genome_t = {
    length : int;
    mutable fitness : float;
    string : bool array; (* arrays are mutable by default *)
  }

type population_t = { size : int; members : genome_t array }

(* ------------------ fitness functions ------------------ *)

let update_population_fitness_with (population : population_t)
    (fitness_fn : genome_t -> float) : unit =
    Array.iter (fun member -> member.fitness <- fitness_fn member) population.members

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
      (fun bit -> Output.print_string (if bit then "1" else "0"))
      genome.string;
    Output.printf " (%.3f)" genome.fitness;
    Output.print_newline ()

let print_population (pop : population_t) : unit =
    Output.print_string ("population with size " ^ string_of_int pop.size ^ ":\n");
    Array.iter print_genome pop.members;
    Output.print_newline ()

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
    let pop_size = curr_pop.size in
    let members = curr_pop.members in
    
    (* Precompute all random numbers needed for parent selection at once *)
    let num_parents = if pop_size mod 2 = 0 then pop_size else pop_size + 1 in
    let parent_targets = Array.init num_parents (fun _ -> Rand.uniform rng_state) in
    
    (* Single pass to compute min fitness and sum *)
    let min_fitness = ref max_float in
    let sum_fitness = ref 0.0 in
    
    Array.iter (fun m -> 
      min_fitness := min !min_fitness m.fitness;
      sum_fitness := !sum_fitness +. m.fitness
    ) members;
    
    (* Build roulette wheel with a single pass *)
    let roulette_wheel = Array.make pop_size 0.0 in
    let min_fitness_val = !min_fitness in
    let sum = ref 0.0 in
    
    for i = 0 to pop_size - 1 do
      let adjusted = (members.(i).fitness -. min_fitness_val) +. 1.0 in
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
    let new_members = Array.make pop_size { length = 0; fitness = 0.0; string = [||] } in
    let parent_idx = ref 0 in
    
    for i = 0 to (pop_size lsr 1) - 1 do
      let p1 = parent_indices.(!parent_idx) in
      let p2 = parent_indices.(!parent_idx + 1) in
      parent_idx := !parent_idx + 2;
      
      let child1 = copy_genome members.(p1) in
      let child2 = copy_genome members.(p2) in
      
      if Rand.uniform rng_state < crossover_rate then
        let c1, c2 = genome_1P_crossover child1 child2 rng_state fitness_fn in
        new_members.(i * 2) <- c1;
        new_members.(i * 2 + 1) <- c2
      else (
        new_members.(i * 2) <- child1;
        new_members.(i * 2 + 1) <- child2
      )
    done;
    
    if pop_size land 1 = 1 then (
      new_members.(pop_size - 1) <- copy_genome members.(parent_indices.(!parent_idx))
    );
    
    (* Apply mutations in parallel if possible *)
    Array.iteri (fun i m -> 
      if Rand.uniform rng_state < mutation_rate then
        genome_mutate m mutation_rate rng_state fitness_fn;
      new_members.(i) <- m
    ) new_members;
    
    { size = pop_size; members = new_members }
