module Rand = Util.Rand
type genome_t = {
    length: int;
    mutable fitness: float;
    string: bool array; (* arrays are mutable by default according to docs*)
}

type population_t = {
    size: int;
    members: genome_t array; (* ditto above *)
}

(* TODO: implement this *)
(* wrapper for whatever fitness func we want *)
let get_fitness (genotype: bool array) : float =
    failwith "TODO: implement get_fitness on genome"

let print_genome (genome: genome_t) : unit = 
    Array.iter (fun bit -> print_string(if bit then "1" else "0")) genome.string ;
    print_string ("(" ^ string_of_float genome.fitness ^ ")")

let print_population (pop: population_t) : unit = 
    print_string("Population with size " ^ string_of_int pop.size) ;
    Array.iter (fun genome -> print_genome genome) pop.members 

let genome_init (size : int) (rng_state : Rand.rng_state) : genome_t =
    let bits = Array.init size (fun _ -> Rand.bool rng_state) in
         { length = size; fitness = get_fitness bits; string = bits }

let population_init (member_count : int) (member_length : int) (rng_state : Rand.rng_state) : population_t =
    {
        size = member_count;
        members = Array.init member_count (fun _ -> genome_init member_length rng_state);
    }

let genome_mutate(genome : genome_t) (mutation_rate : float) (rng_state : Rand.rng_state) : unit =
    for i = 0 to genome.length - 1 do
        if Rand.uniform rng_state < mutation_rate then
            genome.string.(i) <- not genome.string.(i) 
    done;
    genome.fitness <- get_fitness genome.string

let population_mutate (population : population_t) (mutation_rate : float) (rng_state : Rand.rng_state) : unit =
    Array.iter (fun member -> genome_mutate member mutation_rate rng_state) population.members

let genome_1P_crossover (genome1 : genome_t) (genome2 : genome_t) (rng_state : Rand.rng_state) : (genome_t * genome_t) =
    if genome1.length <> genome2.length then
        failwith "genome1 and genome2 are not the same size.";
    let mutated_genotype_1 = Array.copy genome1.string in
    let mutated_genotype_2 = Array.copy genome2.string in
    let split_index = int_of_float (floor (Rand.uniform_range rng_state 1.0 (float_of_int genome1.length -. 1.0))) in
    for i = split_index to genome1.length - 1 do
        mutated_genotype_1.(i) <- genome2.string.(i);
        mutated_genotype_2.(i) <- genome1.string.(i)
    done;
    ({
        length = genome1.length;
        fitness = get_fitness mutated_genotype_1;
        string = mutated_genotype_1;
    },
    {
        length = genome2.length;
        fitness = get_fitness mutated_genotype_2;
        string = mutated_genotype_2;
    })

