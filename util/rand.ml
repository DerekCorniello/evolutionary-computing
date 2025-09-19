(* rand.ml file made from the given C files in assignment 1 code *)

(* constants *)
let multiplier = 16807
let modulus = 2147483647
let divisor = 127773
let remainder = 2836
let table_size = 32
let ndiv = 1 + (modulus - 1) / table_size
let epsilon = 1.2e-7
let max_val = 1.0 -. epsilon

type rng_state = {
  mutable seed: int;
  mutable table: int array;
  mutable last: int;
}

(* create new rng with random seed *)
let create () : rng_state = 
  let rng = {
    seed = 0;
    table = Array.make table_size 0;
    last = 0;
  } in
  Random.self_init ();
  let random_seed = Random.int 1000000 in
  rng.seed <- -random_seed;
  rng.last <- 0;
  rng

let set_seed (rng : rng_state) (new_seed : int) : unit =
  rng.seed <- -new_seed;
  rng.last <- 0

(* generate next random float between 0.0 and 1.0 *)
let next_float (rng : rng_state) : float =
  (* init table if needed *)
  if rng.seed <= 0 || rng.last = 0 then begin
    rng.seed <- (if (-rng.seed) < 1 then 1 else -rng.seed);
    
    for i = table_size + 7 downto 0 do
      let k = rng.seed / divisor in
      rng.seed <- multiplier * (rng.seed - k * divisor) - remainder * k;
      if rng.seed < 0 then rng.seed <- rng.seed + modulus;
      if i < table_size then rng.table.(i) <- rng.seed
    done;
    rng.last <- rng.table.(0)
  end;
  
  let k = rng.seed / divisor in
  rng.seed <- multiplier * (rng.seed - k * divisor) - remainder * k;
  if rng.seed < 0 then rng.seed <- rng.seed + modulus;
  
  let index = rng.last / ndiv in
  rng.last <- rng.table.(index);
  rng.table.(index) <- rng.seed;
  
  let result = (float_of_int rng.last) /. (float_of_int modulus) in
  if result > max_val then max_val else result

(* uniform random float between min and max *)
let uniform_range (rng : rng_state) (min_val : float) (max_val : float) : float =
  min_val +. ((max_val -. min_val) *. (next_float rng))

let uniform (rng : rng_state) : float =
   (next_float rng)

(* uniform random bool *)
let bool (rng : rng_state) : bool =
  next_float rng < 0.5

(* uniform random int between min and max inclusive *)
let int (rng : rng_state) (min_val : int) (max_val : int) : int =
  let range = max_val - min_val + 1 in
  min_val + int_of_float (next_float rng *. float_of_int range)

(* normal distribution using Box-Muller transform *)
let gaussian (rng : rng_state) (mean : float) (variance : float) : float =
  let rec generate_normals () =
    let u1 = uniform_range rng (-1.0) 1.0 in
    let u2 = uniform_range rng (-1.0) 1.0 in
    let s = u1 *. u1 +. u2 *. u2 in
    if s >= 1.0 then generate_normals ()
    else
      let factor = sqrt ((-2.0 *. log s) /. s) in
      u1 *. factor
  in
  let normal = generate_normals () in
  sqrt variance *. normal +. mean
