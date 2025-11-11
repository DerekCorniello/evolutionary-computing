(* rand.ml file made from the given C files in assignment 1 code *)

(* constants *)
let multiplier = 16807
let modulus = 2147483647
let divisor = 127773
let remainder = 2836
let table_size = 32
let ndiv = 1 + ((modulus - 1) / table_size)
let epsilon = 1.2e-7
let max_val = 1.0 -. epsilon
let default_seed = 1

type rng_state = {
    mutable seed : int;
    mutable table : int array;
    mutable last : int;
  }

(* create new rng deterministically with a default seed *)
let create () : rng_state =
    let rng = { seed = 0; table = Array.make table_size 0; last = 0 } in
        rng.seed <- -default_seed;
        rng.last <- 0;
        rng

(* create new rng with a specific deterministic seed *)
let create_with_seed (seed : int) : rng_state =
    let rng = { seed = 0; table = Array.make table_size 0; last = 0 } in
        rng.seed <- -seed;
        rng.last <- 0;
        rng

(* create new rng with a non-deterministic seed based on system randomness *)
let create_random () : rng_state =
    let rng = { seed = 0; table = Array.make table_size 0; last = 0 } in
        Random.self_init ();
        let random_seed = Random.int 1_000_000 in
            rng.seed <- -random_seed;
            rng.last <- 0;
            rng

let set_seed (rng : rng_state) (new_seed : int) : unit =
    rng.seed <- -new_seed;
    rng.last <- 0

(* generate next random float between 0.0 and 1.0 *)
let next_float (rng : rng_state) : float =
    (* init table if needed *)
    if rng.seed <= 0 || rng.last = 0 then (
      rng.seed <- (if -rng.seed < 1 then 1 else -rng.seed);

      for i = table_size + 7 downto 0 do
        let k = rng.seed / divisor in
            rng.seed <-
              (multiplier * (rng.seed - (k * divisor))) - (remainder * k);
            if rng.seed < 0 then rng.seed <- rng.seed + modulus;
            if i < table_size then rng.table.(i) <- rng.seed
      done;
      rng.last <- rng.table.(0));

    let k = rng.seed / divisor in
        rng.seed <- (multiplier * (rng.seed - (k * divisor))) - (remainder * k);
        if rng.seed < 0 then rng.seed <- rng.seed + modulus;

        let index = rng.last / ndiv in
            rng.last <- rng.table.(index);
            rng.table.(index) <- rng.seed;

            let result = float_of_int rng.last /. float_of_int modulus in
                if result > max_val then max_val else result

(* Generates a random float uniformly distributed between min_val and max_val inclusive. *)
let uniform_range (rng : rng_state) (min_val : float) (max_val : float) : float
    =
    min_val +. ((max_val -. min_val) *. next_float rng)

(* Generates a random float uniformly distributed between 0.0 and 1.0. *)
let uniform (rng : rng_state) : float = next_float rng

(* Generates a random bool with equal probability true/false. *)
let bool (rng : rng_state) : bool = next_float rng < 0.5

(* Generates a random int uniformly distributed between min_val and max_val inclusive. *)
let int (rng : rng_state) (min_val : int) (max_val : int) : int =
    let range = max_val - min_val + 1 in
        min_val + int_of_float (next_float rng *. float_of_int range)

(* Generates a random float from a Gaussian (normal) distribution with the specified mean and variance.
   Note: 'variance' is the variance (σ²), not the standard deviation (σ). For std_dev σ, pass σ² as variance.
   Uses Box-Muller transform for accurate sampling. No bounds; can return any real number. *)
let gaussian (rng : rng_state) (mean : float) (variance : float) : float =
    let rec generate_normals () =
        let u1 = uniform_range rng (-1.0) 1.0 in
        let u2 = uniform_range rng (-1.0) 1.0 in
        let s = (u1 *. u1) +. (u2 *. u2) in
            if s >= 1.0 then generate_normals ()
            else
              let factor = sqrt (-2.0 *. log s /. s) in
                  u1 *. factor
    in
    let normal = generate_normals () in
        (sqrt variance *. normal) +. mean

(* Generates a random float between left and right using a Gaussian centered at the midpoint with std_dev = |right-left|/6.
   This ensures ~99.7% of values fall within the interval, providing smooth intermediate blending. *)
let gaussian_between (rng : rng_state) (left : float) (right : float) : float =
    let mean = (left +. right) /. 2.0 in
    (* 3 sigma should span 99.7% ish of the interval,
       sampling outside won't hurt either *)
    let std_dev = abs_float (right -. left) /. 6.0 in
        gaussian rng mean (std_dev ** 2.0)
