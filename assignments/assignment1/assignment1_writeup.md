# SGA Task 3 Implementation Report - Derek Corniello

## Task 3: Rosenbrock Representation

For Task 3, I used a binary genome representation to optimize the Rosenbrock function.
Each genome is a boolean array (`bool array`) of length `genome_length = 64`, encoding two variables 
(`x`, `y`) with `bits_per_variable = 32` (see `assignments/assignment1/task3.ml`). A fitness value
(`float`) that is the Rosenbrock function value, which is minimized.

The decoding maps each 32-bit slice to the continuous range `[-2.0, 2.0]`, with big-endian interpretation,
designed so that `x = 1.0` and `y = 1.0` are exactly representable (see `decode_genome` in `task3.ml`).

## Why This Representation for Task 3

I chose binary encoding with explicit decoding for Rosenbrock because of the following:

1. Compatibility with simple operators. Works naturally with bit-flip mutation and uniform crossover
   used in Task 3.
2. Exact representability of the optimum. The `[-2, 2]` mapping was chosen so `(x, y) = (1, 1)` is
   exactly encodable, addressing a common pitfall where the true optimum lies between discrete codes.
3. Balance of precision and efficiency. 32 bits per variable offers fine precision while keeping genome
   compact for a population of 1000.
4. Minimal changes across tasks. Reuses the same genome structure as other tasks, with only the fitness
   and decoding logic changing.

## Task 3 Algorithm Design

The differences in Task 3 from the other implementations are in selection and crossover while treating
Rosenbrock as a minimization problem:

- Selection: Tournament selection (size 3) biased toward lower fitness (better solutions), instead of
  roulette selection. This increases selection pressure and stability on noisy or skewed landscapes.
- Crossover: Uniform crossover with 0.6 probability (`uniform_crossover` in `task3.ml`), mixing parental
  bits independently, which helps explore along Rosenbrock's curved valley.
- Mutation: Bit-flip mutation with rate 0.001, providing steady local exploration.
- Elitism: The best individual is copied to the next generation to prevent regression.
- Parameters: `population_size = 1000`, `genome_length = 64`, `mutation_rate = 0.001`,
  `crossover_rate = 0.6`, `max_generations = 1000`.
- Convergence tracking: Logging includes champion, average fitness, a convergence ratio, and the
  decoded `(x, y)` of the best-so-far individual each generation.

## Task 3 Results: Rosenbrock

Using the above configuration, the algorithm minimized the Rosenbrock function down to values effectively the
same as zero (see `corniedj-rosenbrock-1000-64-0_001-0_60.txt`). Throughout the run, the decoded best-so-far
`(x, y)` values approach `(1.0, 1.0)`, consistent with the known global minimum.

- Near-optimal convergence: Best fitness falls to ~0.0 within, on average, 65 generations, and
  repeatedly returns values at or below 1e-5 on average.
- Stable tracking of the valley: Uniform crossover plus elitism avoids losing the ridge/valley structure, 
  improving stability relative to 1-point crossover.
- Runtime: On my machine, runs complete in under a second on average with the above parameters.

In practice, the SGA converges extremely close to the global minimum and often to the exact encodable
optimum, but may show some tiny positive residuals.

## Conclusion for Task 3

For Task 3, the binary-encoded SGA with 32 bits per variable, uniform crossover, tournament selection,
and elitism is well-suited to the Rosenbrock function. The representation encodes the true optimum exactly,
and the operator choices enable the population to follow the curved valley efficiently. Empirically,
the algorithm converges to effectively zero objective value and `(x, y)` near `(1, 1)` within the allotted
generations, demonstrating that this SGA can find (or get arbitrarily close to) the global minimum for this 
problem under the chosen precision and runtime budget.

### Additional Implementation Notes

Initially, I considered using a range of [-2.048, 2.048] for easier bit conversion. However, I realized this
would make it impossible to exactly represent the optimal point at (1, 1) due to the discrete nature of
binary encoding. Therefore, I changed it to a range of [-2, 2] to ensure the global optimum is exactly 
representable.

While adjusting population size and mutation/crossover rates provided ok results, implementing tournament
selection, uniform crossover, and elitism led to significantly better performance. These changes improved
the algorithm's ability to navigate the Rosenbrock function's curved valley and maintain diversity in the
population. This set of changes was done after we covered them in class.
