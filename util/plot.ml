let plot_performance prefix evaluations avg_fitnesses best_fitnesses diversities =
  (* Output data to files *)
  let output_data filename x y =
    let oc = open_out filename in
    List.iter2 (fun a b -> Printf.fprintf oc "%.0f %.6f\n" a b) x y;
    close_out oc
  in
  output_data (prefix ^ "avg.dat") evaluations avg_fitnesses;
  output_data (prefix ^ "best.dat") evaluations best_fitnesses;
  output_data (prefix ^ "diversity.dat") evaluations diversities;

  (* Gnuplot script *)
  let script = Printf.sprintf "
set terminal wxt size 1800,600
set size square
set multiplot layout 1,3 margins 0.05,0.98,0.1,0.9 spacing 0.05,0.05
set logscale x
set xlabel 'Number of candidate evaluations'

set ylabel 'Average population performance'
plot '%savg.dat' with lines title 'Average Fitness'

set ylabel 'Performance of best population member'
plot '%sbest.dat' with lines title 'Best Fitness'

set ylabel 'Population diversity (Euclidean distance)'
plot '%sdiversity.dat' with lines title 'Max Distance'

unset multiplot
" prefix prefix prefix in
  let oc = open_out (prefix ^ "plot.gp") in
  output_string oc script;
  close_out oc;

  (* Run gnuplot *)
  ignore (Sys.command ("gnuplot -p " ^ prefix ^ "plot.gp"))