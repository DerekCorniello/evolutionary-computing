
set terminal wxt size 1800,600
set size square
set multiplot layout 1,3 margins 0.05,0.98,0.1,0.9 spacing 0.05,0.05
set logscale x
set xlabel 'Number of candidate evaluations'

set ylabel 'Average population performance'
plot 'es_avg.dat' with lines title 'Average Fitness'

set ylabel 'Performance of best population member'
plot 'es_best.dat' with lines title 'Best Fitness'

set ylabel 'Population diversity (Euclidean distance)'
plot 'es_diversity.dat' with lines title 'Max Distance'

unset multiplot
