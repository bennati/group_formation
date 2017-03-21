#!/bin/bash
for dir in figure_static figure_evolution_static figure_evolution_static_famine; do #  figure_nostatic figure_evolution_nostatic
  make clean
  make -B all -f $dir/Makefile
  cd $dir
  cp ../test test_$dir
  chmod +x test_$dir
  ../run.sh ./params.sh test_$dir
  cd ..
done

## after execution is done
# Rscript plot_figures.R
