#!/bin/sh
# ./build_demos.sh

# x=$(date +"%F-%HH-%MM")
# cabal bench shortest-path --benchmark-options="--csv benchmarks/results/$x-shortest-path.csv"
# x=$(date +"%F-%HH-%MM")
# cabal bench pareto --benchmark-options="--csv benchmarks/results/$x-pareto.csv"
x=$(date +"%F-%HH-%MM")
cabal bench reduced-product --benchmark-options="--csv benchmarks/results/$x-reduced-product.csv"
