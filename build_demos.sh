cabal run fixen -- -o benchmarks/ShortestPath/FixenNoPriorities.hs demos/ShortestPath/NoPriorities.fix
cabal run fixen -- -o benchmarks/ShortestPath/FixenWithPriorities.hs demos/ShortestPath/WithPriorities.fix
cabal run fixen -- -o benchmarks/Pareto/FixenNoPriorities.hs demos/Pareto/NoPriorities.fix
cabal run fixen -- -o benchmarks/Pareto/FixenWithPriorities.hs demos/Pareto/WithPriorities.fix
cabal run fixen -- -o benchmarks/ReducedProduct/FixenNoPriorities.hs demos/StaticAnalysis/ReducedProduct.fix
cabal run fixen -- -o benchmarks/ReducedProduct/FixenWithPriorities.hs demos/StaticAnalysis/ReducedProductWithPriorities.fix
