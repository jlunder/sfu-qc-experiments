module XAG.Benchmarks where

import XAG.Graph

data BenchmarkInput = BenchmarkInput {xag :: Graph, testVectors :: [([Bool], [Bool])]}
  deriving (Read, Show)

readBenchmarkInput :: FilePath -> IO BenchmarkInput
readBenchmarkInput fp = do
  str <- readFile fp
  return (read str)

adder :: IO BenchmarkInput
adder = readBenchmarkInput "benchmarks/processed/adder.xagb"

bar :: IO BenchmarkInput
bar = readBenchmarkInput "benchmarks/processed/bar.xagb"

div :: IO BenchmarkInput
div = readBenchmarkInput "benchmarks/processed/div.xagb"

hyp :: IO BenchmarkInput
hyp = readBenchmarkInput "benchmarks/processed/hyp.xagb"

log2 :: IO BenchmarkInput
log2 = readBenchmarkInput "benchmarks/processed/log2.xagb"

max :: IO BenchmarkInput
max = readBenchmarkInput "benchmarks/processed/max.xagb"

multiplier :: IO BenchmarkInput
multiplier = readBenchmarkInput "benchmarks/processed/multiplier.xagb"

sin :: IO BenchmarkInput
sin = readBenchmarkInput "benchmarks/processed/sin.xagb"

sqrt :: IO BenchmarkInput
sqrt = readBenchmarkInput "benchmarks/processed/sqrt.xagb"

square :: IO BenchmarkInput
square = readBenchmarkInput "benchmarks/processed/square.xagb"
