module Xag.Benchmarks where

data BenchmarkInput
  = BenchmarkInput {xag :: Graph, inputOrder :: [Int], outputOrder :: [Int], testVectors :: [([Bool], [Bool])]}
  deriving (Read, Show)

readBenchmarkInput :: FilePath -> IO BenchmarkInput
readBenchmarkInput fp = do
  str <- readFile fp
  return (read str)

adder :: FilePath -> IO BenchmarkInput
adder = readBenchmarkInput "benchmarks/processed/adder.xagb"

bar :: FilePath -> IO BenchmarkInput
bar = readBenchmarkInput "benchmarks/processed/bar.xagb"

div :: FilePath -> IO BenchmarkInput
div = readBenchmarkInput "benchmarks/processed/div.xagb"

hyp :: FilePath -> IO BenchmarkInput
hyp = readBenchmarkInput "benchmarks/processed/hyp.xagb"

log2 :: FilePath -> IO BenchmarkInput
log2 = readBenchmarkInput "benchmarks/processed/log2.xagb"

max :: FilePath -> IO BenchmarkInput
max = readBenchmarkInput "benchmarks/processed/max.xagb"

multiplier :: FilePath -> IO BenchmarkInput
multiplier = readBenchmarkInput "benchmarks/processed/multiplier.xagb"

sin :: FilePath -> IO BenchmarkInput
sin = readBenchmarkInput "benchmarks/processed/sin.xagb"

sqrt :: FilePath -> IO BenchmarkInput
sqrt = readBenchmarkInput "benchmarks/processed/sqrt.xagb"

square :: FilePath -> IO BenchmarkInput
square = readBenchmarkInput "benchmarks/processed/square.xagb"
