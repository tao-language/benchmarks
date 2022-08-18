import Analyze (analyze)
import Criterion.Main

languages :: [(String, String)]
languages =
  [ ("python", ".py")
  ]

hello :: Benchmark
hello = do
  let filename = "benchmarks/hello-world/hello"
  let run (lang, ext) = bench lang $ whnfIO (analyze lang [filename ++ ext])
  bgroup "hello-world" (map run languages)

main :: IO ()
main = do
  defaultMain
    [ hello
    ]
