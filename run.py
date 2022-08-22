import benchmark as bm


benchmarks = [
    bm.c("hello-world.c"),
    bm.haskell("hello-world.hs"),
    bm.java("HelloWorld.java"),
    bm.python("hello-world.py"),
    bm.ruby("hello-world.rb"),
]


def run(repeats: int):
    for benchmark in benchmarks:
        results = bm.get_results(benchmark, repeats)
        print(results)


if __name__ == "__main__":
    run(repeats=1)
