import benchmark as bm


benchmarks = [
    bm.python("hello-world.py"),
    bm.ruby("hello-world.rb"),
    bm.java("HelloWorld.java"),
]


def run(repeats: int):
    for benchmark in benchmarks:
        results = bm.get_results(benchmark, repeats)
        print(results)


if __name__ == "__main__":
    run(repeats=1)
