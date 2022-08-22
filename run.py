import csv
import sys
from typing import Iterable, List
import benchmark as bm


benchmarks = [
    bm.c("hello-world.c"),
    bm.go("hello-world.go"),
    bm.haskell("hello-world.hs"),
    bm.java("HelloWorld.java"),
    bm.python("hello-world.py"),
    bm.ruby("hello-world.rb"),
]


def print_table(results: Iterable[bm.Result]):
    for i, result in enumerate(results):
        result_dict = result.to_dict()
        if i == 0:
            column_names = [f"{name:10}" for name in result_dict.keys()]
            print("\t".join([" " * 20, *column_names]))

        row = [
            f"{value:10.4f}" if value is not None else " " * 10
            for value in result_dict.values()
        ]
        print("\t".join([f"{result.name:20}", *row]))


def print_csv(results: Iterable[bm.Result]):
    writer = csv.writer(sys.stdout)
    for i, result in enumerate(results):
        result_dict = result.to_dict()
        if i == 0:
            column_names = list(result_dict.keys())
            writer.writerow(["name", *column_names])

        row = [value for value in result_dict.values()]
        writer.writerow([result.name, *row])


def run(benchmarks: List[bm.Benchmark], repeats: int) -> Iterable[bm.Result]:
    for benchmark in benchmarks:
        yield bm.get_results(benchmark, repeats)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--repeats", type=int, default=1)
    parser.add_argument("--format", choices=["table", "csv"], default="table")
    args = parser.parse_args()

    results = run(benchmarks, args.repeats)
    if args.format == "table":
        print_table(results)
    elif args.format == "csv":
        print_csv(results)
    else:
        raise ValueError(f"unsupported --format: {args.format}")
