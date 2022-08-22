import csv
import sys
from typing import Iterable, List
import benchmark as bm


benchmarks = [
    bm.Benchmark("hello-world", ["Hi! 😊"], result="Hi! 😊"),
    bm.Benchmark("fibonacci", ["20"], result="6765"),
    bm.Benchmark("fibonacci", ["30"], result="832040"),
]


def print_table(results: Iterable[bm.Result]):
    for i, result in enumerate(results):
        if i == 0:
            column_names = [f"{name:20}" for name in result.__dict__.keys()]
            print("\t".join(column_names))

        row = [
            f"{f'{value:.4f}':20}" if type(value) == float else f"{str(value):20}"
            for value in result.__dict__.values()
        ]
        print("\t".join(row))


def print_csv(results: Iterable[bm.Result]):
    writer = csv.writer(sys.stdout)
    for i, result in enumerate(results):
        if i == 0:
            writer.writerow(result.__dict__.keys())
        writer.writerow(result.__dict__.values())


def run(benchmarks: List[bm.Benchmark]) -> Iterable[bm.Result]:
    for bench in benchmarks:
        for result in bench.run():
            yield result


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--repeats", type=int, default=10)
    parser.add_argument("--format", choices=["table", "csv"], default="table")
    args = parser.parse_args()

    results = run(benchmarks)
    if args.format == "table":
        print_table(results)
    elif args.format == "csv":
        print_csv(results)
    else:
        raise ValueError(f"unsupported --format: {args.format}")
