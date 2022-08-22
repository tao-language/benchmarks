import subprocess
from dataclasses import dataclass, field
import json
import os
from typing import Dict, List, Optional


@dataclass
class Benchmark:
    run: List[str]
    compile: Optional[List[str]] = None
    cleanup: List[str] = field(default_factory=list)


@dataclass
class Stats:
    min: float
    max: float
    avg: float


@dataclass
class Results:
    compile_time: float
    compile_memory: float
    # TODO: compiled file size
    run_time: Stats
    run_memory: Stats


def c(source: str, args: List[str] = [], compiler: str = "gcc") -> Benchmark:
    return Benchmark(
        compile=[compiler, "-O3", source],
        run=["./a.out", *args],
        cleanup=["a.out"],
    )


def haskell(source: str, args: List[str] = []) -> Benchmark:
    (name, _) = os.path.splitext(source)
    return Benchmark(
        compile=["ghc", source, "-O3"],
        run=["./" + name, *args],
        cleanup=[name, name + ".hi", name + ".o"],
    )


def java(source: str, args: List[str] = []) -> Benchmark:
    (mainClass, _) = os.path.splitext(source)
    return Benchmark(
        compile=["javac", source],
        run=["java", mainClass, *args],
        cleanup=[mainClass + ".class"],
    )


def python(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark(["python", source, *args])


def ruby(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark(["ruby", source, *args])


def get_stats(xs: List[float]) -> Stats:
    return Stats(min=min(xs), max=max(xs), avg=sum(xs) / len(xs))


def measure(cmd: List[str]) -> Dict[str, float]:
    try:
        p = subprocess.run(
            ["python", "measure.py", "src/", *cmd],
            capture_output=True,
            check=True,
        )
        return json.loads(p.stdout.decode("utf-8"))
    except subprocess.CalledProcessError as e:
        raise RuntimeError(e.stderr.decode("utf-8"))


def get_results(benchmark: Benchmark, repeats: int) -> Results:
    compile_results = measure(benchmark.compile) if benchmark.compile else {}
    runs_results = [measure(benchmark.run) for _ in range(repeats)]
    for filename in benchmark.cleanup:
        os.remove(os.path.join("src", filename))

    return Results(
        compile_time=compile_results.get("time"),
        compile_memory=compile_results.get("memory"),
        run_time=get_stats([run["time"] for run in runs_results]),
        run_memory=get_stats([run["memory"] for run in runs_results]),
    )
