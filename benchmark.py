import subprocess
from dataclasses import dataclass
import json
import os
from typing import Dict, List, Optional


@dataclass
class Benchmark:
    run: List[str]
    compile: Optional[List[str]] = None


@dataclass
class Stats:
    min: float
    max: float
    avg: float


@dataclass
class Results:
    compile_time: float
    compile_memory: float
    run_time: Stats
    run_memory: Stats


def python(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark(["python", source, *args])


def ruby(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark(["ruby", source, *args])


def java(source: str, args: List[str] = []) -> Benchmark:
    (mainClass, _) = os.path.splitext(source)
    return Benchmark(
        compile=["javac", source],
        run=["java", mainClass, *args],
    )


def get_stats(xs: List[float]) -> Stats:
    return Stats(min=min(xs), max=max(xs), avg=sum(xs) / len(xs))


def measure(cmd: List[str]) -> Dict[str, float]:
    try:
        p = subprocess.run(
            ["python", "measure.py", "--dir=src/", *cmd],
            capture_output=True,
            check=True,
        )
        return json.loads(p.stdout.decode("utf-8"))
    except subprocess.CalledProcessError as e:
        raise RuntimeError(e.stderr.decode("utf-8"))


def get_results(benchmark: Benchmark, repeats: int) -> Results:
    compile_results = measure(benchmark.compile) if benchmark.compile else {}
    runs_results = [measure(benchmark.run) for _ in range(repeats)]
    return Results(
        compile_time=compile_results.get("time"),
        compile_memory=compile_results.get("memory"),
        run_time=get_stats([run["time"] for run in runs_results]),
        run_memory=get_stats([run["memory"] for run in runs_results]),
    )
