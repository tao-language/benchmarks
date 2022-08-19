import os
from dataclasses import dataclass, field
import subprocess
from measure import Resources
from typing import Optional, List


@dataclass
class Stat:
    min: float
    max: float
    avg: float

    @staticmethod
    def compute(xs: List[float]):
        return Stat(min=min(xs), max=max(xs), avg=sum(xs) / len(xs))


@dataclass
class Result:
    compile_time: float
    compile_memory: float
    run_time: Stat
    run_memory: Stat

    @staticmethod
    def compute(runs: List[Resources], compile_res: Optional[Resources] = None):
        return Result(
            compile_time=compile_res.time if compile_res else None,
            compile_memory=compile_res.memory if compile_res else None,
            run_time=Stat.compute([run.time for run in runs]),
            run_memory=Stat.compute([run.memory for run in runs]),
        )


def measure(cmd: str, source: str, args: List[str]) -> Resources:
    try:
        p = subprocess.run(
            ["python", "measure.py", cmd, source, *args],
            capture_output=True,
            check=True,
        )
        return Resources.from_json(p.stdout.decode("utf-8"))
    except subprocess.CalledProcessError as e:
        raise RuntimeError(e.stderr.decode("utf-8"))


@dataclass
class Benchmark:
    cmd: str
    source: str
    args: List[str]
    compile_cmd: str = ""
    compile_args: List[str] = field(default_factory=list)
    program: str = ""

    def run(self, repeats: int) -> Result:
        if self.compile_cmd:
            compile_res = measure(self.compile_cmd, self.source, self.compile_args)
        else:
            self.program = self.source
            compile_res = None

        runs = [measure(self.cmd, self.program, self.args) for _ in range(repeats)]
        return Result.compute(runs, compile_res)


def python(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark("python", source, args)


def ruby(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark("ruby", source, args)


def java(source: str, args: List[str] = []) -> Benchmark:
    return Benchmark(cmd="java", source=source, args=args, compile_cmd="javac")


benchmarks = [
    python("hello-world.py"),
    ruby("hello-world.rb"),
    java("HelloWorld.java"),
]


def run(repeats: int, directory: str = "src"):
    os.chdir(directory)
    for benchmark in benchmarks:
        result = benchmark.run(repeats)
        print(result)


if __name__ == "__main__":
    run(repeats=1)
