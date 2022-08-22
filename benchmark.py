from enum import Enum
import subprocess
from dataclasses import dataclass, field, fields
import json
import os
from typing import Dict, Iterable, List, Optional, Tuple


@dataclass
class Result:
    name: str
    language: str
    phase: str
    time: float
    memory: float


@dataclass
class Language:
    name: str
    run_cmd: List[str]
    compile_cmd: Optional[List[str]] = None
    cleanup: List[str] = field(default_factory=list)

    @staticmethod
    def measure(bench_name: str, cmd: List[str]) -> Dict[str, float]:
        try:
            path = os.path.join("src", bench_name)
            p = subprocess.run(
                ["python", "measure.py", path, *cmd],
                capture_output=True,
                check=True,
            )
            return json.loads(p.stdout.decode("utf-8"))
        except subprocess.CalledProcessError as e:
            raise RuntimeError(e.stderr.decode("utf-8"))

    def run(self, bench_name: str, repeats: int) -> Iterable[Result]:
        if self.compile_cmd:
            stats = Language.measure(bench_name, self.compile_cmd)
            yield Result(
                bench_name, self.name, "compile", stats["time"], stats["memory"]
            )

        for _ in range(repeats):
            stats = Language.measure(bench_name, self.run_cmd)
            yield Result(bench_name, self.name, "run", stats["time"], stats["memory"])

        for filename in self.cleanup:
            os.remove(os.path.join("src", bench_name, filename))


@dataclass
class Benchmark:
    name: str
    repeats: int = 1
    languages: List[Language] = field(default_factory=list)

    def run(self) -> Iterable[Result]:
        for lang in self.languages:
            for result in lang.run(self.name, self.repeats):
                yield result


def c(args: List[str] = []) -> Language:
    return Language(
        name="C",
        compile_cmd=["gcc", "-O3", "main.c"],
        run_cmd=["./a.out", *args],
        cleanup=["a.out"],
    )


def go(args: List[str] = []) -> Language:
    return Language(
        name="Go",
        compile_cmd=["go", "build", "main.go"],
        run_cmd=["./main", *args],
        cleanup=["main"],
    )


def haskell(args: List[str] = []) -> Language:
    return Language(
        name="Haskell",
        compile_cmd=["ghc", "main.hs", "-O3"],
        run_cmd=["./main", *args],
        cleanup=["main", "main.hi", "main.o"],
    )


def java(args: List[str] = []) -> Language:
    return Language(
        name="Java",
        compile_cmd=["javac", "Main.java"],
        run_cmd=["java", "Main", *args],
        cleanup=["Main.class"],
    )


def python(args: List[str] = []) -> Language:
    return Language("Python", ["python", "main.py", *args])


def ruby(args: List[str] = []) -> Language:
    return Language("Ruby", ["ruby", "main.rb", *args])
