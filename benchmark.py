import subprocess
from dataclasses import dataclass, field
import json
import os
from typing import Dict, Iterable, List, Optional


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
    run: List[str]
    compile: Optional[List[str]] = None
    cleanup: List[str] = field(default_factory=list)


@dataclass
class Benchmark:
    name: str
    repeats: int = 1
    languages: List[Language] = field(default_factory=list)

    def measure(self, cmd: List[str]) -> Dict[str, float]:
        try:
            path = os.path.join("src", self.name)
            p = subprocess.run(
                ["python", "measure.py", path, *cmd],
                capture_output=True,
                check=True,
            )
            return json.loads(p.stdout.decode("utf-8"))
        except subprocess.CalledProcessError as e:
            raise RuntimeError(e.stderr.decode("utf-8"))

    def run(self) -> Iterable[Result]:
        for lang in self.languages:
            if lang.compile:
                stats = self.measure(lang.compile)
                yield Result(
                    self.name, lang.name, "compile", stats["time"], stats["memory"]
                )

            for _ in range(self.repeats):
                stats = self.measure(lang.run)
                yield Result(
                    self.name, lang.name, "run", stats["time"], stats["memory"]
                )

            for filename in lang.cleanup:
                os.remove(os.path.join("src", self.name, filename))


def c(args: List[str] = []) -> Language:
    return Language(
        name="C",
        compile=["gcc", "-O3", "main.c"],
        run=["./a.out", *args],
        cleanup=["a.out"],
    )


def go(args: List[str] = []) -> Language:
    return Language(
        name="Go",
        compile=["go", "build", "main.go"],
        run=["./main", *args],
        cleanup=["main"],
    )


def haskell(args: List[str] = []) -> Language:
    return Language(
        name="Haskell",
        compile=["ghc", "main.hs", "-O3"],
        run=["./main", *args],
        cleanup=["main", "main.hi", "main.o"],
    )


def java(args: List[str] = []) -> Language:
    return Language(
        name="Java",
        compile=["javac", "Main.java"],
        run=["java", "Main", *args],
        cleanup=["Main.class"],
    )


def python(args: List[str] = []) -> Language:
    return Language("Python", ["python", "main.py", *args])


def ruby(args: List[str] = []) -> Language:
    return Language("Ruby", ["ruby", "main.rb", *args])
