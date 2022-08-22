import subprocess
from dataclasses import dataclass, field
import json
import os
import sys
from typing import Dict, Iterable, List, Optional, Tuple


@dataclass
class Result:
    name: str
    language: str
    phase: str
    time: float
    memory: float
    output: str


@dataclass
class Language:
    name: str
    run: List[str]
    compile: Optional[List[str]] = None
    cleanup: List[str] = field(default_factory=list)


@dataclass
class Benchmark:
    name: str
    args: List[str]
    result: str
    repeats: int = 10
    skip: List[str] = field(default_factory=list)

    def languages(self) -> List[Language]:
        return [
            Language(
                name="C",
                compile=["gcc", "-O3", "main.c"],
                run=["./a.out", *self.args],
                cleanup=["a.out"],
            ),
            Language(
                name="Go",
                compile=["go", "build", "main.go"],
                run=["./main", *self.args],
                cleanup=["main"],
            ),
            Language(
                name="Haskell",
                compile=["ghc", "main.hs", "-O3"],
                run=["./main", *self.args],
                cleanup=["main", "main.hi", "main.o"],
            ),
            Language(
                name="Java",
                compile=["javac", "Main.java"],
                run=["java", "Main", *self.args],
                cleanup=["Main.class"],
            ),
            Language("Python", ["python", "main.py", *self.args]),
            Language("Ruby", ["ruby", "main.rb", *self.args]),
        ]

    def measure(self, phase: str, lang_name: str, cmd: List[str]) -> Optional[Result]:
        try:
            path = os.path.join("src", self.name)
            p = subprocess.run(
                ["python", "measure.py", path, *cmd],
                capture_output=True,
                check=True,
            )
            stats = json.loads(p.stdout.decode("utf-8"))
            return Result(
                name=" ".join([self.name, *self.args]),
                language=lang_name,
                phase=phase,
                time=stats["time"],
                memory=stats["memory"],
                output=stats["stdout"],
            )
        except subprocess.CalledProcessError as e:
            print(e.stderr.decode("utf-8"), file=sys.stderr)
            return None

    def run(self) -> Iterable[Result]:
        for lang in self.languages():
            if lang.name in self.skip:
                continue

            if lang.compile:
                if result := self.measure("compile", lang.name, lang.compile):
                    yield result

            for _ in range(self.repeats):
                if result := self.measure("run", lang.name, lang.run):
                    assert result.output == self.result, "\n".join(
                        [
                            f"Incorrect result on {lang.name} {self.name}:",
                            result.output,
                            f"---=== Expected ===---",
                            f"{self.result}",
                        ]
                    )
                    yield result

            for filename in lang.cleanup:
                try:
                    os.remove(os.path.join("src", self.name, filename))
                except Exception as e:
                    print(e, file=sys.stderr)
