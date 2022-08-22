import subprocess
from dataclasses import dataclass, field, replace
import json
import os
import sys
from typing import Dict, Iterable, List, Optional, Tuple


@dataclass
class Result:
    name: str
    language: str
    phase: str
    time_sec: float
    memory_mb: float
    output: str

    def trim_output(self):
        output = (
            self.output
            if len(self.output) < 20
            else f"{self.output[:15]} ({len(self.output)} characters)"
        )
        return replace(self, output=output)


@dataclass
class Language:
    name: str
    source: str
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
                source="main.c",
                compile=["gcc", "-O3", "main.c"],
                run=["./a.out", *self.args],
                cleanup=["a.out"],
            ),
            Language(
                name="Go",
                source="main.go",
                compile=["go", "build", "main.go"],
                run=["./main", *self.args],
                cleanup=["main"],
            ),
            Language(
                name="Haskell",
                source="main.hs",
                compile=["ghc", "-O3", "main.hs"],
                run=["./main", *self.args],
                cleanup=["main", "main.hi", "main.o"],
            ),
            Language(
                name="Java",
                source="Main.java",
                compile=["javac", "Main.java"],
                run=["java", "Main", *self.args],
                cleanup=["Main.class"],
            ),
            Language("Python", "main.py", ["python", "main.py", *self.args]),
            Language("Ruby", "main.rb", ["ruby", "main.rb", *self.args]),
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
                time_sec=stats["time"],
                memory_mb=stats["memory"] / 1024.0 / 1024.0,
                output=stats["stdout"],
            )
        except subprocess.CalledProcessError as e:
            raise RuntimeError(e.stderr.decode("utf-8"))

    def run(self) -> Iterable[Result]:
        for lang in self.languages():
            if lang.name in self.skip:
                continue

            source = os.path.join("src", self.name, lang.source)
            if not os.path.exists(source):
                print(
                    f"⚠️ [Skipping {self.name} in {lang.name}] File not found: {source}",
                    file=sys.stderr,
                )
                continue

            if lang.compile:
                if result := self.measure("compile", lang.name, lang.compile):
                    yield result

            for _ in range(self.repeats):
                if result := self.measure("run", lang.name, lang.run):
                    assert result.output == self.result, "\n".join(
                        [
                            f"Incorrect result on {lang.name} {self.name}:",
                            result.output
                            if len(result.output) < 100
                            else f"{result.output[:20]} ... {result.output[-20:]}",
                            f"---=== Expected ===---",
                            f"{self.result}"
                            if len(self.result) < 100
                            else f"{self.result[:20]} ... {self.result[-20:]}",
                        ]
                    )
                    yield result.trim_output()

            for filename in lang.cleanup:
                try:
                    os.remove(os.path.join("src", self.name, filename))
                except Exception as e:
                    print(e, file=sys.stderr)
