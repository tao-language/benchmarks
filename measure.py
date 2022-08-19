from dataclasses import dataclass
import json
import resource
import subprocess
from typing import List


@dataclass
class Resources:
    stdout: str
    stderr: str
    time: float
    memory: int

    @staticmethod
    def from_json(json_data: str):
        data = json.loads(json_data)
        return Resources(**data)

    def to_json(self):
        return json.dumps(self.__dict__)


def measure(cmd: str, args: List[str]) -> Resources:
    try:
        p = subprocess.run([cmd, *args], capture_output=True, check=True)

        # https://docs.python.org/3/library/resource.html
        res = resource.getrusage(resource.RUSAGE_CHILDREN)

        return Resources(
            stdout=p.stdout.decode("utf-8").strip(),
            stderr=p.stderr.decode("utf-8").strip(),
            time=res.ru_utime + res.ru_stime,
            memory=res.ru_maxrss,
        )
    except subprocess.CalledProcessError as e:
        raise RuntimeError(e.stderr.decode("utf-8"))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("cmd")
    parser.add_argument("args", nargs="*")
    args = parser.parse_args()

    print(measure(args.cmd, args.args).to_json())
