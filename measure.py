import json
import os
import resource
import subprocess
from typing import Dict, List


def measure(cmd: str, args: List[str], directory: str = ".") -> Dict[str, float]:
    os.chdir(directory)
    try:
        p = subprocess.run([cmd, *args], capture_output=True, check=True)

        # https://docs.python.org/3/library/resource.html
        res = resource.getrusage(resource.RUSAGE_CHILDREN)

        return {
            "stdout": p.stdout.decode("utf-8").strip(),
            "stderr": p.stderr.decode("utf-8").strip(),
            "time": res.ru_utime + res.ru_stime,
            "memory": res.ru_maxrss,
        }
    except subprocess.CalledProcessError as e:
        raise RuntimeError(e.stderr.decode("utf-8"))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("cmd")
    parser.add_argument("args", nargs="*")
    parser.add_argument("--dir", default=".")
    args = parser.parse_args()

    results = measure(args.cmd, args.args, args.dir)
    print(json.dumps(results))
