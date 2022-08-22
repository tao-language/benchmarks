import json
import os
import resource
import subprocess
import sys
from typing import Dict, List


def measure(cmd: str, args: List[str], directory: str = ".") -> Dict[str, float]:
    os.chdir(directory)
    try:
        p = subprocess.run([cmd, *args], capture_output=True, check=True)

        # https://docs.python.org/3/library/resource.html
        res = resource.getrusage(resource.RUSAGE_CHILDREN)

        return {
            "stdout": p.stdout.decode("utf-8").rstrip(),
            "stderr": p.stderr.decode("utf-8").rstrip(),
            "time": res.ru_utime + res.ru_stime,
            "memory": res.ru_maxrss,
        }
    except subprocess.CalledProcessError as e:
        raise RuntimeError(e.stderr.decode("utf-8"))


if __name__ == "__main__":
    # Do not use argparse to not interfere with flags passed to the subprocess.
    if len(sys.argv) < 3:
        raise ValueError("usage: measure.py directory command arguments..")

    directory = sys.argv[1]
    command = sys.argv[2]
    args = sys.argv[3:]

    results = measure(command, args, directory)
    print(json.dumps(results))
