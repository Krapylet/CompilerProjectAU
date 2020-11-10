#! /usr/bin/env python3.8

import sys
import json

from pathlib import Path
from subprocess import run, PIPE

# Let user pass a path on the commandline to search for tests in:
testcases_path = Path('student_tests') if len(sys.argv) == 1 else Path(sys.argv[-1])
files = sorted(testcases_path.rglob('*.tig'))

if not files:
    print('No .tig files found in directory.')
else:
    for path in files:
        try:
            run(['/bin/sh', 'tigerjs.sh', str(path)], capture_output=True, text=True)
            print('.', end='', flush=True)

        except UnicodeDecodeError as exc:
            print(f'Could not decode output when run on {path}: {exc}')

    print(f'\nGenerated {len(files)} files.')
