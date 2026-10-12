"""Report actual R CMD BATCH completion, including interrupted check runs."""
import argparse
import json
import math
import os
from pathlib import Path
import re

NUMBER = r'[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?'
FOOTER = re.compile(
    r'(?:^|\n)>\s*proc\.time\(\)\s*\n\s*user\s+system\s+elapsed\s*\n'
    + r'\s*(' + NUMBER + r')\s+(' + NUMBER + r')\s+(' + NUMBER + r')\s*\Z'
)


def collect(check_dir, tests_dir):
    expected = sorted(p.name for p in tests_dir.glob('*.R'))
    outputs = {}
    for path in check_dir.glob('**/tests/*.Rout*'):
        if not re.search(r'\.Rout(?:\.fail)?$', path.name):
            continue
        name = re.sub(r'\.Rout(?:\.fail)?$', '.R', path.name)
        if name in outputs:
            raise ValueError('Duplicate test output: ' + name)
        outputs[name] = path
    unknown = sorted(set(outputs) - set(expected))
    if unknown:
        raise ValueError('Unexpected test outputs: ' + ', '.join(unknown))
    records, skips = [], []
    for name in expected:
        record = {'test': name, 'status': 'not_started'}
        path = outputs.get(name)
        if path is not None:
            text = path.read_text(errors='replace')
            footer = FOOTER.search(text)
            record['status'] = 'failed' if path.name.endswith('.fail') else 'incomplete'
            if footer:
                values = [float(value) for value in footer.groups()]
                if not all(math.isfinite(value) and value >= 0 for value in values):
                    raise ValueError('Invalid test timings: ' + name)
                record.update(zip(['user_seconds', 'system_seconds', 'elapsed_seconds'], values))
                if record['status'] != 'failed':
                    record['status'] = 'completed'
            for line in text.splitlines():
                if re.match(r'^(SKIP|CmdStanR unavailable)', line):
                    skips.append(name + ': ' + line)
        records.append(record)
    return records, skips


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=['default-package', 'extended-r-only'], default='default-package')
    parser.add_argument('--check-dir', type=Path, default=Path('check'))
    parser.add_argument('--tests-dir', type=Path, default=Path('tests'))
    parser.add_argument('--output-dir', type=Path, default=Path('check'))
    args = parser.parse_args()
    records, skips = collect(args.check_dir, args.tests_dir)
    counts = {state: sum(r['status'] == state for r in records)
              for state in ['completed', 'failed', 'incomplete', 'not_started']}
    summary = {'sha': os.environ.get('BUILD_SHA', ''), 'suite': args.suite, 'expected': len(records), **counts}
    lines = [f"Checked commit: {summary['sha']}",
             f"Suite: {args.suite}; no claim of native execution, posterior recovery or scientific qualification",
             f"Successfully completed test files: {counts['completed']} / {len(records)}",
             f"Failed: {counts['failed']}; incomplete: {counts['incomplete']}; not started: {counts['not_started']}"]
    lines += [f"{r['test']}: {r['status']}" for r in records if r['status'] != 'completed']
    lines += skips
    text = '\n'.join(lines) + '\n'
    print(text)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / 'test-times.txt').write_text(
        'test\tstatus\tuser_seconds\tsystem_seconds\telapsed_seconds\n'
        + ''.join('\t'.join(str(r.get(key, '')) for key in
                            ['test', 'status', 'user_seconds', 'system_seconds', 'elapsed_seconds'])
                  + '\n' for r in records)
    )
    (args.output_dir / 'test-summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    if os.environ.get('GITHUB_STEP_SUMMARY'):
        with open(os.environ['GITHUB_STEP_SUMMARY'], 'a') as output:
            output.write('## Engineering test coverage\n\n' + text)
    if not records or counts['completed'] != len(records):
        raise SystemExit('Package tests did not all finish successfully')


if __name__ == '__main__':
    main()
