#!/usr/bin/env python3
"""Classify watcher2 log: fork-children vs fresh execs, grouped by image."""
import sys, re
from collections import Counter, defaultdict

log = open(sys.argv[1]).read().splitlines()
cur = {}            # pid -> (path, argv) current image
parent = {}         # pid -> ppid
execs = []          # (pid, path, argv, how)
forks = []          # (pid, ppid, path)
lost = 0
line_re = re.compile(r'^([\d.]+) (\S+) (\d+) (\d+) (.*?) \| (.*)$')

for ln in log:
    m = line_re.match(ln)
    if not m:
        if ' LOST ' in ln:
            lost += 1
        continue
    t, tag, pid, aux, path, argv = m.groups()
    pid, aux = int(pid), int(aux)
    if tag == 'ROOT':
        cur[pid] = (path, argv)
        execs.append((pid, path, argv, 'root'))
    elif tag == 'CHILD':
        parent[pid] = aux
        pcur = cur.get(aux)
        cur[pid] = (path, argv)
        if pcur is not None and path == pcur[0] and argv == pcur[1]:
            forks.append((pid, aux, path))
        else:
            execs.append((pid, path, argv, 'spawn'))
    elif tag == 'EXEC':
        cur[pid] = (path, argv)
        execs.append((pid, path, argv, 'exec'))

def bucket(path, argv):
    if path.endswith('/bin/hb'):
        return 'bin/hb'
    if 'hb-under-test' in path:
        return 'hb-under-test (candidate)'
    if '/hb-stage' in path or '/hb-stdin' in path or '/hb-seed' in path:
        return 'fixpoint stage engine'
    if 'hbtmp' in path or 'hb-gate' in path or '/gate-' in path:
        return 'other scratch binary: ' + path.rsplit('/', 1)[-1]
    return path

ec = Counter(bucket(p, a) for _, p, a, _ in execs)
fc = Counter(bucket(p, '') if False else (p.rsplit('/',1)[-1]) for _, _, p in forks)

print(f"total exec events (fresh images): {len(execs)}")
for k, v in ec.most_common():
    print(f"  {v:6d}  {k}")
print(f"total fork children (no exec): {len(forks)}")
for k, v in fc.most_common():
    print(f"  {v:6d}  {k}")
print(f"lost registrations: {lost}")

# breakdown of hb execs by argv shape (first --load arg or mode)
hb = [a for _, p, a, _ in execs if bucket(p, a).startswith(('bin/hb', 'hb-under-test', 'fixpoint'))]
shapes = Counter()
for a in hb:
    m2 = re.search(r'--load (\S+)', a)
    if m2:
        # find last .f before " -- " to get entry file plus slice arg
        tail = re.search(r' -- (\S+(?: \S+)?)$', a)
        entry = a.split('--load ',1)[1]
        files = [w for w in entry.split() if w.endswith('.f')]
        key = (files[-1] if files else m2.group(1)) + (f' -- {tail.group(1)}' if tail else '')
    elif '--build' in a:
        key = '--build'
    else:
        key = a[:80] if a else '(no argv)'
    shapes[key] += 1
print("\nhb-family exec argv shapes:")
for k, v in shapes.most_common():
    print(f"  {v:6d}  {k}")
