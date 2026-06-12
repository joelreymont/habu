#!/usr/bin/env python3
"""parity-lint: token-diff paired emitter definitions between the gforth
bootstrap builder and the habu-side port. The recurring bug class is silent
divergence (instruction order, template constants); the goldens only say
"bytes differ" — this names the word and the first divergent token.
Pairs are matched by definition name across the file pairs below."""
import re, sys

PAIRS = [
    ('bootstrap/cg/forth.fs', ['src/habu/habu1.f', 'src/habu/habu2.f']),
    ('bootstrap/cg/treeshake.fs', ['src/habu/treeshake.f']),
    ('bootstrap/cg/regalloc.fs', ['src/habu/regalloc.f']),
    ('bootstrap/cg/jit.fs', ['src/habu/jit.f']),
    ('bootstrap/cg/prof.fs',  ['src/habu/prof.f']),
    ('bootstrap/cg/rt.fs',    ['src/habu/rt.f']),
    ('bootstrap/cg/crash.fs', ['src/habu/crash.f']),
]
LBL = re.compile(r'NEWLBL \{: (\w[\w-]*) :\}')
GRP = re.compile(r'\{:([^}]*):\}')

def defs(path):
    s = open(path, encoding='utf-8', errors='surrogateescape').read()
    s = re.sub(r'\\[^\n]*', '', s)
    s = re.sub(r'\( [^)]*?\)', '', s)
    out = {}
    for m in re.finditer(r'(?m)^: ([^\s]+)\s(.*?);\s*$', s, re.S):
        name, body = m.group(1), m.group(2)
        labels = set(LBL.findall(body))
        for g in GRP.findall(body):
            labels.update(w for w in g.split() if w != '--')
        body = LBL.sub('', body)
        body = re.sub(r'NEWLBL', '', body)
        body = GRP.sub('', body)
        toks = [t.lower() for t in body.split() if t not in labels]
        out[name.lower()] = toks
    return out

# words whose two implementations legitimately differ structurally (gforth idioms
# vs subset idioms: swap/?do/locals-helpers). Their OUTPUT is still byte-golden-
# checked; the lint covers the near-transcription emitters where divergence hides.
ALLOW = {a.lower() for a in (
    'EMIT-FORTH','FPRIM','FPRIM-L','c-emitw','cf-entry','cfn-entry','emit-dict',
    'emit-main','reg-prim','(sigact)','emit-crash-handler','emit-source','CRH-INIT')}

fail = 0
for boot, ports in PAIRS:
    b = defs(boot)
    p = {}
    for f in ports: p.update(defs(f))
    for name in sorted((set(b) & set(p)) - ALLOW):
        x, y = b[name], p[name]
        for i, (a, c) in enumerate(zip(x, y)):
            if a != c:
                print(f"DIVERGE {name} @tok{i}: boot={x[max(0,i-3):i+4]} port={y[max(0,i-3):i+4]}")
                fail += 1; break
        else:
            if len(x) != len(y):
                print(f"LENGTH {name}: boot={len(x)} port={len(y)} tail boot={x[len(y):][:5]} port={y[len(x):][:5]}")
                fail += 1
print(f"parity-lint: {fail} divergence(s)")
sys.exit(1 if fail else 0)
