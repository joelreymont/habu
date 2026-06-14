#!/usr/bin/env python3
r"""prop-test.py — property-based soundness test for the native checker.

Property: a CERTIFIED definition's real runtime out-arity equals its declared
`( in -- out )`. Oracle = execution in bin/habu (no gforth). See PROP-TESTING.md.

Each program runs in its OWN bin/habu process (pipe mode does not recover from a
compile error, so isolation is the only robust option; processes parallelize):

  0 set-check  + the NAB sentinel-counter helper (compiled unchecked)
  : VH CHECK! dup . cr ;   ' VH set-check        \ print the verdict
  : Gi ( sig ) body ;                            \ VH prints -1 / 0 / 1
  0 set-check
  MK <in dummies> Gi NAB . cr                    \ measured out-arity (certified only)

verdict −1 (certified): compare measured to declared → mismatch = FALSE-CERT.
A certified def that consumes too much underflows NAB and traps (no measured
line) → FALSE-CERT (consumes too much). verdict 0/1 → ignored (sound).
"""
from __future__ import annotations

import argparse
import json
import os
import random
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import prop_gen

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HABU = os.path.join(ROOT, "bin", "habu")
MK = "-987654321"
PRELUDE = (
    f"0 set-check  variable D  {MK} constant MK\n"
    ": NAB 0 D ! begin dup MK <> while drop D @ 1+ D ! repeat drop D @ ;\n"
    ": VH CHECK! dup . cr ;\n' VH set-check\n"
)


def evaluate(p: prop_gen.Program) -> dict:
    """Run one program; return {verdict, measured|None}."""
    dummies = " ".join(["7"] * p.n_in)
    script = (PRELUDE + p.definition() + "\n0 set-check\n"
              f"MK {dummies} {p.name} NAB . cr\n")
    try:
        r = subprocess.run([HABU], input=script, capture_output=True,
                           text=True, timeout=10.0)
        out = r.stdout
    except subprocess.TimeoutExpired:
        return {"verdict": None, "measured": None}
    nums = [int(x) for x in out.split() if x.lstrip("-").isdigit()]
    # first number = the verdict (VH prints it before the runner); last = measured
    verdict = nums[0] if nums else None
    measured = nums[-1] if len(nums) >= 2 else None
    return {"verdict": verdict, "measured": measured}


def classify(p: prop_gen.Program, res: dict) -> dict | None:
    if res["verdict"] != -1:
        return None                       # only certified defs can be unsound
    if res["measured"] is None:
        return {"kind": "trap", "sig": p.sig(), "body": p.body.strip(),
                "declared": p.n_out}
    if res["measured"] != p.n_out:
        return {"kind": "arity", "sig": p.sig(), "body": p.body.strip(),
                "declared": p.n_out, "measured": res["measured"]}
    return None


def is_false_cert(p: prop_gen.Program, kind: str) -> bool:
    res = evaluate(p)
    if res["verdict"] != -1:
        return False
    if kind == "trap":
        return res["measured"] is None
    return res["measured"] is not None and res["measured"] != p.n_out


def shrink(find: dict) -> dict:
    toks = find["body"].split()
    n_in = _count(find["sig"], 0)

    def mk(ts):
        return prop_gen.Program("S", n_in, find["declared"], " ".join(ts) + " ", -1, False)

    changed = True
    while changed:
        changed = False
        for i in range(len(toks)):
            trial = toks[:i] + toks[i + 1:]
            if _balanced(trial) and is_false_cert(mk(trial), find["kind"]):
                toks = trial
                changed = True
                break
    out = dict(find)
    out["minimal"] = " ".join(toks)
    return out


def _count(sig: str, side: int) -> int:
    inside = sig.strip("( )").split("--")
    return len([t for t in inside[side].split() if t])


def _balanced(toks: list[str]) -> bool:
    openers = {"if", "?do", "do", "[:", "{:", "begin"}
    closers = {"then", "loop", "+loop", ";]", ":}", "until", "repeat", "again"}
    depth = 0
    for t in toks:
        if t in openers:
            depth += 1
        elif t in closers:
            depth -= 1
            if depth < 0:
                return False
        elif t in ("else", "while") and depth == 0:
            return False
    return depth == 0


def replay(dirpath: str) -> int:
    """Each corpus line `n_in|declared_out|body` was once a false-cert; assert it
    no longer is (the checker now rejects it, or certifies and runs correctly)."""
    import glob
    regressions = []
    n = 0
    for path in sorted(glob.glob(os.path.join(dirpath, "*.txt"))):
        with open(path) as fh:
            for ln in fh:
                ln = ln.strip()
                if not ln or ln.startswith("#"):
                    continue
                n_in, declared, body = ln.split("|", 2)
                p = prop_gen.Program("R", int(n_in), int(declared), body + " ", -1, False)
                n += 1
                if is_false_cert(p, "arity") or is_false_cert(p, "trap"):
                    regressions.append(ln)
    if regressions:
        print(f"prop-test replay: {len(regressions)} REGRESSION(s) — a frozen false-cert returned:")
        for r in regressions:
            print(f"  {r}")
        return 1
    print(f"prop-test replay: {n} frozen counterexample(s), all still fixed")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--count", type=int, default=300)
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--json", action="store_true")
    ap.add_argument("--stats", action="store_true")
    ap.add_argument("--self-test", action="store_true",
                    help="prove the detector flags a fabricated false-cert, then exit")
    ap.add_argument("--replay", metavar="DIR",
                    help="regression: assert no frozen counterexample in DIR is still a false-cert")
    a = ap.parse_args()
    if a.replay:
        return replay(a.replay)
    if a.self_test:
        p = prop_gen.Program("X", 1, 1, "dup ", 2, False)
        assert classify(p, {"verdict": -1, "measured": 2}) is not None, "arity not flagged"
        assert classify(p, {"verdict": -1, "measured": None}) is not None, "trap not flagged"
        assert classify(p, {"verdict": -1, "measured": 1}) is None, "sound flagged"
        assert classify(p, {"verdict": 0, "measured": 2}) is None, "rejected flagged"
        print("self-test: detector flags arity+trap false-certs, ignores sound/rejected — OK")
        return 0
    rng = random.Random(a.seed)
    progs = [prop_gen.gen_program(rng, i) for i in range(a.count)]
    with ThreadPoolExecutor(max_workers=a.jobs) as ex:
        results = list(ex.map(evaluate, progs))
    n_cert = sum(1 for r in results if r["verdict"] == -1)
    findings = []
    for p, r in zip(progs, results):
        f = classify(p, r)
        if f:
            findings.append(shrink(f))
    report = {"seed": a.seed, "count": a.count, "certified": n_cert,
              "false_certs": len(findings), "findings": findings}
    if a.stats:
        import collections
        h = collections.Counter(r["verdict"] for r in results)
        report["verdicts"] = {str(k): v for k, v in h.items()}
    if a.json:
        print(json.dumps(report, indent=2))
    else:
        line = (f"prop-test: {a.count} programs, {n_cert} certified, "
                f"{len(findings)} FALSE-CERT(s) [seed {a.seed}]")
        if a.stats:
            line += f"  verdicts={report['verdicts']}"
        print(line)
        for f in findings:
            print(f"  FALSE-CERT [{f['kind']}] {f['sig']}  declared={f['declared']}"
                  + (f"  measured={f.get('measured')}" if "measured" in f else "")
                  + f"\n    body: {f['body']}"
                  + (f"\n    min:  {f['minimal']}" if "minimal" in f else ""))
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
