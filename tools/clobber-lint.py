#!/usr/bin/env python3
"""clobber-lint: register-clobber analysis for the engine emitters.

The engine's BL-able routines (Lxxx) are hand-allocated assembly written in the
builder DSL (`rd rn imm OP,`). Twice now a routine's scratch register was a
caller's live register (Lbcap x6 vs c-lbrace's block start; Lbcap x15 vs
c-constant's popped value). This lint computes each routine's CLOBBER SET and
flags call sites where a register written before `Lxxx @ BL,` is read after it
while being in the callee's clobber set.

Model:
- A routine's region runs from its `Lxxx @ LBL,` to the first RET,/B, that
  immediately precedes another `Lyyy @ LBL,` opening (fall-through entries like
  Lbcap -> Lbcs stay folded together).
- Clobber sets close over the BL graph.
- RETURNS: registers a routine deliberately returns/updates (Lcfpop -> x9,
  Lkwcmp -> x0, Lcemit advances CP...). Reads after the BL are the contract,
  and the callee's write doesn't poison; a LATER call can still poison them.
- PRESERVES: registers a routine saves/restores on every RETURNING path
  (frame save/restore, or writes only on a no-return fatal `exit` path).
- CONTRACT_REGS are never tracked: x30 (every frame routine saves/restores LR
  around inner BLs — the lint can't see stack slots), x31 (SP, callee-balanced;
  also encodes XZR), x28 (CP: every emitting routine advances it by design —
  all CP writes funnel through Lcemit).
- Builder dispatch entries (cf-entry, vop-entry, ...) are pseudo-instructions:
  on the fall-through path only Lkwcmp has run (clobbers x0,x2-x5).
- Approximations: linear scan (builder IF/loops treated as straight-line);
  BLR, (indirect hook calls) writes x0-x17 but does not poison.

False positives go in ALLOW with a reason.
"""
import re, sys, pathlib

FILES = [
    "bootstrap/cg/forth.fs", "bootstrap/cg/jit.fs", "bootstrap/cg/regalloc.fs", "bootstrap/cg/prof.fs",
    "bootstrap/cg/rt.fs", "bootstrap/cg/crash.fs",
    "src/habu/habu1.f", "src/habu/habu2.f", "src/habu/jit.f", "src/habu/regalloc.f",
    "src/habu/prof.f", "src/habu/rt.f", "src/habu/crash.f",
]

REGNAMES = {"XDS": 19, "SP": 31, "A": 9, "B": 10, "C": 11,
            "RBASE": 20, "INP": 21, "INE": 22, "TKA": 23, "TKL": 24,
            "PEND": 25, "DBASE": 26, "NDICT": 27, "CP": 28, "DATA": 20}

CONTRACT_REGS = {28, 30, 31}   # CP / LR / SP-XZR — see module docstring

# label -> registers it deliberately returns; reading them after BL is the contract
RETURNS = {
    "Lcfpop": {9},                   # ( -> x9 )
    "Lkwcmp": {0},                   # ( x0 x1 -> x0=match? )
    "Lloc-find": {0},                # ( -> x0=slot | -1 )
    "Ltok": {0, 21, 23, 24},         # ( -> x0=have? ) + INP/TKA/TKL advance
    "Lfind": {11, 12, 13},           # ( x9 x10 -> x11=addr x12=clen x13=found )
    "Lnum": {11, 12},                # ( -> x11=val x12=ok )
    "Lvralloc": {14},                # ( -> x14=reg | 0 )
    "Lvforcek": {14},                # ( x5 -> x14=reg | 0 )
    "Lvtop2c": {11, 12, 13},         # ( -> x13=ok x11=a x12=b )
    "Lvbinprep": {11, 12, 13, 14, 15},
    "Lvdrop": {13}, "Lvswapx": {13}, "Lvnipx": {13}, "Lvcopy": {13},
}

# label -> registers untouched on every RETURNING path (frame-saved, or only
# written on a no-return fatal exit path)
PRESERVES = {
    "Lvpushc": {11},                 # frame-saves its x11=val argument
    "Lvpushr": {14},                 # frame-saves its x14=reg argument
    "Lvforcek": {5},                 # frame-saves its x5=k argument
    "Lbcap": {0, 1, 2, 16},          # x0-x2/x16 written only on the exit(71) path
    "Lbcs": {0, 1, 2, 16},
}

# mnemonic classes
W3 = {"ADD,", "SUB,", "MUL,", "AND,", "ORR,", "EOR,", "LSLV,", "LSRV,", "SDIV,"}
W2I = {"ADDI,", "SUBI,", "LSLI,", "LSRI,", "ASRI,", "ANDI,"}
W1 = {"MOVZ,", "MOVN,", "ADR,", "LIT64,", "CSET,"}
WRMW = {"MOVK,", "MOVZHW,", "MOVKHW,", "MOVNHW,"}
LD = {"LDR,", "LDRB,", "LDRW,"}
ST = {"STR,", "STRB,", "STRW,"}
NOREG = {"LBL,", "DCQ,", "DLBL,", "BYTES,", "B,", "BCOND,", "RET,", "DCD,"}

# builder pseudo-instructions: (writes, reads) as absolute registers, except
# g-push/g-pop where 0 means "the operand register"
PSEUDO = {
    "g-push": (set(), {0}),
    "g-pop": ({0}, set()),
    "g-print9": ({0, 1, 2, 16}, {9}),
    "c-lit": ({5, 6, 7, 8, 9, 30}, {11}),
    "c-call": ({5, 7, 8, 9, 10, 13, 14, 15, 30}, {11, 12}),
    "c-popflag": ({9, 19}, set()),
    "c-pushcp": ({9, 30}, set()),
    "c-emitw": ({9, 30}, set()),
    "c-bback": ({5, 9, 10, 30}, {9}),
}
# dispatch-entry builder words: the linear (fall-through) path has only run
# Lkwcmp — clobbers x0,x2-x5, leaves x0=0
KWCMP_CLOB = {0, 2, 3, 4, 5}
for entry in ("cf-entry", "cfb-entry", "fold-entry", "vop-entry",
              "vcmp-entry", "vshuf-entry", "vun-entry"):
    PSEUDO[entry] = (set(KWCMP_CLOB), set())

# normalize: source words are UPPER-CASE now (docs/forth.md); all name keys and
# source-derived names compare case-folded
RETURNS = {k.lower(): v for k, v in RETURNS.items()}
PRESERVES = {k.lower(): v for k, v in PRESERVES.items()}
PSEUDO = {k.lower(): v for k, v in PSEUDO.items()}

# (file, word, reg, callee) — verified false positives
ALLOW = {
    # the only x1/x2 "read" after the Lhex BLs is the final exit(134) SVC;
    # the SVC model reads x0-x2/x16 but exit uses only x0/x16 — x1/x2 are dead
    ("bootstrap/cg/crash.fs", "emit-crash-handler", 1, "lhex"),
    ("bootstrap/cg/crash.fs", "emit-crash-handler", 2, "lhex"),
    ("src/habu/crash.f", "emit-crash-handler", 1, "lhex"),
    ("src/habu/crash.f", "emit-crash-handler", 2, "lhex"),
}

def tokens_of(path):
    out = []
    for ln in pathlib.Path(path).read_text().splitlines():
        ln = ln.split("\\")[0]
        out.extend(ln.split())
    return out

def reg_of(tok):
    if tok in REGNAMES: return REGNAMES[tok]
    if re.fullmatch(r"\d+", tok):
        v = int(tok)
        return v if v < 32 else None
    return None

def parse_words(toks):
    """yield (name, body tokens) for each colon definition"""
    i = 0
    while i < len(toks):
        if toks[i] == ":" and i + 1 < len(toks):
            name = toks[i + 1].lower()
            j = i + 2
            body = []
            while j < len(toks) and toks[j] != ";":
                body.append(toks[j]); j += 1
            yield name, body
            i = j + 1
        else:
            i += 1

def callee_of(ops):
    """`Lxxx @ BL,` call target, else None"""
    if len(ops) >= 2 and ops[-1] == "@" and ops[-2][:1].upper() == "L":
        return ops[-2].lower()
    return None

def instr_stream(body):
    """yield (mnemonic, accumulated operand tokens)"""
    ops = []
    for t in body:
        if (t.endswith(",") and t.upper() == t and not t.startswith("$")) \
           or t.lower() in PSEUDO:
            yield t, ops
            ops = []
        else:
            ops.append(t)

def effects(mn, ops):
    """return (writes, reads) register sets for one instruction"""
    regs = [reg_of(t) for t in ops]
    rr = [r for r in regs if r is not None]
    W, R = set(), set()
    def w(i):
        if i < len(rr): W.add(rr[i])
    def r(i):
        if i < len(rr): R.add(rr[i])
    if mn in W3: w(0); r(1); r(2)
    elif mn in W2I: w(0); r(1)
    elif mn in W1: w(0)
    elif mn in WRMW:
        if rr: W.add(rr[0]); R.add(rr[0])
    elif mn in LD: w(0); r(1)
    elif mn in ST: r(0); r(1)
    elif mn == "CMP,": r(0); r(1)
    elif mn == "CMPI,": r(0)
    elif mn in ("CBZ,", "CBNZ,"): r(0)
    elif mn == "SVC,": W |= {0}; R |= {0, 1, 2, 16}
    elif mn == "SYS,": W |= {0, 16}; R |= {0, 1, 2}   # NR-x SYS, = movz x16 + svc
    elif mn == "RET,": R.add(30)
    elif mn == "BLR,": W |= set(range(0, 18)) | {30}; r(0)
    elif mn.lower() in PSEUDO:
        pw, pr = PSEUDO[mn.lower()]
        for x in pw:
            if x == 0 and mn.lower() == "g-pop": w(0)
            else: W.add(x)
        for x in pr:
            if x == 0 and mn.lower() == "g-push": r(0)
            else: R.add(x)
        if mn.lower() in ("g-push", "g-pop"): W.add(19); R.add(19)
    return W, R

def label_openings(body):
    """token indexes k where body[k:k+3] is `Lxxx @ LBL,`"""
    return [k for k in range(len(body) - 2)
            if body[k][:1].upper() == "L" and body[k + 1] == "@"
            and body[k + 2] == "LBL,"]

def routine_region(body, openings, oi):
    """instruction region of opening oi: runs through later openings while the
    instruction just before them falls through (not RET,/B,)"""
    start = openings[oi] + 3
    last_mn = None
    region = []
    nxt = oi + 1
    k = start
    ops = []
    while k < len(body):
        if nxt < len(openings) and k == openings[nxt]:
            if last_mn in ("RET,", "B,"):
                break
            nxt += 1
            k += 3       # skip the `Lyyy @ LBL,` opening itself
            continue
        t = body[k]
        if (t.endswith(",") and t.upper() == t and not t.startswith("$")) \
           or t.lower() in PSEUDO:
            region.append((t, ops)); last_mn = t; ops = []
        else:
            ops.append(t)
        k += 1
    return region

def main():
    # pass 1: routine clobber sets + BL graph
    clob = {}
    words = {}
    for f in FILES:
        p = pathlib.Path(f)
        if not p.exists(): continue
        for name, body in parse_words(tokens_of(f)):
            words.setdefault(name, []).append((f, body))
            openings = label_openings(body)
            for oi in range(len(openings)):
                lbl = body[openings[oi]].lower()
                ws, calls = clob.get(lbl, (set(), set()))
                for mn, ops in routine_region(body, openings, oi):
                    c = callee_of(ops)
                    if mn == "BL," and c:
                        calls.add(c)
                        continue
                    W, _R = effects(mn, ops)
                    ws |= W
                clob[lbl] = (ws, calls)
    # close over the BL graph, applying PRESERVES at each routine's boundary
    changed = True
    while changed:
        changed = False
        for lbl, (ws, calls) in list(clob.items()):
            eff = set(ws)
            for c in calls:
                eff |= clob.get(c, (set(), set()))[0]
            eff -= PRESERVES.get(lbl, set())
            if eff != ws:
                clob[lbl] = (eff, calls); changed = True
    # pass 2: call-site liveness per word
    bad = 0
    for name, defs in words.items():
        for f, body in defs:
            dirty = {}      # reg -> True since last write
            poisoned = {}   # reg -> callee that clobbered it while dirty
            for mn, ops in instr_stream(body):
                callee = callee_of(ops)
                if mn == "BL," and callee:
                    rets = RETURNS.get(callee, set())
                    cw = clob.get(callee, (set(), set()))[0] \
                         - CONTRACT_REGS - rets
                    for r in list(dirty):
                        if r in cw:
                            poisoned[r] = callee
                    for r in rets:
                        dirty[r] = True; poisoned.pop(r, None)
                    continue
                W, R = effects(mn, ops)
                for r in R - CONTRACT_REGS:
                    if r in poisoned and r in dirty:
                        key = (f, name, r, poisoned[r])
                        if key not in ALLOW:
                            print(f"CLOBBER {f} {name}: x{r} written, "
                                  f"clobbered by {poisoned[r]}, then read")
                            bad += 1
                        del poisoned[r]
                for r in W - CONTRACT_REGS:
                    dirty[r] = True
                    poisoned.pop(r, None)
    if bad:
        print(f"clobber-lint: {bad} finding(s)"); sys.exit(1)
    print("clobber-lint: clean")

main()
