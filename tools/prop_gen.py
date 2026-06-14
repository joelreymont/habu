#!/usr/bin/env python3
"""prop_gen.py — seeded generator of well-formed, RUNNABLE, terminating typed
habu definitions over the safe integer sublanguage. See PROP-TESTING.md.

Every program is built by a stack-depth-tracked random walk: at each step only an
op whose input arity <= current depth is chosen, so the body never underflows its
inputs at generation time (it is runnable with `in_count` integer dummies). The
walk tracks the true net depth change, so we know the body's real out-arity and
can declare a signature that either matches (intended-certify) or is perturbed by
+-1 (intended-reject). All data-stack values stay int-typed; comparisons appear
ONLY immediately before `if` (the flag is consumed at once), so declared `i64`
outputs always unify and the certified yield stays high.
"""
from __future__ import annotations

import random
from dataclasses import dataclass


@dataclass
class Program:
    name: str
    n_in: int
    n_out: int          # DECLARED out-arity (may be perturbed away from true_out)
    body: str
    true_out: int       # the generator's tracked real out-arity
    perturbed: bool     # declared != true_out on purpose (intended-reject)

    def sig(self) -> str:
        ins = " ".join(["i64"] * self.n_in)
        outs = " ".join(["i64"] * self.n_out)
        return f"( {ins} -- {outs} )"

    def definition(self) -> str:
        return f": {self.name} {self.sig()} {self.body};"


# ---- leaf ops: (token, need, delta) over int-typed values ----
_LEAF = [
    ("dup", 1, +1), ("drop", 1, -1), ("swap", 2, 0), ("over", 2, +1),
    ("nip", 2, -1), ("rot", 3, 0),
    ("+", 2, -1), ("-", 2, -1), ("*", 2, -1),
    ("and", 2, -1), ("or", 2, -1), ("xor", 2, -1),
    ("1+", 1, 0), ("1-", 1, 0), ("negate", 1, 0),
]
_CMP1 = ["0=", "0<"]          # ( n -- flag )
_CMP2 = ["=", "<>", "<", ">"]  # ( n n -- flag )


class _Gen:
    def __init__(self, rng: random.Random, budget: int, rdepth: int = 0):
        self.rng = rng
        self.budget = budget        # remaining structural-step budget (bounds size)
        self.rdepth = rdepth        # recursion depth of structural nesting
        self.toks: list[str] = []

    def emit(self, t: str) -> None:
        self.toks.append(t)

    def lit(self) -> None:
        self.emit(str(self.rng.randint(0, 9)))

    # build tokens that take `depth` -> `depth` (stack-neutral), for loop/branch bodies
    def neutral(self, depth: int, steps: int) -> None:
        d = depth
        for _ in range(steps):
            if self.budget <= 0:
                break
            self.budget -= 1
            choices = [o for o in _LEAF if o[1] <= d]
            if not choices:           # depth 0 -> only a literal is possible
                self.lit(); d += 1
                continue
            tok, need, delta = self.rng.choice(choices)
            self.emit(tok)
            d = max(0, d + delta)
        # rebalance back to the original depth
        while d < depth:
            self.lit(); d += 1
        while d > depth:
            self.emit("drop"); d -= 1

    def maybe_if(self, depth: int) -> int:
        """emit `<cmp> if <bal> else <bal> then`; return new depth. needs depth>=1."""
        if self.rng.random() < 0.5 and depth >= 2:
            self.emit(self.rng.choice(_CMP2)); d = depth - 1   # n n -- flag
        else:
            self.emit(self.rng.choice(_CMP1)); d = depth        # n -- flag (top reused)
        d -= 1  # `if` consumes the flag
        m = self.rng.choice([-1, 0, 0, +1])  # both branches share the SAME net m
        if d + m < 0:
            m = 0
        self.emit("if")
        self._branch(d, m)
        self.emit("else")
        self._branch(d, m)
        self.emit("then")
        return d + m

    def _branch(self, depth: int, net: int) -> None:
        self.neutral(depth, self.rng.randint(0, 2))
        d = depth
        while net > 0:
            self.lit(); net -= 1
        while net < 0:
            if d <= 0:
                self.lit(); d += 1   # ensure a value to drop
            self.emit("drop"); net += 1

    def maybe_loop(self, depth: int) -> int:
        """`k 0 ?do <neutral, may use i> loop` — bounded, stack-neutral."""
        k = self.rng.randint(0, 3)
        self.emit(str(k)); self.emit("0"); self.emit("?do")
        # neutral body at depth `depth`; may push the loop index then drop it
        if self.rng.random() < 0.5:
            self.emit("i"); self.neutral(depth + 1, self.rng.randint(0, 1)); self.emit("drop")
        else:
            self.neutral(depth, self.rng.randint(0, 2))
        self.emit("loop")
        return depth  # net 0

    def maybe_rstack(self, depth: int) -> int:
        """balanced `>r <neutral> r>` (optionally `r@`)."""
        if depth < 1:
            return depth
        self.emit(">r"); d = depth - 1
        self.neutral(d, self.rng.randint(0, 1))
        if self.rng.random() < 0.5:
            self.emit("r@"); d += 1  # copy index back
        self.emit("r>"); d += 1
        return d

    def maybe_quot(self, depth: int) -> int:
        """`[: <body net q> ;] execute` — a simple net transform applied now."""
        q = self.rng.choice([0, 0, +1])
        self.emit("[:")
        if q == 0 and self.rng.random() < 0.5:
            self.emit(self.rng.choice(["1+", "negate", "1-"]))   # ( n -- n )
        elif q == +1:
            self.emit("dup")                                     # ( n -- n n )
        # q==0 empty quot is also valid
        self.emit(";]"); self.emit("execute")
        return depth + q  # execute applies the quot's net (q); needs >=1 input for 1+/dup

    def walk(self, depth: int, steps: int) -> int:
        d = depth
        for _ in range(steps):
            if self.budget <= 0:
                break
            self.budget -= 1
            kinds = ["leaf", "leaf", "leaf", "lit", "lit"]
            if d >= 1 and self.rdepth < 2:
                kinds += ["if", "loop", "rstack", "quot"]
            k = self.rng.choice(kinds)
            if k == "lit":
                self.lit(); d += 1
            elif k == "leaf":
                opts = [o for o in _LEAF if o[1] <= d]
                if not opts:
                    self.lit(); d += 1
                else:
                    tok, need, delta = self.rng.choice(opts)
                    self.emit(tok); d = max(0, d + delta)
            elif k == "if":
                sub = _Gen(self.rng, self.budget, self.rdepth + 1)
                sub.toks = self.toks
                d = sub.maybe_if(d); self.budget = sub.budget
            elif k == "loop":
                sub = _Gen(self.rng, self.budget, self.rdepth + 1)
                sub.toks = self.toks
                d = sub.maybe_loop(d); self.budget = sub.budget
            elif k == "rstack":
                d = _bridge(self, self.maybe_rstack, d)
            elif k == "quot":
                if d >= 1:
                    sub = _Gen(self.rng, self.budget, self.rdepth + 1)
                    sub.toks = self.toks
                    d = sub.maybe_quot(d); self.budget = sub.budget
        return d


def _bridge(g: "_Gen", fn, d: int) -> int:
    return fn(d)


def gen_program(rng: random.Random, idx: int) -> Program:
    n_in = rng.randint(0, 3)
    use_locals = rng.random() < 0.35 and n_in >= 1
    g = _Gen(rng, budget=rng.randint(4, 12))
    toks: list[str] = []
    g.toks = toks
    if use_locals:
        names = ["a", "b", "c"][:n_in]
        g.emit("{:"); [g.emit(nm) for nm in names]; g.emit(":}")
        depth = 0
        # reference some locals to seed the stack
        for _ in range(rng.randint(1, n_in + 1)):
            g.emit(rng.choice(names)); depth += 1
    else:
        depth = n_in
    true_out = g.walk(depth, rng.randint(2, 6))
    # declare out = true_out (certify) or perturb by +-1 (intended-reject)
    perturbed = rng.random() < 0.3
    n_out = true_out
    if perturbed:
        n_out = max(0, true_out + rng.choice([-1, +1]))
        perturbed = (n_out != true_out)
    body = " ".join(toks)
    return Program(f"G{idx}", n_in, n_out, body + " ", true_out, perturbed)


if __name__ == "__main__":
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--count", type=int, default=10)
    a = ap.parse_args()
    rng = random.Random(a.seed)
    for i in range(a.count):
        p = gen_program(rng, i)
        flag = " [perturbed]" if p.perturbed else ""
        print(f"{p.definition()}    \\ true_out={p.true_out}{flag}")
