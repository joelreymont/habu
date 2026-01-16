---
title: Add numeric tower test matrix
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:08.435529+02:00"
---

Files: tests/
Add comprehensive tests (use ohsnap):
- All type pairs × all ops: fixnum/bignum/rational/float/complex × +/-/*/÷
- Contagion: (+ fixnum float) → float
- Bignum edges: (- 0 big), (- big 0), (- -big -big)
- Rational: (/ 4 6) → 2/3, (+ 1/2 1/3) → 5/6
- Complex: (+ #C(1 2) #C(3 4)) → #C(4 6), (* #C(0 1) #C(0 1)) → #C(-1 0)
Dependencies: habu-implement-numeric-contagion-11078a56
Verification: zig build test passes, all tower correct
