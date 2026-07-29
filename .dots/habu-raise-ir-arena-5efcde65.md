---
title: Raise IR arena registry capacity decision
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:34:39.865881+02:00"
---

Full context: IR-ARENA holds 64 registry slots (src/compiler/ir/arena.f). A module currently owns 15 arenas (IR-BUILD), so at most 4 modules - builders or frozen - can be live at once; the freeze verifier's derived predecessor/successor tables add more arenas per module (17-19 following the established pool/rows convention), dropping the ceiling to 3. The limit is a named refusal (E-IR-BUILD-SLOTS / arena registry exhausted), not silent, and no current consumer needs more than 3 live modules; this dot records the decision point. When a dialect or pipeline lane needs more concurrent live modules, raise the IR-ARENA slot count as IR-ARENA's own capacity decision (one constant plus its registry sizing and tests) instead of compressing per-module tables into shared arenas, which would break the one-authority-per-table discipline.
