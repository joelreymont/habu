---
title: Refuse a number-shaped definition name in the engine
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T11:16:27.832482+03:00"
---

Problem: 'bin/hb --load' accepts ': 42 ( -- n ) 1 ;' with exit 0, while tools/check.f refuses the same source with E-NUMERIC-DEFINITION (measured 2026-09-18 by the forth-card lane on 5a3d9f82); docs/forth.md Naming reads as if the engine refuses it, and a program that defines a number-shaped word shadows number parsing for every later token in the session. The engine already refuses a compile-keyword name (': I' is E-RESERVED-DEFINITION), so the number-shaped case is the missing arm of the same guard. Acceptance: the engine refuses a definition whose name parses as a number with the same code the checker gate uses (E-NUMERIC-DEFINITION) through the labeled compile-die tail, catchable inside evaluate; tools/check.f's verdict unchanged; a case beside the E-RESERVED-DEFINITION regression; docs/forth.md Naming states one rule with one owner. Files: src/habu/habu2.f (the definer name guard beside the keyword arm), test/, docs/forth.md. Verify: the fixture; three generations with cmp; test/run.f. Depends: none. Ownership: engine. Claim: unassigned.
