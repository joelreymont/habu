---
title: "Publish an exported definer's does> clause"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T17:17:49.760468+02:00"
---

Problem: EXPORT of a private definer publishes the definer (MK) but not its does> clause record (MK;does, src/habu/habu2.f DOES-REC), so AOT capture cannot name the clause a window word made through the public alias branches into: `7 ACP:MK SEVEN` is refused rc 74 (review 161 probe ~/.cache/tmp/kestrel-r4-rev161/probe/e2.f). Acceptance: a window word made by an EXPORTed private definer captures and runs its clause (rc 0, same output loaded and captured); the fix publishes the clause with its definer or resolves the clause through the parent's alias, decided with evidence; a case that fails first in the AOT band suite; the acap-redef rule (undefine retires the clause with its definer, review 178) still holds. Undefining the private original while its public alias lives retires only the original's pair; the alias still names the definer and its clause, so a window word made through the alias still captures and runs (today it is refused by scope, review 178 item 3(2)); a case pins it. Files: src/habu/habu2.f (DOES-REC, EXPORT), src/habu/aot-capture.f, test/aot-prelude-band-suite.f. Verify: the new case rc 0; test/aot-prelude-band-suite.f rc 0; g1 == g2; two-generation build.
