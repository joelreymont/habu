---
title: The judge report columns re-parse the signature
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T22:52:50.686221+02:00"
---

tools/judge/src.f still parses the corpus's ( ... ) stack comment itself for JUDGE-SRC:IN/OUT, which feed judge/pass.f's report columns. Since master b4329129 the compiler reads arity from the checker's certificate (NDICT:SPELL-ARITY); the judge's own parse is now a second reading of the same signature the compiler no longer trusts - the next instance of the caller-stated-authority class that a1c8067f/A2 closed. It only feeds artifact columns (46/46 agrees today), so it is drift waiting, not a live bug. Fix shape: derive the columns from the same reader, or from the judged word's own certificate, and delete the parse.
