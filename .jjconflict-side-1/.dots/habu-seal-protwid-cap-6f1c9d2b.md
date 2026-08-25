---
title: "Batch session: ~16 public ADT families trip protected-WID guard (silent exit 84)"
status: open
priority: 2
issue-type: bug
created-at: "2026-07-10T09:18:23.000000+00:00"
---

Found while integrating TFAM 16 (layout policies) fixtures into
`test/type-decl-suite.f` (dot `habu-tfam-16-layout-a764d28c`). In a BATCH session
(piped stdin / `--load`), after ~16 PUBLIC ADT families have been declared, the
17th public family's constructor generation trips the protected-WID seal guard
and the process exits **84** with NO diagnostic — even in a fresh engine with no
DIAG buffer active.

Reproducer (fresh engine, repo root):

```
S=/tmp/many.f; : > "$S"
for i in $(seq 1 20); do
  printf 'SUMTYPE sm%s 1 VARIANT foo a ;VARIANT VARIANT bar a ;VARIANT ;SUMTYPE\n." got%s " cr\n' "$i" "$i" >> "$S"
done
bin/hb < "$S"    # prints got1..got16, then exits 84 with no message on the 17th
```

Exit 84 is the protected-WID guard reject (cf. dot
`habu-aot-protected-wid-08716547`, "forge rc 84"). Two distinct defects here:

1. **Silent guard reject.** The overflow/guard path emits no diagnostic on any
   channel (reproduced in a fresh engine, no DIAG-BUFFER set), so the failure is
   opaque — a public ADT declaration just kills the process with a bare exit 84.
   The guard should emit a named diagnostic ("protected-WID ..." with the WID and
   the offending word) before exiting.
2. **~16 public-family ceiling in batch mode.** A batch program (the primary
   LLM-facing path) can declare only ~16 public ADT families per session before
   the guard fires. This is very likely a downstream symptom of
   `habu-aot-protected-wid-08716547`: in batch mode `EM-AOT-REGISTER-PROT-WIDS`
   runs too late (LEXIT), so `WIDN`/`PROT-WID-*` are not restored before the
   interpret loop, and public-constructor WID allocation walks into the
   AOT-baked protected-WID region — root-cause and fix belong to that dot. Verify
   whether that fix also lifts this ceiling; if not, the pwid registry capacity
   needs raising (like the earlier DICT-CAP raise) since real LLM/maki sessions
   will declare many more than 16 public ADT families.

Impact on TFAM 16: none to the POLICY feature — the POLICY parse/validate is
visibility-independent and fully proven. The suite's policy-parse fixtures are
declared PRIVATE (package-wrapped) so they exercise `TDECL-POLICY` on
sum/enum/product without publishing constructors, sidestepping this ceiling. This
dot tracks the seal-subsystem defect for the seal/AOT owner. Engine territory:
seal/protected-WID machinery + `src/habu/habu2.f` AOT restore.
