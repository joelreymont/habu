---
title: Answer several pcs in one imgdump scan
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T14:59:03.612795+03:00"
---

Claim: alder, .jj-ws/alder-imgdump-pcs, base b4efad25. The PRN? early-exit
repair already makes the scan quick; retain that bound and measure the
remaining repeated-scan cost rather than relying on the old 63-second figure.

Implementation: --pc accepts one or more addresses and prepares the image
once. The real-engine fixture asks for + and evaluate in one child and checks
both complete output lines in order. Namespace slots remain raw public/private
WIDs; zero roles remain valid scan records and skip code-address validation.

Proof: the new fixture on the old tool refuses the batch with rc64 and prints
the wrong namespace WID ($100000011 instead of $11). The changed suite passes,
including zero namespace roles followed by a code record. A real hb-build
--repl snapshot passes batched canonical-PC queries and a complete dictionary
dump. A malformed later PC still returns rc64. The full hb-build-fixtures
reader row passes on the final tree; Astra review is clear.

Performance on the current engine: two separate real-engine queries take a
median 0.427s versus 0.206s batched (three runs each). The whole imgdump suite
was 1.00s before, 0.79s after the initial patch including its extra namespace
fixture; the former 63-second scan had already been fixed by PRN?'s early exit.
Hazel supplies the final integration/gate; this dot stays active until then.

Problem: tools/imgdump.f --pc runs FIND-DICT's longest-run scan over the whole image (about 63 s for a 5.8 MB engine on a loaded host) once per invocation and answers one pc, so tools/imgdump-test.f's two real-engine --pc cases (62fdea1b) cost about 128 s and make suite imgdump-compare the long pole of the stdlib gate. Also pre-existing: in a no-trailer image E-S adds XTBASE to a namespace record's slot 0 (a WID role), so a package row's start column is garbage under a header that says start. Acceptance: one --pc invocation accepts several pcs and prints one line per pc after a single scan; tools/imgdump-test.f queries + and evaluate through one child and its wall time roughly halves (measure before and after); the namespace row prints its two WID roles under honest column names or E-S stops rebasing namespace records (ENT? and FIND-DICT must keep working, so verify the scan on a real engine and a snapshot image). Files: tools/imgdump.f, tools/imgdump-test.f. Verify: bin/hb --load tools/imgdump-test.f with timing; --pc on a real engine and on an hb-build --repl snapshot. Depends: none. Ownership: tools/imgdump.f tools/imgdump-test.f. Claim: unassigned. Source: audit-imgdump worker notes 2026-09-16.
