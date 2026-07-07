---
title: Checked image writers
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.896981+02:00"
---

Rewrite src/os/{linux/elf.f,linux/sign.f,macos/macho.f,macos/sign2.f} under records + ptr-arith capability so BF-APPEND-CHECK-OFF (build-fixpoint.f:536) and BF-APPEND-IMAGE-TRUSTS (:555-560, 5 generated TRUST rows) are deleted - the image writers become ordinary checked source in stage2. Also converts aot-lib.f's open 0-set-check region (~237 ln ARM64 relocation core) into named TRUSTED: words at minimum (raw region -> named+tested boundaries). Effort M (~4d). Depends: ptr-arith + dict-record capabilities.

## Build-fixpoint half landed (2026-07-07, from head 671c15a7)

Finding: the image writers ALREADY check as written. Blocking certify
(BF-CERTIFY-GENERATED) has been statically verifying every definition in
elf.f/sign.f/macho.f/sign2.f for as long as it has been green - VERIFY:
SOURCE-BUF ignores the generated set-check window - so the window and the
five synthetic TRUST rows protected nothing. Deleted BF-APPEND-CHECK-OFF,
BF-APPEND-FRESH-CHECK-HOOK, BF-APPEND-IMAGE-TRUSTS (+ BF-APPEND-TRUST/
BF-APPEND-SQUOTE helpers) at both emit sites (stage2 + snap), and the
mirrored injection in tools/bootstrap.sh emit_src. The live stage compile
now checks the writers with the real hook: refresh converged (fixpoint OK)
first try. BFT pins the new shape (stage2 contains NO bare 0 set-check
line and none of the retired TRUST rows; image region has no window).
TRUSTED.md generated-trust exemption updated (set now empty).

No records/ptr-arith capability was needed: the writers' effects come from
their own checked definitions (IMG-M*/M-* from already-checked
image-bytes.f, phase tokens from roles.f, in-file SNAP-EXTRA-* TRUSTs).

REMAINING (this dot stays open): the aot-lib.f half - convert its
file-top 0 set-check region (~237 ln ARM64 relocation core) into named
TRUSTED: words at minimum; probe first whether most defs check as-is the
same way the writers did (hb-build maker source under certify).
