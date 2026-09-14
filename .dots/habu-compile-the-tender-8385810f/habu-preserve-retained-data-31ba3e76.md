---
title: Preserve retained DATA address provenance and literal owners in stripped images
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T15:25:49.033014+03:00"
---

Hazel reproduced on current numeric engine/current native stripped route: /tmp/hazel-c5/unmap-fail-subject.f calls MEM:UNMAP with misaligned4097,length4096 through a fixture-only typed boundary. Engine prints memory: unmap failed; stripped output /tmp/hazel-c5/unmap-fail/hb-aot-got builds0/exits71 but emits20 NUL bytes. A retained pre-window literal survives as an unclassified old DATA address and reads zero. Investigate EM-AOT-RELOC-DATA missing ADDRMAP publication (CODE/named passes restore their markers); preserve authoritative site provenance through seed and snapshot. Retain immutable NSTR pool/row ownership across WINDOW-OPEN and restore, and re-intern owned old rows into current captured span before emitting literal addresses. Refuse old mutable DATA without string-owner evidence. No magnitude scan/reclassification workaround. Hazel owns reproducer/design review; internal Astra checker_followup_review owns exact-site attribution/engine fix; Cedar owns NSTR/link integration. Acceptance: actual library failure message and rc, previous-window and restored-engine literal ownership, unrelated mutable DATA refusal, scalar non-relocation, native rebuild and full gate.

2026-09-14: seed map publication committed7bc7d386; two native generations are
byte-identical SHA515efca8 and the reproducer then refuses74 at link. Persistent
ownership test engine SHA4ccbc973 built from the older native host and passes
retained-host NTRAP literal ownership. Cedar and Hazel independently built/run
the actual MEM:UNMAP subject after linker re-interning: build0, run71, stdoutempty,
stderr exactly20 bytes `memory: unmap failed`. The full hb-build regression and
mutable pre-window variable refusal74/no-image pass, as do quotation/scalar and
address validation controls. Guarded importer source is integrated atde322418
after Hazel and separate Astra review. Native-string passes on guarded engine
SHAc3ddf790; snapshot-writer restored ownership passes. Combined integration
rebuild/full gate remain before closing this dot.
