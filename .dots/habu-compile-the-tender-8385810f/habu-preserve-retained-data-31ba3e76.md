---
title: Preserve retained DATA address provenance and literal owners in stripped images
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T15:25:49.033014+03:00"
---

Hazel reproduced on current numeric engine/current native stripped route: /tmp/hazel-c5/unmap-fail-subject.f calls MEM:UNMAP with misaligned4097,length4096 through a fixture-only typed boundary. Engine prints memory: unmap failed; stripped output /tmp/hazel-c5/unmap-fail/hb-aot-got builds0/exits71 but emits20 NUL bytes. A retained pre-window literal survives as an unclassified old DATA address and reads zero. Investigate EM-AOT-RELOC-DATA missing ADDRMAP publication (CODE/named passes restore their markers); preserve authoritative site provenance through seed and snapshot. Retain immutable NSTR pool/row ownership across WINDOW-OPEN and restore, and re-intern owned old rows into current captured span before emitting literal addresses. Refuse old mutable DATA without string-owner evidence. No magnitude scan/reclassification workaround. Hazel owns reproducer/design review; internal Astra checker_followup_review owns exact-site attribution/engine fix; Cedar owns NSTR/link integration. Acceptance: actual library failure message and rc, previous-window and restored-engine literal ownership, unrelated mutable DATA refusal, scalar non-relocation, native rebuild and full gate.
