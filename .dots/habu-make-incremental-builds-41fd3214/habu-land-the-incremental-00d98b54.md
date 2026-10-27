---
title: Land the incremental-build stack on master with the FFI relocation mask
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T23:26:30.725720+02:00"
closed-at: "2026-10-27T05:44:04.215742+02:00"
close-reason: "landed on master 9f905e31: full suite 503/503 (477 s), two-generation gen2..5 byte-equal, landing review ACCEPT"
---

Lane carl-rebase (.jj-ws.noindex/carl-rebase), brief ~/.cache/tmp/carl-file/brief-reloc.md. Stack duplicated onto master@origin 6981fddd (head tomsnnno). Root conflict yyrvkswslmql src/lib/ffi-abi.f DECLARE-ROW: master's reloc mask vs stack FN-INDEX/OBS-FN. File-object FFI-FN rows must carry reloc; struct-by-value frame fixture. Builder ~/.cache/tmp/carl-mb.vTnd.
