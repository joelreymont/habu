---
title: "Repair packet: typed replacement token for fix_type"
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T20:49:09.833981+02:00"
---

Packet-capability gap found by maki/eval-repair-mech.f: a fix_type packet (tools/repair-packet-core.f RP-PACKET over src/core/render.f DIAG-JSON) carries the offending token (e.g. '0=' with expected 'i64 ' actual 'bool '), stack rows, and prose suggestion, but NO typed replacement token, so the mechanical repairer must report MECH-UNREPAIRABLE for this class (maki/eval-repair-mech-test.f 'mech type' cases prove it). Closing the gap needs the checker/render side to propose a concrete replacement token (or an explicit 'no mechanical alternative' marker) in the diagnostic, a new RP-PACKET field proven by tools/repair-packet-test.f goldens, then EVAL:MECH-APPLY replace-span support and a green fix_type fixture in maki/eval-repair-mech-test.f.
