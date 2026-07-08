---
title: "Repair packet: machine-insertable producer for add_producer"
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T20:49:09.828145+02:00"
---

Packet-capability gap found by maki/eval-repair-mech.f: an add_producer packet (tools/repair-packet-core.f RP-PACKET over src/core/render.f DIAG-JSON) carries only the flagged consumer token (token/byte_start/byte_end, e.g. 'drop'), expected/actual stack rows, and prose suggestion text. It has NO machine-insertable producer token and NO insertion position, so the mechanical repairer must report MECH-UNREPAIRABLE for this class (maki/eval-repair-mech-test.f 'mech add' cases prove it). Closing the gap needs checker/render support: derive a suggested producer token (or 'delete the consumer' directive) plus an insertion byte position in the diagnostic, thread it through RP-PACKET as a new packet field, prove it in tools/repair-packet-test.f goldens, then teach EVAL:MECH-APPLY the insert edit and flip the mech add fixture to green.
