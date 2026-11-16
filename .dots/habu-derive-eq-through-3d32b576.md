---
title: Derive eq through str, bool and option fields
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.670595+01:00"
---

Problem: STRUCTURE opts DERIVE eq FIELD root str FIELD key option<str> is refused 'field family must also derive eq', and an ENUM variant with a bool field 'payload type has no derived equality' (bob's /tmp/bob-habu-derive-eq.md); programs compare by hand or bend the type. Acceptance: DERIVE eq accepts str (byte equality), bool and option<T> where T derives eq, on STRUCTURE and ENUM payloads; a test compares equal and unequal values of each. Depends: none. Claim: unassigned.
