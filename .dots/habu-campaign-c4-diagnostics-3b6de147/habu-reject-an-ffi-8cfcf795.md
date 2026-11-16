---
title: Reject an FFI group open across an include in the verifier
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T09:25:16.153230+01:00"
---

Problem: a FUNCTION: group opened in one file and a FUNCTION: or BEGIN-STRUCTURE in a file it includes before ;FUNCTION (/tmp/jerry-h8c/a.f, a2.f): the load places the rejection at the `<input>` script-required line; tools/check.f places the nested FUNCTION: case at the inner opener labelled "opened here"; --verify-only passes the BEGIN-STRUCTURE case rc 0. A declaration spans a load, and no reader can place a token of the including file once the included one runs. Rule: no declaration spans a load. Acceptance: a load word called while a declaration is open is E-UNCLOSED at the declaration's opener, before it loads anything; the three readers print one and the same record for a.f and a2.f, rc 70; docs/forth.md and the card state the rule. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-h8c.
