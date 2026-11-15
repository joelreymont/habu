---
title: Test an unclosed declaration in the REPL
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T09:25:16.175129+01:00"
---

Problem: the REPL's end of input (src/habu/interpreter.f REPL-END, DECL-SOURCE-END) and a failed REPL line (REPL-LINE, SOURCE-ROOT:TEXT-UNWOUND) reject or drop an open BEGIN-STRUCTURE or FUNCTION: group, but no test drives either. Acceptance: a test feeds the REPL an unclosed BEGIN-STRUCTURE and sees one E-UNCLOSED record. Depends: none. Claim: unassigned.
