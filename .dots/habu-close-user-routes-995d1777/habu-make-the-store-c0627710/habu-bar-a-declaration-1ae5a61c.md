---
title: Bar a declaration word under every spelling
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:04:25.516060+01:00"
---

Problem: a package cannot define a global word, so each global declaration implemented in a package is a global forwarder to a public twin, and UNSAFE-TOK? (src/core/checker.f:15920) bars a checked body's call by token text: master's engine accepts ': foo ( -- ) ENUM-DECL:ED-RUN ;', and so for DYNAMIC-STORAGE:DECLARE, ARENA-DECL:RUN, TYPE-DECL:TDECL-SUMTYPE and the LBUF definers. A colon-ending declaration (generates:) has no qualified spelling at all, so it cannot be published from a package. Design (Fable, /tmp/bar-design/ probes): a third section word, `global` (the user, 2026-11-15: "fine, add global section"), defines into the global wordlist from inside a package; each declaration is one global word whose implementation stays private; the twins, forwarders and workspace.f's undefine lines go; checker registrar names leave UNSAFE-TOK?'s list as each goes private. Acceptance: each package spelling of a declaration in a checked body is E-UNDEFINED; each global spelling stays E-UNSAFE; every declaration works at top level and under tools/check.f; no new TRUSTED:. Depends: none. Claim: agent=worker workspaces=.jj-ws.noindex/carl-global (section word), carl-gfam, carl-gsum, carl-gstore (declarations).
