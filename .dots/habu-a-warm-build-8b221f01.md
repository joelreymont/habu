---
title: A warm build may miss a type named only inside angle brackets
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T19:16:14.845518+02:00"
---

Problem (read, not run; type-states design 2026-11-11): the linker's spelled-name scan cuts a name only at white space, ':' and '"' (src/habu/linker.f CUT? :1029), so a part that writes 'box<turn>' spells no 'turn'; whether the declaration of turn is tracked for that part another way was not found. 'TYPED-BUFFER BT box<turn>' bakes the width as a number, so a missed dependency is a warm build that differs from a cold one after turn's layout changes. Acceptance: a three-file reproducer shows warm equals cold after the edit, with the edge that carries the dependency named, or the miss is root-caused and fixed with a test. Files: src/habu/linker.f. Verify: the reproducer cold and warm. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-tydep (hunt, brief .out/briefs/hunt-type-argument-dependency.md).
