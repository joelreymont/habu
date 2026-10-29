---
title: Own the unit-object site kinds in the build tool
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T00:32:45.687430+02:00"
---

Problem: tools/native-unit-object.f:1778,1788,1809-1810,2066,2072,2105-2114 name NEMIT:CALL, NEMIT:TAIL, NEMIT:ADDR-CODE and NEMIT:ADDR-DATA at compile time; the tool runs on the host engine and its require of src/compiler/native/emission.f (:29) is satisfied by the host's baked copy, so the constants are the host's, not the tree's. A host older than de4fceae has no NEMIT:ADDR-CODE: loading the tool is E-UNDEFINED at :1788, rc 70, so no engine older than de4fceae builds master (reproduced with carl-cstr-lib/.out/e/hb). Before de4fceae the lines named A64IR:ADDR-CODE with the same flaw. Acceptance: the persisted unit-object format owns CALL, TAIL, ADDR-DATA and ADDR-CODE in the tool (precedent: native-file-reference.f HOST-FIXED, HOST-REBIND); capture reads the window's four values by name once per window and translates, an unknown kind is BAD; read-back compares against the format's constants; the emission.f require goes; no compile-time NEMIT reference remains in tools/. Proof: the old host loads the tool rc 0 and builds the engine rc 0; the product built by the public hb is byte-identical before and after the change, warm equals cold; aot-payload-graph and the tool's tests green; check.f and shadow lint clean. Files: tools/native-unit-object.f, tools/native-unit-file.f. Verify: carl-dots/.out/carl-file/brief-nunit-kinds.md. Depends: none. Ownership: the two files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-nunit-kinds
