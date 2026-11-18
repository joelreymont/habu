---
title: "Give src/core/include.f's global words a package"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T16:51:53.542071+01:00"
---

Problem: src/core/include.f defines 103 words outside any package, so a program calls them: ' REQUIRE-SNAPSHOT drop and ' EVENT-COPY-PATH drop pass tools/check.f -- --verify-only, rc 0. Files loaded after the build seals the engine call some of them by name: tools/source-discovery.f :1228 :1234 and test/source-root-alias-child.f :48 :51 (REQUIRE-SNAPSHOT, REQUIRE-RESTORE). Acceptance: every include.f word sits in a package; a word only engine files call is E-UNDEFINED to a program; a word a tool calls after the seal is a public word of that package, under a name Joel picked. Depends: none. Claim: unassigned.
