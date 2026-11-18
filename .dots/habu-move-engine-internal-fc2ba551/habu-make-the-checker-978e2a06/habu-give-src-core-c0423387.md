---
title: "Give src/core/include.f's global words a package"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:51:53.542071+01:00"
closed-at: "2026-11-18T17:45:43.777435+01:00"
---

Problem: src/core/include.f defines 105 words outside any package, so a program calls them: ' REQUIRE-SNAPSHOT drop and ' EVENT-COPY-PATH drop pass tools/check.f -- --verify-only, rc 0. Tools loaded after the build seals the engine call 20 of them (tools/source-discovery.f, tools/check-core.f, tools/check-verify-core.f, tools/lsp-workspace.f), and so do seven tests. Acceptance: every include.f word but the language words included, required, provided, script-required and ?TARGET sits in package LOADER, REQUIRE-REG or SOURCE-EVENT; a word only engine files call is E-UNDEFINED to a program; the tools call the public names; each test case calls a public name, reaches the loader end to end, or is deleted where no real input reaches it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-inc.
