---
title: Publish the AAPCS register list from the target contract
status: open
priority: 3
issue-type: task
created-at: "2026-07-31T20:35:15.686445+02:00"
---

The routine contract (src/compiler/a64-effect.f) now says what a calling convention IS - an ordered register list per argument and per returned value - but not which one is in force. Every caller that wants the C ABI therefore spells x0, x1, x2 out for itself: test/compiler/native-chain-fixture.f builds its leaf contracts that way and tools/codegen-compare-chain.f measures against them. That is one fact in two places already and it will be in more. AAPCS64 passes the first eight integer arguments in x0..x7 and returns an integer in x0, and which ABI is in force is exactly what CBIND/CTARGET already records (CTARGET-ABI:AAPCS64-DARWIN). Move the list there: a reader that answers the argument register for position i and the result register for position j of the bound ABI, refusing a position the ABI passes on the stack instead. Then a caller builds a contract from the ABI rather than from three literals, and a second ABI is a row rather than an edit. Owner: CTARGET/CBIND, consumed by A64EFF's constructors and the chain fixtures.
