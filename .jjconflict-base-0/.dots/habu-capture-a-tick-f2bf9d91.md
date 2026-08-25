---
title: "Capture a tick's target into the reconstructed body"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T17:10:50.014103+02:00"
---

src/habu/habu2.f C-BTICK consumes the token after ['] with no body capture, so the definition text the engine hands the checker ends at the ['] and no source tape can ever hold the ticked word. Measured 2026-08-10: ': NF-TICKED ( -- n ) [`] NF-SW-IMPL ;' records two rows and the kept text is 'NF-TICKED ( -- n ) [`] '. The checker half is already in place - IS-TARGET-TOK? reports every token a keyword consumes (commit 'checker: report the token a keyword consumes') - so the remaining work is the engine's body capture. Until it lands the native chain cannot model ['] at all (census: 1 refusal over src+lib). Pinned by test/compiler/native-feed.f TICK-CASE, which is the case that moves when this lands.
