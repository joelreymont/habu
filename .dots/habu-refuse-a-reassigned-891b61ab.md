---
title: Refuse a reassigned checker owner offset instead of running its xt
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T09:35:45.360321+02:00"
---

CHECKER-OWNER:UNIT-FACTS (src/compiler/native/checker-owner.f) guards only an owner record too short for the cell. A retained host whose UNIT-FACTS offset now names another field (rebase moved UNIT-FACTS-OFF $558 -> $578; $558 became NATIVE-DOES-REPORT-OFF) runs that field's xt: RUN-BEFORE FACTS ran NATIVE-DOES-REPORT on an empty stack and failed as underdepth at env-base.f:4 instead of answering false. Log /Users/joel/.cache/tmp/carl-hostd.eMz6/t5/log. Found by carl host lane (workspace carl-host).
