---
title: Keep the Mac awake while jobs run
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:29:11.611272+02:00"
---

Maki design r2 (/Users/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/design-chatgpt-r2.md §4.5, §5 item 4, lane H4): a new package (POWER:KEEP-AWAKE ( ptr u8 n -- POWER:awake ) / POWER:ALLOW-SLEEP ( POWER:awake -- ), src/lib/power.f): on macOS IOPMAssertionCreateWithName(kIOPMAssertionTypePreventUserIdleSystemSleep, kIOPMAssertionLevelOn, name, &id) and IOPMAssertionRelease through FUNCTION: over IOKit and CoreFoundation (CFStringCreateWithCString), as aio-macos.f:75-81 binds libc; Linux refuses until a backend exists (pg.f -9258 E-PLATFORM pattern). Test: hold, then pmset -g assertions lists the process's assertion; release removes it. Caller: Maki JOBS (Joel's decision). Joel chose the names (2026-10-31, LESSONS.md). Ownership: carl. Claim: unassigned.
