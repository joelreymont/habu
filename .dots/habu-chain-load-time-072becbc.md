---
title: Chain load-time installers need boot-run rows
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T06:38:19.805553+02:00"
---

Final milestone blocker, found by bake-chain-12 (2026-08-16) - and it fails LOUDLY by name now: the merged engine exits 76 'checker: no source-tape observer to arm' (checker.f:11154). The chain has three top-level installers; only DKEEP-HOOK is on the boot-run list. feed.f:216 INSTALL (CHECKER-TAPE:INSTALL) and clobber.f:193 WATCH-INSTALL (CODE-RECLAIM:WATCH) run at load time inside the window and never run in a seeded engine; both write PREFIX state so no captured cell carries them. Class bounded at exactly these two (bake-chain-12 surveyed every top-level effect in src/compiler/native/). Fix shape already ruled (e98b03d4 MERGE CHANNEL RULINGS 2, the DKEEP-HOOK precedent): move each to public, tools/aot-chain-capture.f declares them on the boot-run list with its existing refusal discipline. Acceptance: the milestone transcript green - tools/aot-chain-bake.f engine, echo s" : FOO ( n -- n ) 1 + ;" 1 1 8 NMIGRATE:DEFINE 7 FOO . prints 8 exit 0, in the registered suite; a mutation dropping either row reds by name.
