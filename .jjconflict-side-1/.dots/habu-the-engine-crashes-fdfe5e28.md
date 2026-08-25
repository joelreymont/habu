---
title: The engine crashes on a brace-named local
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T02:31:00.357547+02:00"
---

Found by the ij-locals lane (2026-08-13): ': OLOC ( n -- n ) {: {::n :} {: {: + ;' crashes bin/hb - SIGBUS rc 134 with the habu-crash register dump. Pathological source (the name stays unspellable under the merged local-first rule either way), but a crash on any input is an engine robustness bug: the locals parser should refuse the spelling by name. Reproduce, diagnose with the debugger per the standing rule, fix fail-closed. Files: src/habu (locals parsing). Depends: none.
