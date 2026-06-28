---
title: Rebuild local habu-maki bin/hb for master source
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T11:19:26.602574+02:00"
---

After rebasing habu-maki onto master, the local bin/hb (gitignored derived artifact, mtime 06-27 22:23) is stale: it rejects master's lib/string.f (rc=70, checker mismatch). Both rebuild paths are blocked in THIS workspace: (1) the sibling /home/user/Work/habu/bin/hb loads master libs but is too old to run build-fixpoint (HIDE-DEFS-FROM undefined, rc=14); (2) the gforth bootstrap fails because the installed gforth lacks the {: :} locals syntax the probe needs (rc=69). Workaround in use: run maki/dev workloads with the master-compatible sibling bin/hb (it runs maki/array-test etc. green). PROPER FIX: obtain a master-built bin/hb (from the agent/host that built master) or a newer gforth, then refresh via 'bin/hb --load ... tools/build-fixpoint.f tools/build-fixpoint-main.f -- install'. Until then, the committed-gate-via-local-bin/hb path is unavailable here.
