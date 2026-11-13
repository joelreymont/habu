---
title: "Census: every cell a capture hook clears, plain or scratch"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:26:39.683580+02:00"
closed-at: "2026-11-10T12:44:09.804750+02:00"
close-reason: census recorded in the dot; seven fix dots filed, waiting on design habu-design-no-file-298feb05
---

Problem: a program build scrubs process state out of what it writes by running hooks (IMAGE-LIFECYCLE:REGISTER, run by PREPARE from app-end.f and end.f). A hook exists only in the process whose top level registered it, while a file object carries the plain cells that hook would clear, so a build that links such a file from its object writes the builder's state into the program, or throws. The design Joel pre-approved on 2026-11-09 settles the rule: each owner keeps its cells free of process state as scratch cells, linked as zero. Fixed so far: the hook count and five flags (c1efdd1a). 27 REGISTER call sites in 23 files remain unchecked (rg IMAGE-LIFECYCLE:REGISTER under src and tools); build-out.f and engine-id.f are habu-build-out-and-322dc2f5, task.f is habu-task-cells-a-5a896f0c. Acceptance: for every call site, the hook, each cell it writes, whether that cell is plain, scratch, a PTR cell, a TASK:+USER slot or a DYNAMIC-BUFFER cell, and whether a file's top level can reach the registration; then one fix dot per group of files whose cells are plain. Files: read-only. Verify: the table in the report, each row a path:line. Depends: none. Ownership: none (read-only). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-hooks (read-only census).

Result (2026-11-10, by reading; kinds inferred, none reproduced). Of the REGISTER sites outside image-lifecycle.f, build-out.f, engine-id.f and task.f, 15 clean at least one plain cell and 3 are clean. Carried, one fix dot per group: native handles habu-native-handle-owners-ae05a6e8 (f64-text.f:78, zip-ffi.f:87, font.f:168, net/curl.f:529, signal.f:304); record chains habu-aio-and-curl-4089d8c2 (aio.f:505, net/curl.f:926); Postgres habu-postgres-owners-carry-4a97027f (pg.f:784, pg-rows.f:226); process rows habu-process-argv-and-8a7ca17d (process-argv.f:41, process-env.f:55); habu-genio-slot-state-6148c9db (genio-core.f:310); habu-lexer-tables-and-7cd295ec (source-lex.f:114, compiler/ir/context.f:520). Clean, their state in user slots: genio.f:38, log.f:70, pg.f:941. Persistent registrations, zip-state.f:50's FORK-CHILD hook and the unchecked typed cells: habu-persistent-registrations-and-9e225466. The fix dots wait for design habu-design-no-file-298feb05; each dot lists its cells as path:line.
