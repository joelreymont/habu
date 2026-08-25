---
title: Type process fork role
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T21:08:26.879632+02:00"
---

lib/process.f:65-168 already owns fork tracing in package PROCESS-TRACE, but models its closed role {direct,reaper} with raw constants, a generic variable, and a fallthrough decoder. ROLE$ (:86-88) treats every non-reaper integer as direct; FORKED/REAPER/RESET write bare codes (:143-167). The state is not a wire/ABI value and the shipped ENUM/TYPED-VARIABLE features apply directly. Declare a private role ENUM with direct ordinal zero, store it in TYPED-VARIABLE so the zero image keeps the current default, and render it with exhaustive MATCH. Delete ROLE-DIRECT/ROLE-REAPER and the generic variable; all transitions must carry the role type. Preserve hook event strings, one-shot reaper reset semantics, cleanup/error behavior, and fork tracing exactly. Add checked negatives showing n/foreign-enum writes reject, transition tests covering parent/child/failure reset paths, and before/after CODELEN/JIT/DATA measurement; require no growth. Files: lib/process.f and process trace/fork tests. Depends: none. Ownership: trace-role typing only; outcome STRUCTURE/ENUM migration remains habu-libs-migrate-process-6bfe40be.
