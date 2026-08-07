---
title: Resolve a callee from the dictionary and the checker
status: active
priority: 2
issue-type: task
created-at: "2026-08-02T10:37:37.562216+02:00"
---

Claim: agent=nameres workspace=.jj-ws/habu-complete-the-chain-5aab8cee

src/compiler/native/migrate.f DEFINE-CALLING takes a callee's entry address and declared effect from the CALLER, exactly as DEFINE-DATA takes a data word's address. Both facts already exist in the running engine and neither is checked against it: the address is the callee's dictionary record (src/habu/xref.f XREF-FIND-WL-INDEX plus XREF-START, which src/compiler/native/publish.f already reads from checked code), and the effect is what the checker accepted for that name. A caller that states either wrongly compiles a routine that computes the wrong thing and nothing refuses it: proved by mutation on 2026-08-02, a body ': LIE-W ( n n -- n n ) LIE-DBL ;' whose callee really takes and leaves one value but is declared as taking and leaving two selects, allocates, passes the register-allocation validator, emits and publishes with no throw. The validator cannot see it because the selector builds BOTH the store/load runs and the two byte counts from the same stated arity, so the two derivations it holds against each other always agree. Fix: HIR-WORD:DECLARE-CALLABLE keeps its shape, and NMIGRATE resolves the spelling to a record and reads the entry and the arity off the engine, so a caller states a NAME and nothing else. Depends on habu-bind-checker-env-ed4f9f87 for the effect half; the address half needs nothing new.
