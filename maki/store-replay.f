\ maki/store-replay.f - durable backing for the in-memory schedule replay table.
\
\ The hot path is the bounded in-memory key->selection table in maki/sched-key.f
\ (SK-GET/SK-PUT); this bridge makes it durable against the CAD artifact store
\ (maki/store.f schedules.rows). One concern: the memory<->file bridge only. sched-key
\ stays pure in-memory; store stays pure storage; this file wires them.
\
\ SK-PUT-DURABLE records a selection in BOTH places: the in-memory table (so replay
\ stays a cheap lookup) and schedules.rows (so a fresh process can rehydrate). Memory
\ first - the hot table must succeed; the durable append is best-effort and its IO
\ errors propagate (never swallowed). STORE-REPLAY-LOAD replays schedules.rows back
\ into the table in file order, so the LATEST row for a key wins (append-only); the
\ table GROWS on demand (maki/sched-key.f), so a store with any legal number of
\ distinct keys rehydrates without a capacity wall. LOAD merges (update in place per
\ key) rather than resetting - the caller SK-TAB-RESETs first for a clean
\ durable->memory rebuild.
\
\ schedules.rows stays APPEND-ONLY, uncompacted (dot habu-maki-sk-table-59bb1d4d
\ considered compact-on-load). Append is the one crash-safe file mutation available
\ (lib/fs-mutate has no atomic replace; a compactor that truncates+rewrites on load
\ could tear the store under a crash or a concurrent process), latest-wins on load
\ already gives correct semantics over superseded rows, and the file is loudly
\ capped (E-STORE-FULL at maki/store.f STORE-READ-CAP) rather than silently
\ unbounded. Real compaction belongs to the V2 design database's atomic
\ content-addressed store (MODEL-CAD-V2-PLAN.md section 9.5), which replaces this
\ file wholesale.
\
\ Session latch (REPLAY-READY? / REPLAY-ENSURE): production callers never load the
\ file explicitly - they call REPLAY-ENSURE, which MERGES schedules.rows into the
\ live table exactly once per process (no SK-TAB-RESET), before the first replay
\ lookup (TILE/TUNE) AND before the first durable write (SK-PUT-DURABLE ensures
\ first). That is the load-before-first-write order: the durable state is merged in
\ before any fresh session row lands, so rehydration can never clobber a fresher row
\ written through the production path. A missing store is a clean no-op (fail-safe
\ miss, not a throw - STORE-READ-CLASS reads a missing file as empty). REPLAY-RESET
\ re-arms the latch; it is the test/session-boundary seam.
\
\ Engine-hash decision (section 7.4 engine field).
\ The engine field distinguishes schedules produced by different bin/hb engine builds
\ so a schedules.rows written by one engine is not replayed under another. It is now
\ the real content key of bin/hb, supplied by the first-class engine-identity
\ capability in lib/engine-id.f: the engine resolves its OWN executable path from the
\ kernel-provided process image (macOS apple[] executable_path - the source
\ _NSGetExecutablePath reads; Linux /proc/self/exe), not from a script guessing
\ "bin/hb" against the cwd or the caller-controlled argv[0], and SHA-256s that binary
\ once on first request, cached thereafter. SK-ENGINE$ returns that key, so the store
\ keys by real engine identity. The two properties that once forced the
\ "engine-unbound" placeholder now hold:
\   - Robust self-path: an engine-side fact from the process image, resolved once and
\     cached; ENGINE-KEY$ fails closed with a named throw if it cannot be resolved or
\     hashed, never a silent degrade to a placeholder (which would fragment the store,
\     one engine -> two keys).
\   - No hot-path weight: the SHA-256 is lazy + cached, run only when a durable key is
\     first rendered, so it never weighs on the interactive MODEL:/TILE key render.
\ schedules.rows still lives under tmp/ (regenerable, per-workspace, never committed),
\ so replay stays local and self-healing.
\
\ maki -> habu only.

require maki/sched-key.f
require maki/store.f

package MAKI
public

\ rehydrate the in-memory table from schedules.rows (latest row per key wins);
\ merges per key into the live table - SK-TAB-RESET first for a clean rebuild
: STORE-REPLAY-LOAD ( -- )
   [: SK-PUT ;] SCHED-LOAD ;

private
\ ---- recovery lifecycle: cold -> ready (fully loaded) | failed(error) ---------
\ The merge attempt has three explicit states. COLD is the initial / post-reset
\ state (not yet attempted). READY means the WHOLE file merged successfully.
\ FAILED means an attempt threw; SRP-ERR carries the original throw code. The old
\ code kept a single "ready" latch and set it BEFORE loading, so a load that threw
\ partway left the latch READY over a HALF-loaded table and returned success on
\ retry - a partial, failed recovery published as success. The lifecycle below is
\ transactional instead: READY is reached only after a fully successful load, a
\ throwing load transitions to FAILED (publishing no partial state - SCHED-LOAD
\ authenticates every row before applying any), and a FAILED store STAYS failed -
\ every retry re-raises the original error rather than silently succeeding, so a
\ corrupt durable store must be repaired (STORE-RESET; it is regenerable) before
\ any replay lookup or durable write proceeds.
0 constant RPS-COLD                  \ not yet attempted (initial / after REPLAY-RESET)
1 constant RPS-READY                 \ merged the whole file successfully
2 constant RPS-FAILED                \ a merge attempt threw; SRP-ERR holds its code
variable SRP-STATE                   \ RPS-COLD | RPS-READY | RPS-FAILED
variable SRP-ERR                     \ the original throw code while SRP-STATE = RPS-FAILED
public

: REPLAY-READY?  ( -- bool )  SRP-STATE @ RPS-READY  = ;
: REPLAY-FAILED? ( -- bool )  SRP-STATE @ RPS-FAILED = ;
: REPLAY-ERROR   ( -- n )     SRP-ERR @ ;
: REPLAY-RESET   ( -- )  RPS-COLD SRP-STATE !  0 SRP-ERR ! ;

\ merge schedules.rows into the live table once per process. No SK-TAB-RESET: rows
\ already live in memory keep their entries; a same-key file row updates in place,
\ which is safe on every production path because both production entries (TILE/TUNE
\ lookup, SK-PUT-DURABLE write) ensure BEFORE touching the table - see the
\ load-before-first-write header note. The load is wrapped so its outcome, not just
\ its first attempt, drives the state: success advances to READY; a throw records
\ the error, transitions to FAILED, and re-raises; a later call over a FAILED store
\ re-raises the recorded error without a second load attempt.
: REPLAY-ENSURE ( -- )
   REPLAY-READY?  if exit then
   REPLAY-FAILED? if SRP-ERR @ throw then
   [: STORE-REPLAY-LOAD ;] catch {: rc:n :}
   rc 0<> if  rc SRP-ERR !  RPS-FAILED SRP-STATE !  rc throw  then
   RPS-READY SRP-STATE ! ;

\ record a schedule selection in the hot table AND durably in schedules.rows.
\ THE production write path: it rehydrates first (load-before-first-write), so
\ the fresh row lands after the durable merge and can never be shadowed by it.
\ SEALED WRITER (R7 store rehydrate, dot habu-v2-typestate-store-57afdc0a): SK-PUT-DURABLE
\ leaves the public MAKI surface so nothing outside the promote/store owner (package MAKI)
\ can plant a DURABLE schedule row - the direct SCHED-PUT plant was already sealed by the
\ promotion sub-dot (maki/store.f, census probe 3, MODEL-CAD-V2-PLAN.md:1576-1583); this
\ closes the durable side. It stays reachable to the package-MAKI promote path (maki/cad.f
\ PROMOTE-EVIDENCE) and the store suites (all package-MAKI reopens); a cross-package /
\ qualified MAKI:SK-PUT-DURABLE no longer resolves (maki/store-rehydrate-test.f).
private
: SK-PUT-DURABLE ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}
   REPLAY-ENSURE
   a u sel SK-PUT
   a u sel SCHED-PUT ;

;package
