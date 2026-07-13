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
\ into the table in file order, so the LATEST row for a key wins (append-only), and it
\ is capacity-guarded: SK-PUT throws E-SK-FULL past SK-TAB-CAP. LOAD merges (update in
\ place per key) rather than resetting - the caller SK-TAB-RESETs first for a clean
\ durable->memory rebuild.
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
variable SRP-READY                   \ nonzero once this process merged schedules.rows
public

: REPLAY-READY? ( -- bool )  SRP-READY @ 0<> ;
: REPLAY-RESET  ( -- )  0 SRP-READY ! ;

\ merge schedules.rows into the live table once per process. The latch is set
\ BEFORE the load: one attempt per session, so a throwing load (E-SK-FULL, IO)
\ surfaces loudly to its first caller instead of re-merging on every lookup.
\ No SK-TAB-RESET: rows already live in memory keep their entries; a same-key
\ file row updates in place, which is safe on every production path because
\ both production entries (TILE/TUNE lookup, SK-PUT-DURABLE write) ensure
\ BEFORE touching the table - see the load-before-first-write header note.
: REPLAY-ENSURE ( -- )
   REPLAY-READY? if exit then
   -1 SRP-READY !
   STORE-REPLAY-LOAD ;

\ record a schedule selection in the hot table AND durably in schedules.rows.
\ THE production write path: it rehydrates first (load-before-first-write), so
\ the fresh row lands after the durable merge and can never be shadowed by it.
: SK-PUT-DURABLE ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}
   REPLAY-ENSURE
   a u sel SK-PUT
   a u sel SCHED-PUT ;

;package
