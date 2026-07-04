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
\ Engine-hash decision (section 7.4 engine field; cad-5 seam).
\ The engine field distinguishes schedules produced by different bin/hb engine
\ versions so a schedules.rows written by engine v1 is not replayed under engine v2.
\ Computing it as the content key of bin/hb via lib/content-key.f was evaluated for
\ cad-5 and DEFERRED; SK-ENGINE$ keeps the honest "engine-unbound" placeholder. Why:
\   1. No robust self-path. A --loaded script has no portable way to learn bin/hb's own
\      absolute path: "bin/hb" resolves only from the workspace-root cwd, and argv[0]
\      is caller-controlled (a bare PATH name need not resolve from a test cwd). A key
\      that silently degrades to a placeholder when the path fails would fragment the
\      store (one engine -> two keys) - a silent fallback, forbidden. A sometimes-real,
\      sometimes-placeholder key is worse than one honest placeholder.
\   2. Hot-path weight. lib/content-key.f drags SHA256 + memory + fs machinery into the
\      interactive MODEL:/TILE key-render path (maki/sched-key.f) for a field that only
\      matters to cross-process durability.
\   3. Bounded risk. schedules.rows lives under tmp/ (regenerable, per-workspace, never
\      committed), so a stale-engine replay is local and self-healing (STORE-RESET / a
\      tmp wipe clears it), not a shipped soundness hole.
\ Correct fix (needs a dot): a first-class engine-identity capability - a stable
\ self-path fact plus a content key computed once at load and cached - then SK-ENGINE$
\ returns that key and the store keys by real engine identity.
\
\ maki -> habu only.

require maki/sched-key.f
require maki/store.f

package MAKI
public

\ record a schedule selection in the hot table AND durably in schedules.rows
: SK-PUT-DURABLE ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}
   a u sel SK-PUT
   a u sel SCHED-PUT ;

\ rehydrate the in-memory table from schedules.rows (latest row per key wins)
: STORE-REPLAY-LOAD ( -- )
   [: SK-PUT ;] SCHED-LOAD ;

end-package
