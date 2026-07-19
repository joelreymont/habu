\ maki/store-frame-test.f - durable-store integrity: authenticated record framing
\ plus transactional recovery states (dot habu-authenticated-framing-for-580f8f7f,
\ review finding 5).
\
\ Two data-integrity holes are covered here:
\   (1) maki/store.f treated end-of-file as a record boundary, so a torn record tail
\       (a crash truncated the appended tail, losing its terminating newline) was
\       silently replayed as a committed record. The trailing newline is now the
\       authenticated commit marker: a class file whose last byte is not a newline is
\       a NAMED reject (E-STORE-TORN) on every read - the query scan AND the replay
\       load - never scanned as committed.
\   (2) maki/store-replay.f set the "ready" latch BEFORE loading, so a load that threw
\       partway left the latch ready over a half-loaded table and returned success on
\       retry. Recovery is now an explicit cold | ready | failed(error) lifecycle:
\       READY only after a fully successful load; a throwing load transitions to
\       FAILED (carrying the original error), publishes NO partial rows (SCHED-LOAD
\       authenticates every row before applying any), and STAYS failed on retry.
\
\ Fixtures are written as RAW bytes (the validated writers can never emit a torn or
\ malformed row), so the tests exercise exactly the corruption the readers must reject.
\ Writes only under the store root; STORE-RESET + SK-TAB-RESET + REPLAY-RESET leave no
\ rows, table entries, or warm latch behind.

require lib/test.f
require lib/string.f
require lib/fs.f
require maki/store-replay.f

package MAKI

\ ---- raw-byte fixture writers (bypass the validated writers on purpose) -------
create FX-NL 1 allot  STORE-NL FX-NL c!            \ a lone newline byte
: FX-PATH ( -- ptr u8 n )  CLS-SCHED STORE-CLASS-PATH ;
: FX+     ( ptr u8 n -- ) {: ca:ptr cu:n :}  FX-PATH ca cu APPEND-FILE ;
: FX-NL+  ( -- )  FX-NL 1 FX+ ;
: FX-CLEAN ( -- )  STORE-RESET STORE-ENSURE ;      \ fresh, empty store tree on disk

: FX-TORN1 ( -- )                                  \ one unterminated record: "k1|7"
   FX-CLEAN  s" k1|7" FX+ ;
: FX-TORN2 ( -- )                                  \ full record then a torn tail: "good|7<LF>bad"
   FX-CLEAN  s" good|7" FX+  FX-NL+  s" bad" FX+ ;
: FX-CORRUPT ( -- )                                \ framed, but a malformed MIDDLE row
   FX-CLEAN  s" k1|7" FX+  FX-NL+  s" BADROW" FX+  FX-NL+  s" k3|9" FX+  FX-NL+ ;
: FX-GOOD ( -- )                                   \ two well-framed records
   FX-CLEAN  s" k1|7" FX+  FX-NL+  s" k2|3" FX+  FX-NL+ ;

: FRESH ( -- )  SK-TAB-RESET  REPLAY-RESET ;       \ cold table + cold recovery latch

\ ---- fail-closed probes (for TTHROWS) ---------------------------------------
: TRY-TORN-GET ( -- )  s" k1" SCHED-GET 2drop ;    \ a query over a torn store must reject
: TRY-ENSURE   ( -- )  REPLAY-ENSURE ;             \ a replay merge over a corrupt store must reject

T-RESET

\ ==== (1) framing: a torn tail is a NAMED reject, never replayed as committed ====
\ query path: the torn single record "k1|7" (no trailing LF) rejects instead of
\ returning 7 as a committed selection (the original symptom 1).
FX-TORN1
' TRY-TORN-GET E-STORE-TORN TTHROWS

\ replay path: "good|7<LF>bad" rejects on the framing check before any row lands,
\ transitions to FAILED carrying the torn code, and leaves the table empty.
FX-TORN2  FRESH
' TRY-ENSURE E-STORE-TORN TTHROWS
REPLAY-FAILED?          TTRUE
REPLAY-READY?           TFALSE
REPLAY-ERROR E-STORE-TORN T=
SK-TAB-COUNT 0 T=                        \ no partial rows published
' TRY-ENSURE E-STORE-TORN TTHROWS        \ retry stays failed, re-raises the same code
SK-TAB-COUNT 0 T=

\ ==== (2) transactional recovery: a mid-file corrupt row fails closed ==========
\ "k1|7<LF>BADROW<LF>k3|9<LF>" is fully framed (ends in LF) but BADROW has no pipe.
\ The validate pass rejects BADROW BEFORE the apply pass runs, so the preceding
\ good row k1 is NEVER applied - the failed load publishes no partial state.
FX-CORRUPT  FRESH
' TRY-ENSURE E-STORE-ROW TTHROWS
REPLAY-FAILED?          TTRUE
REPLAY-READY?           TFALSE
REPLAY-ERROR E-STORE-ROW T=
SK-TAB-COUNT 0 T=                        \ k1 not applied: no partial records visible
s" k1" SK-GET nip TFALSE                 \ ...and not individually visible either
' TRY-ENSURE E-STORE-ROW TTHROWS         \ retry stays failed (no silent success)
SK-TAB-COUNT 0 T=                        \ still no partial rows after the retry

\ ==== (3) round-trip of a good store (framing accepts well-framed records) ======
\ replay path: both records rehydrate into the table.
FX-GOOD  FRESH
REPLAY-ENSURE
REPLAY-READY?  TTRUE
REPLAY-FAILED? TFALSE
SK-TAB-COUNT 2 T=
s" k1" SK-GET TTRUE 7 T=
s" k2" SK-GET TTRUE 3 T=

\ query path: the same well-framed store reads back per key (append-only latest-wins).
s" k1" SCHED-GET TTRUE 7 T=
s" k2" SCHED-GET TTRUE 3 T=
s" nope" SCHED-GET nip TFALSE

\ round-trip through the VALIDATED writer (the normal path always terminates rows).
STORE-RESET  FRESH
s" wk" 5 SCHED-PUT
s" wk" SCHED-GET TTRUE 5 T=
STORE-REPLAY-LOAD                        \ a validly-written store never trips the frame check
s" wk" SK-GET TTRUE 5 T=

STORE-RESET  SK-TAB-RESET  REPLAY-RESET  \ leave no rows, table entries, or warm latch
T-REPORT

;package
