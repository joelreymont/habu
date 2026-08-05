\ gate-stats.f - checked append-only counters for native gate RCA.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ and lib/process-env.f.

$40000 constant GS-CAP
$200 constant GS-LINE-CAP
$0A constant GS-LF
$09 constant GS-TAB
$67 constant GS-CHAR-G
$3A constant GS-COLON
$2D constant GS-DASH
64 constant GS-GEN-CAP
256 constant GS-TROW-MAX
4 constant GS-SUBJ-N
-2 constant GS-SUBJ-AMBIG

create GS-PATH-BUF FS-PATH-CAP allot
create GS-LINE-BUF GS-LINE-CAP allot
create GS-BUF GS-CAP allot
create GS-SLOW-LABEL GS-LINE-CAP allot
create GS-CHILD-BUF GS-LINE-CAP allot
create GS-GEN-BUF GS-GEN-CAP allot
create GS-QUAL-BUF GS-LINE-CAP allot
create GS-TROW-OFFS GS-TROW-MAX cells allot
create GS-TROW-US GS-TROW-MAX cells allot
create GS-TROW-SUBJS GS-TROW-MAX cells allot
create GS-SUBJ-TOTALS GS-SUBJ-N cells allot
create GS-SUBJ-MAXES GS-SUBJ-N cells allot
create GS-SUBJ-COUNTS GS-SUBJ-N cells allot

variable GS-PATH-U
variable GS-INITED
variable GS-U
variable GS-I
variable GS-START
variable GS-LINE-U
variable GS-J
variable GS-SPAN-V
variable GS-SPAN-LABEL-I
variable GS-SPANS
variable GS-SLOW-MS
variable GS-SLOW-U
variable GS-CHILD-U
variable GS-GEN-U
variable GS-GEN-I
variable GS-UQ-I
variable GS-TROW-N
variable GS-LOAD-SPANS
variable GS-LOAD-MS
variable GS-SPAN-STRAY
variable GS-SPAN-STRAY-EXPECTED
variable GS-SPAN-STRAY-UNEXPECTED
variable GS-T-I
variable GS-T-START
variable GS-T-K
variable GS-LABEL-DUP
variable GS-LS-I
variable GS-LS-FOUND
variable GS-LD-I
variable GS-LD-J

variable GS-TOP-PHASE
variable GS-TOP-CAPTURE
variable GS-UNDER-PHASE
variable GS-UNDER-ENV
variable GS-RUNNER-PHASE
variable GS-RUNNER-BUILD
variable GS-INNER-HB
variable GS-INNER-HB-STDIN
variable GS-INPROCESS-EVAL
variable GS-BOUNDARY
variable GS-MAKER-HIT
variable GS-MAKER-MISS
variable GS-MAKER-BUILD
variable GS-MAKER-RUN
variable GS-ARTIFACT-HIT
variable GS-ARTIFACT-MISS
variable GS-CANDIDATE
variable GS-CANDIDATE-HIT
variable GS-CANDIDATE-MISS
variable GS-CANDIDATE-INSTALL
variable GS-CANDIDATE-IMPORT
variable GS-CANDIDATE-READY
variable GS-CANDIDATE-BUILD-SKIP
variable GS-CANDIDATE-VALIDATE
variable GS-CANDIDATE-CORRUPT
variable GS-HELPER-SPAWN

: GS-FALSE ( -- bool )
   0 0= 0= ;

: GS-TRUE ( -- bool )
   0 0= ;

: GS-DIGIT? ( n -- bool ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and ;

\ Generation: a per-process identity path ("0", "0-3", "0-3-7") rooted at "0"
\ and extended by the pool at every fork (parent gen + "-" + slot seq). Span
\ and test-row labels are qualified with it ("g<gen>:<label>") so equal label
\ bytes emitted by different pool generations stay distinct identities.
: GS-GEN$ ( -- ptr u8 n )
   GS-GEN-BUF GS-GEN-U @ ;

: GS-GEN-CHAR? ( n -- bool ) {: c:n :}
   c GS-DIGIT? if GS-TRUE exit then
   c GS-DASH = ;

: GS-GEN-OK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 < if GS-FALSE exit then
   u GS-GEN-CAP > if GS-FALSE exit then
   0 GS-GEN-I !
   begin GS-GEN-I @ u < while
      a GS-GEN-I @ BYTE+ c@ GS-GEN-CHAR? 0= if GS-FALSE exit then
      GS-GEN-I @ 1+ GS-GEN-I !
   repeat GS-TRUE ;

: GS-GEN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u GS-GEN-CAP > if E-STR-CAPACITY throw then
   a u GS-GEN-OK? 0= if E-STR-BOUNDS throw then
   a GS-GEN-BUF u BYTE-COPY
   u GS-GEN-U ! ;

\ Spawn-slot generation inheritance: a pool-SPAWNED child cannot inherit the
\ parent's in-memory generation, so the pool exports the slot generation as
\ HABU_GATE_GEN and this init adopts it. Absent means root "0". A malformed
\ value dies with a source-pointing diagnostic - a bare throw here would exit
\ with an opaque low-byte code and no message, and a silent "0" fallback would
\ hide a broken exporter.
: GS-GEN-INIT ( -- )
   s" HABU_GATE_GEN" GETENV dup 0= if
      2drop s" 0" GS-GEN! exit
   then
   2dup GS-GEN-OK? 0= if
      2drop s" gate-stats: malformed HABU_GATE_GEN in environment (digits and dashes only)" 1 die
   then
   GS-GEN! ;

GS-GEN-INIT

: GS-QUAL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   GS-GEN-U @ u + 2 + GS-LINE-CAP > if E-STR-CAPACITY throw then
   GS-CHAR-G GS-QUAL-BUF c!
   GS-GEN-BUF GS-QUAL-BUF 1 BYTE+ GS-GEN-U @ BYTE-COPY
   GS-COLON GS-QUAL-BUF GS-GEN-U @ 1+ BYTE+ c!
   a GS-QUAL-BUF GS-GEN-U @ 2 + BYTE+ u BYTE-COPY
   GS-QUAL-BUF GS-GEN-U @ 2 + u + ;

\ Strip a "g<gen>:" qualifier for display and stray classification; a label
\ without one comes back unchanged.
: GS-UNQUAL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 3 < if a u exit then
   a c@ GS-CHAR-G <> if a u exit then
   1 GS-UQ-I !
   begin GS-UQ-I @ u < while
      a GS-UQ-I @ BYTE+ c@ {: c:n :}
      c GS-COLON = if
         GS-UQ-I @ 1 = if a u exit then
         a GS-UQ-I @ 1+ BYTE+ u GS-UQ-I @ 1+ - exit
      then
      c GS-GEN-CHAR? 0= if a u exit then
      GS-UQ-I @ 1+ GS-UQ-I !
   repeat
   a u ;

: GS-EMPTY$ ( -- ptr u8 n )
   GS-LINE-BUF 0 ;

: GS-PATH$ ( -- ptr u8 n )
   GS-PATH-BUF GS-PATH-U @ ;

: GS-COPY-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GS-PATH-BUF u BYTE-COPY
   u GS-PATH-U ! ;

: GS-ENV-PATH! ( -- )
   s" HABU_GATE_STATS" GETENV dup 0= if
      2drop 0 GS-PATH-U ! exit
   then
   GS-COPY-PATH! ;

: GS-ENSURE ( -- )
   GS-INITED @ 0= if
      GS-ENV-PATH!
      -1 GS-INITED !
   then ;

: GS-ON? ( -- bool )
   GS-ENSURE
   GS-PATH-U @ 0 > ;

: GS-ROOT! ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" gate-stats.tsv" GS-PATH-BUF JOIN-PATH GS-PATH-U !
   -1 GS-INITED !
   GS-PATH$ GS-EMPTY$ WRITE-ALL ;

: GS-ENV+ ( -- )
   GS-ON? if s" HABU_GATE_STATS" >LEN GS-PATH$ >LEN PROC-ENV+ then ;

: GS-EVENT ( ptr u8 n -- ) {: a:ptr u:n :}
   GS-ON? 0= if exit then
   u 0 < if E-STR-BOUNDS throw then
   u 1 + GS-LINE-CAP > if E-STR-CAPACITY throw then
   a GS-LINE-BUF u BYTE-COPY
   GS-LF GS-LINE-BUF u + c!
   GS-PATH$ GS-LINE-BUF u 1 + APPEND-FILE ;

: GS-LINE-RESET ( -- )
   0 GS-LINE-U ! ;

: GS-LINE-CHECK ( n -- ) {: add:n :}
   add 0 < if E-STR-BOUNDS throw then
   GS-LINE-U @ add + GS-LINE-CAP > if E-STR-CAPACITY throw then ;

: GS-LINE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u GS-LINE-CHECK
   a GS-LINE-BUF GS-LINE-U @ + u BYTE-COPY
   GS-LINE-U @ u + GS-LINE-U ! ;

: GS-LINE-C+ ( n -- ) {: c:n :}
   1 GS-LINE-CHECK
   c GS-LINE-BUF GS-LINE-U @ + c!
   GS-LINE-U @ 1+ GS-LINE-U ! ;

: GS-EVENT-FIELD ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n :}
   GS-ON? 0= if exit then
   GS-LINE-RESET
   key keyu GS-LINE+
   GS-TAB GS-LINE-C+
   val valu GS-LINE+
   GS-LF GS-LINE-C+
   GS-PATH$ GS-LINE-BUF GS-LINE-U @ APPEND-FILE ;

: GS-INNER-HB-EVENT ( ptr u8 n -- )
   s" inner-hb-spawn" 2swap GS-EVENT-FIELD ;

: GS-INNER-HB-STDIN-EVENT ( ptr u8 n -- )
   s" inner-hb-stdin" 2swap GS-EVENT-FIELD ;

: GS-BOUNDARY-EVENT ( ptr u8 n -- )
   s" boundary-test" 2swap GS-EVENT-FIELD ;

: GS-LINE-U+ ( n -- ) {: v:n :}
   v 0 < if E-STR-BOUNDS throw then
   v 10 >= if v 10 / RECURSE then
   v 10 mod STR-ZERO + GS-LINE-C+ ;

\ Store the fork child's own slot identity, qualified with the generation the
\ pool set for this child, so the suppression byte-match below compares whole
\ identities ("g<gen>:<label>"), never raw label bytes.
: GS-CHILD-LABEL! ( ptr u8 n -- )
   GS-QUAL$ {: a:ptr u:n :}
   a GS-CHILD-BUF u BYTE-COPY
   u GS-CHILD-U ! ;

\ A fork child suppresses re-emitting the span for its own qualified slot
\ identity because the parent pool emits the authoritative one (GS-SPAN-AUTH).
\ Generation qualification keeps another generation's reuse of the same label
\ bytes from matching; within one generation the label IS the identity, and a
\ cross-subject reuse there is witnessed by GS-LABEL-DUP and its hard guard.
: GS-CHILD-OWNED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   GS-CHILD-U @ 0= if GS-FALSE exit then
   a u GS-CHILD-BUF GS-CHILD-U @ STR= ;

\ Complete process telemetry is installed only by the gate stats module. The
\ process library owns the choke hooks; this package owns gate attribution,
\ candidate identity, per-process live counts, and the one-shot owner seam.
package GATE-PROCESS

create OWNER-BUF GS-LINE-CAP allot

variable OWNER-U
variable EXECS
variable FORKS
variable REAPERS
variable CANDS
variable LIVE-EXECS
variable LIVE-FORKS
variable FIELD-I
variable FIELD-START

: OWNER-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u GS-LINE-CAP > if E-STR-CAPACITY throw then
   a OWNER-BUF u BYTE-COPY
   u OWNER-U ! ;

: OWNER$ ( -- ptr u8 n )
   OWNER-U @ 0 > if OWNER-BUF OWNER-U @ exit then
   GS-CHILD-U @ 0 > if GS-CHILD-BUF GS-CHILD-U @ GS-UNQUAL$ exit then
   GS-EMPTY$ ;

: CANDIDATE? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   s" HABU_UNDER_TEST" GETENV dup 0 <> if
      path pathu 2swap STR= if GS-TRUE exit then
   else
      2drop
   then
   path pathu s" hb-under-test" ENDS-WITH? ;

: BASELINE? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu s" bin/hb" ENDS-WITH? ;

: IDENTITY$ ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   path pathu CANDIDATE? if s" candidate" exit then
   path pathu BASELINE? if s" baseline" exit then
   s" other" ;

: FIELD+ ( ptr u8 n -- )
   GS-TAB GS-LINE-C+
   GS-LINE+ ;

: EVENT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: kind:ptr kindu:n path:ptr pathu:n ident:ptr identu:n :}
   GS-ON? 0= if 0 OWNER-U ! exit then
   GS-LINE-RESET
   kind kindu GS-LINE+
   GS-GEN$ FIELD+
   OWNER$ dup 0= if 2drop path pathu then FIELD+
   ident identu FIELD+
   path pathu FIELD+
   GS-LF GS-LINE-C+
   GS-PATH$ GS-LINE-BUF GS-LINE-U @ APPEND-FILE
   0 OWNER-U ! ;

: EXEC-EVENT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   LIVE-EXECS @ 1+ LIVE-EXECS !
   s" process-exec" path pathu path pathu IDENTITY$ EVENT ;

: FORK-EVENT ( ptr u8 n -- ) {: role:ptr roleu:n :}
   LIVE-FORKS @ 1+ LIVE-FORKS !
   OWNER-U @ 0= GS-CHILD-U @ 0= and if s" root" OWNER-COPY! then
   s" process-fork" role roleu s" -" EVENT ;

: CHILD-RESET ( -- )
   0 OWNER-U !
   0 LIVE-EXECS !
   0 LIVE-FORKS ! ;

public

: OWNER! ( ptr u8 n -- )
   GS-ON? if OWNER-COPY! else 2drop then ;

: INSTALL ( -- )
   [: EXEC-EVENT ;] PROCESS-TRACE:EXEC-HOOK!
   [: FORK-EVENT ;] PROCESS-TRACE:FORK-HOOK!
   [: 0 OWNER-U ! ;] PROCESS-TRACE:CLEAR-HOOK!
   [: CHILD-RESET ;] PROCESS-TRACE:CHILD-HOOK! ;

: COUNTS-RESET ( -- )
   0 OWNER-U !
   0 EXECS !
   0 FORKS !
   0 REAPERS !
   0 CANDS ! ;

: LIVE-RESET ( -- )
   0 LIVE-EXECS !
   0 LIVE-FORKS ! ;

: EXEC# ( -- n ) EXECS @ ;
: FORK# ( -- n ) FORKS @ ;
: REAPER# ( -- n ) REAPERS @ ;
: CANDIDATE# ( -- n ) CANDS @ ;
: LIVE-EXEC# ( -- n ) LIVE-EXECS @ ;
: LIVE-FORK# ( -- n ) LIVE-FORKS @ ;

;package

GATE-PROCESS:INSTALL

\ Legacy wrapper sites now supply attribution only; the process choke writes
\ the sole count, so a wrapper and its raw spawn cannot double-count one exec.
: GS-HELPER-EVENT ( ptr u8 n -- )
   GATE-PROCESS:OWNER! ;

: GS-SPAN-EMIT ( ptr u8 n ptr u8 n n -- ) {: kind:ptr kindu:n label:ptr labelu:n ms:n :}
   GS-ON? 0= if exit then
   GS-LINE-RESET
   kind kindu GS-LINE+
   GS-TAB GS-LINE-C+
   ms GS-LINE-U+
   GS-TAB GS-LINE-C+
   label labelu GS-LINE+
   GS-LF GS-LINE-C+
   GS-PATH$ GS-LINE-BUF GS-LINE-U @ APPEND-FILE ;

: GS-SPAN ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   label labelu GS-QUAL$ {: ql:ptr qlu:n :}
   ql qlu GS-CHILD-OWNED? if exit then
   s" span" ql qlu ms GS-SPAN-EMIT ;

\ Pool-authoritative completion span (pass hooks only): the emitting pool owns
\ the slot, so this must never be self-suppressed - a fork child that is itself
\ a pool parent emits its nested slots' authoritative spans here even when a
\ nested label collides with the child's own slot label.
: GS-SPAN-AUTH ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   s" span" label labelu GS-QUAL$ ms GS-SPAN-EMIT ;

: GS-SPAN-LOAD ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   s" span-load" label labelu ms GS-SPAN-EMIT ;

\ Pool-timeout RCA row: a distinct kind so a slot the pool timed out under
\ saturation is attributable in gate-stats.tsv, not folded into a span/test row.
\ Fields: kind, elapsed ms, qualified label, live-at-kill, pool limit, WAIT count.
: GS-POOL-TIMEOUT ( ptr u8 n n n n n -- ) {: label:ptr labelu:n ms:n live:n limit:n waits:n :}
   GS-ON? 0= if exit then
   GS-LINE-RESET
   s" pool-timeout" GS-LINE+
   GS-TAB GS-LINE-C+
   ms GS-LINE-U+
   GS-TAB GS-LINE-C+
   label labelu GS-QUAL$ GS-LINE+
   GS-TAB GS-LINE-C+
   live GS-LINE-U+
   GS-TAB GS-LINE-C+
   limit GS-LINE-U+
   GS-TAB GS-LINE-C+
   waits GS-LINE-U+
   GS-LF GS-LINE-C+
   GS-PATH$ GS-LINE-BUF GS-LINE-U @ APPEND-FILE ;

: GS-TAB+ ( -- )
   GS-TAB GS-LINE-C+ ;

: GS-RUNTIME-SUBJECT-HEAD ( n bool ptr u8 n -- )
   {: id:n parity:bool src:ptr srcu:n :}
   s" runtime-subject" GS-LINE+
   GS-TAB+ s" id=" GS-LINE+ id GS-LINE-U+
   GS-TAB+ s" mode=" GS-LINE+
   parity if s" parity" else s" subject" then GS-LINE+
   GS-TAB+ s" source-sha256=" GS-LINE+ src srcu GS-LINE+ ;

: GS-RUNTIME-SUBJECT-OUTCOME ( bool bool n -- )
   {: exited:bool timed:bool code:n :}
   GS-TAB+ s" outcome=" GS-LINE+
   timed if s" timeout" GS-LINE+ exit then
   exited if s" exited:" else s" signaled:" then GS-LINE+
   code GS-LINE-U+ ;

: GS-RUNTIME-SUBJECT-OUT ( n ptr u8 n -- )
   {: u:n sha:ptr shau:n :}
   GS-TAB+ s" stdout-len=" GS-LINE+ u GS-LINE-U+
   GS-TAB+ s" stdout-sha256=" GS-LINE+ sha shau GS-LINE+ ;

: GS-RUNTIME-SUBJECT-ERR ( n ptr u8 n -- )
   {: u:n sha:ptr shau:n :}
   GS-TAB+ s" stderr-len=" GS-LINE+ u GS-LINE-U+
   GS-TAB+ s" stderr-sha256=" GS-LINE+ sha shau GS-LINE+ ;

: GS-RUNTIME-SUBJECT ( n bool ptr u8 n bool bool n n ptr u8 n n ptr u8 n -- )
   {: id:n parity:bool src:ptr srcu:n exited:bool timed:bool code:n
      outu:n outsha:ptr outshau:n erru:n errsha:ptr errshau:n :}
   GS-ON? 0= if exit then
   GS-LINE-RESET
   id parity src srcu GS-RUNTIME-SUBJECT-HEAD
   exited timed code GS-RUNTIME-SUBJECT-OUTCOME
   outu outsha outshau GS-RUNTIME-SUBJECT-OUT
   erru errsha errshau GS-RUNTIME-SUBJECT-ERR
   GS-LF GS-LINE-C+
   GS-PATH$ GS-LINE-BUF GS-LINE-U @ APPEND-FILE ;

: GS-TEST ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n subj:ptr subju:n run:ptr runu:n bound:ptr boundu:n sha:ptr shau:n :}
   GS-ON? 0= if exit then
   GS-LINE-RESET
   s" test" GS-LINE+
   GS-TAB+
   label labelu GS-QUAL$ GS-LINE+
   GS-TAB+
   subj subju GS-LINE+
   GS-TAB+
   run runu GS-LINE+
   GS-TAB+
   bound boundu GS-LINE+
   GS-TAB+
   sha shau GS-LINE+
   GS-LF GS-LINE-C+
   GS-PATH$ GS-LINE-BUF GS-LINE-U @ APPEND-FILE ;

: GS-INC ( ptr n -- ) {: p:ptr :}
   p @ 1 + p ! ;

: GS-SUBJ$ ( n -- ptr u8 n ) {: i:n :}
   i case
      0 of s" host-source" endof
      1 of s" candidate-cli" endof
      2 of s" candidate-source" endof
      3 of s" artifact" endof
      E-TBL-BOUNDS throw
   endcase ;

: GS-SUBJ-CHECK ( n -- n ) {: i:n :}
   i 0 < if E-TBL-BOUNDS throw then
   i GS-SUBJ-N >= if E-TBL-BOUNDS throw then
   i ;

: GS-SUBJ-TOTAL-PTR ( n -- ptr n )
   GS-SUBJ-CHECK cells GS-SUBJ-TOTALS + ;

: GS-SUBJ-MAX-PTR ( n -- ptr n )
   GS-SUBJ-CHECK cells GS-SUBJ-MAXES + ;

: GS-SUBJ-COUNT-PTR ( n -- ptr n )
   GS-SUBJ-CHECK cells GS-SUBJ-COUNTS + ;

: GS-SUBJ-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup GS-SUBJ-N < while
      dup GS-SUBJ$ a u STR= if exit then
      1+
   repeat drop
   -1 ;

: GS-SUBJ-RESET ( n -- ) {: i:n :}
   0 i GS-SUBJ-TOTAL-PTR !
   0 i GS-SUBJ-MAX-PTR !
   0 i GS-SUBJ-COUNT-PTR ! ;

: GS-SUBJS-RESET ( -- )
   0 begin dup GS-SUBJ-N < while
      dup GS-SUBJ-RESET
      1+
   repeat drop ;

: GS-RESET-COUNTS ( -- )
   0 GS-TOP-PHASE !
   0 GS-TOP-CAPTURE !
   0 GS-UNDER-PHASE !
   0 GS-UNDER-ENV !
   0 GS-RUNNER-PHASE !
   0 GS-RUNNER-BUILD !
   0 GS-INNER-HB !
   0 GS-INNER-HB-STDIN !
   0 GS-INPROCESS-EVAL !
   0 GS-BOUNDARY !
   0 GS-MAKER-HIT !
   0 GS-MAKER-MISS !
   0 GS-MAKER-BUILD !
   0 GS-MAKER-RUN !
   0 GS-ARTIFACT-HIT !
   0 GS-ARTIFACT-MISS !
   0 GS-CANDIDATE !
   0 GS-CANDIDATE-HIT !
   0 GS-CANDIDATE-MISS !
   0 GS-CANDIDATE-INSTALL !
   0 GS-CANDIDATE-IMPORT !
   0 GS-CANDIDATE-READY !
   0 GS-CANDIDATE-BUILD-SKIP !
   0 GS-CANDIDATE-VALIDATE !
   0 GS-CANDIDATE-CORRUPT !
   0 GS-HELPER-SPAWN !
   GATE-PROCESS:COUNTS-RESET
   0 GS-SPANS !
   0 GS-SLOW-MS !
   0 GS-SLOW-U !
   0 GS-LOAD-SPANS !
   0 GS-LOAD-MS !
   0 GS-SPAN-STRAY !
   0 GS-SPAN-STRAY-EXPECTED !
   0 GS-SPAN-STRAY-UNEXPECTED !
   0 GS-LABEL-DUP !
   0 GS-TROW-N !
   GS-SUBJS-RESET ;

: GS-KIND-PREFIX? ( n n ptr u8 n -- bool ) {: off:n u:n key:ptr keyu:n :}
   u keyu 2 + < if GS-FALSE exit then
   GS-BUF off BYTE+ keyu key keyu STR= 0= if GS-FALSE exit then
   GS-BUF off keyu + BYTE+ c@ GS-TAB = ;

: GS-KIND-SPAN-PARSE? ( n n ptr u8 n -- bool ) {: off:n u:n key:ptr keyu:n :}
   off u key keyu GS-KIND-PREFIX? 0= if GS-FALSE exit then
   0 GS-SPAN-V !
   keyu 1+ GS-J !
   begin GS-J @ u < while
      GS-BUF off GS-J @ + BYTE+ c@ {: c:n :}
      c GS-TAB = if
         GS-J @ keyu 1+ = if GS-FALSE exit then
         GS-J @ 1+ GS-SPAN-LABEL-I !
         GS-SPAN-LABEL-I @ u < if GS-TRUE exit then
         GS-FALSE exit
      then
      c GS-DIGIT? 0= if GS-FALSE exit then
      GS-SPAN-V @ 10 * c STR-ZERO - + GS-SPAN-V !
      GS-J @ 1+ GS-J !
   repeat
   GS-FALSE ;

: GS-SPAN-PARSE? ( n n -- bool ) {: off:n u:n :}
   off u s" span" GS-KIND-SPAN-PARSE? ;

: GS-SLOW! ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   labelu GS-LINE-CAP > if E-STR-CAPACITY throw then
   ms GS-SLOW-MS !
   label GS-SLOW-LABEL labelu BYTE-COPY
   labelu GS-SLOW-U ! ;

: GS-FIELD-END ( n n n -- n ) {: off:n u:n start:n :}
   start GS-T-K !
   begin GS-T-K @ u < while
      GS-BUF off GS-T-K @ + BYTE+ c@ GS-TAB = if GS-T-K @ exit then
      GS-T-K @ 1+ GS-T-K !
   repeat
   u ;

: GS-TROW+ ( n n n -- ) {: off:n labelu:n subj:n :}
   GS-TROW-N @ GS-TROW-MAX >= if
      s" gate-stats: test-row table overflow; raise GS-TROW-MAX" 1 die
   then
   off GS-TROW-N @ cells GS-TROW-OFFS + !
   labelu GS-TROW-N @ cells GS-TROW-US + !
   subj GS-TROW-N @ cells GS-TROW-SUBJS + !
   GS-TROW-N @ 1+ GS-TROW-N ! ;

: GS-INDEX-TEST-LINE ( n n -- ) {: off:n u:n :}
   off u s" test" GS-KIND-PREFIX? 0= if exit then
   off u 5 GS-FIELD-END {: lend:n :}
   lend 5 <= if exit then
   lend u >= if exit then
   off u lend 1+ GS-FIELD-END {: send:n :}
   GS-BUF off lend 1+ + BYTE+ send lend 1+ - GS-SUBJ-ID {: id:n :}
   id 0 < if exit then
   off 5 + lend 5 - id GS-TROW+ ;

: GS-INDEX-TESTS ( -- )
   0 GS-T-START !
   0 GS-T-I !
   begin GS-T-I @ GS-U @ < while
      GS-BUF GS-T-I @ BYTE+ c@ GS-LF = if
         GS-T-START @ GS-T-I @ GS-T-START @ - GS-INDEX-TEST-LINE
         GS-T-I @ 1 + GS-T-START !
      then
      GS-T-I @ 1 + GS-T-I !
   repeat
   GS-T-START @ GS-U @ < if
      GS-T-START @ GS-U @ GS-T-START @ - GS-INDEX-TEST-LINE
   then ;

: GS-TROW-LABEL$ ( n -- ptr u8 n ) {: i:n :}
   GS-BUF i cells GS-TROW-OFFS + @ BYTE+ i cells GS-TROW-US + @ ;

\ Resolve a span label to its subject by matching indexed test rows. A label is
\ an IDENTITY key here, so two test rows sharing the label text but naming
\ DIFFERENT subjects make it ambiguous: string equality alone would silently join
\ the span to whichever row indexed first (an attribution bug). Return the subject
\ only when every matching row agrees; return GS-SUBJ-AMBIG when they conflict, or
\ -1 when nothing matches. Rows repeating one label under the SAME subject (re-run)
\ still resolve, not conflict.
: GS-LABEL-SUBJ ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 GS-LS-FOUND !
   0 GS-LS-I !
   begin GS-LS-I @ GS-TROW-N @ < while
      GS-LS-I @ GS-TROW-LABEL$ a u STR= if
         GS-LS-I @ cells GS-TROW-SUBJS + @ {: s:n :}
         GS-LS-FOUND @ 0 < if
            s GS-LS-FOUND !
         else
            s GS-LS-FOUND @ <> if GS-SUBJ-AMBIG exit then
         then
      then
      GS-LS-I @ 1+ GS-LS-I !
   repeat
   GS-LS-FOUND @ ;

\ A test row is a label collision when an earlier row carries the same label but
\ a different subject; that is the exact condition that breaks both attribution
\ (GS-LABEL-SUBJ) and fork-child span suppression (GS-CHILD-OWNED?), which both
\ key on raw label bytes. GS-LABEL-DUP counts these so the collision is visible in
\ the gate-stats RCA output instead of silently miscounting.
: GS-ROW-LABEL-DUP? ( n -- bool ) {: i:n :}
   0 GS-LD-J !
   begin GS-LD-J @ i < while
      GS-LD-J @ GS-TROW-LABEL$ i GS-TROW-LABEL$ STR= if
         GS-LD-J @ cells GS-TROW-SUBJS + @  i cells GS-TROW-SUBJS + @  <> if
            GS-TRUE exit
         then
      then
      GS-LD-J @ 1+ GS-LD-J !
   repeat GS-FALSE ;

: GS-CHECK-LABEL-DUPS ( -- )
   0 GS-LD-I !
   begin GS-LD-I @ GS-TROW-N @ < while
      GS-LD-I @ GS-ROW-LABEL-DUP? if GS-LABEL-DUP GS-INC then
      GS-LD-I @ 1+ GS-LD-I !
   repeat ;

\ Hard gate: a nonzero label-dup means fork-child span suppression and subject
\ attribution can silently miscount, so the runner refuses the run instead of
\ reporting around it. Callers run this after GS-SUMMARY has scanned.
: GS-LABEL-DUP-GUARD ( -- )
   GS-LABEL-DUP @ 0= if exit then
   s" gate-stats: duplicate test labels across subjects - qualify the label at its source" 1 die ;

: GS-SPAN-LABEL$ ( n n -- ptr u8 n ) {: off:n u:n :}
   GS-BUF off GS-SPAN-LABEL-I @ + BYTE+ u GS-SPAN-LABEL-I @ - ;

\ Registered fixture-span prefixes: a stray (span with no matching test row) is
\ expected drill-down detail when its label starts with one of these. Anything
\ else is an unexpected stray worth investigating as an attribution bug.

: GS-STRAY-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" lib/" STARTS-WITH? if GS-TRUE exit then
   a u s" tools/" STARTS-WITH? if GS-TRUE exit then
   a u s" test/" STARTS-WITH? if GS-TRUE exit then
   GS-FALSE ;

: GS-STRAY-SLICE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" dictionary/" STARTS-WITH? if GS-TRUE exit then
   a u s" lint-tools/" STARTS-WITH? if GS-TRUE exit then
   a u s" fork " STARTS-WITH? if GS-TRUE exit then
   a u s" fs mutation " STARTS-WITH? if GS-TRUE exit then
   a u s" process " STARTS-WITH? if GS-TRUE exit then
   a u s" hb baseline " STARTS-WITH? if GS-TRUE exit then
   GS-FALSE ;

: GS-STRAY-LINT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" dot-dep-lint" STARTS-WITH? if GS-TRUE exit then
   a u s" maki-dep-lint" STARTS-WITH? if GS-TRUE exit then
   a u s" repl-lint" STARTS-WITH? if GS-TRUE exit then
   GS-FALSE ;

: GS-STRAY-EXPECTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u GS-STRAY-PATH? if GS-TRUE exit then
   a u GS-STRAY-SLICE? if GS-TRUE exit then
   a u GS-STRAY-LINT? ;

: GS-STRAY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GS-SPAN-STRAY GS-INC
   a u GS-UNQUAL$ GS-STRAY-EXPECTED? if GS-SPAN-STRAY-EXPECTED GS-INC exit then
   GS-SPAN-STRAY-UNEXPECTED GS-INC ;

: GS-SUBJ+ ( ptr u8 n n n -- ) {: label:ptr labelu:n id:n ms:n :}
   id GS-SUBJ-AMBIG = if
      GS-SPAN-STRAY GS-INC GS-SPAN-STRAY-UNEXPECTED GS-INC exit
   then
   id 0 < if label labelu GS-STRAY+ exit then
   id GS-SUBJ-TOTAL-PTR {: tp:ptr :}
   tp @ ms + tp !
   id GS-SUBJ-COUNT-PTR {: cp:ptr :}
   cp @ 1 + cp !
   id GS-SUBJ-MAX-PTR {: mp:ptr :}
   ms mp @ > if ms mp ! then ;

: GS-COUNT-SPAN ( n n -- ) {: off:n u:n :}
   off u GS-SPAN-PARSE? 0= if exit then
   GS-SPANS GS-INC
   GS-SPAN-V @ {: ms:n :}
   off u GS-SPAN-LABEL$ {: la:ptr lu:n :}
   ms GS-SLOW-MS @ > if la lu GS-UNQUAL$ ms GS-SLOW! then
   la lu la lu GS-LABEL-SUBJ ms GS-SUBJ+ ;

: GS-COUNT-LOAD-SPAN ( n n -- ) {: off:n u:n :}
   off u s" span-load" GS-KIND-SPAN-PARSE? 0= if exit then
   GS-LOAD-SPANS GS-INC
   GS-LOAD-MS @ GS-SPAN-V @ + GS-LOAD-MS ! ;

: GS-LINE= ( n n ptr u8 n -- bool ) {: off:n u:n key:ptr keyu:n :}
   u keyu <> if GS-FALSE exit then
   GS-BUF off BYTE+ u key keyu STR= ;

: GS-LINE-KEY? ( n n ptr u8 n -- bool ) {: off:n u:n key:ptr keyu:n :}
   u keyu < if GS-FALSE exit then
   GS-BUF off BYTE+ keyu key keyu STR= 0= if GS-FALSE exit then
   u keyu = if GS-TRUE exit then
   GS-BUF off keyu + BYTE+ c@ GS-TAB = ;

package GATE-PROCESS

variable ROW-I
variable ROW-START
variable ROW-N

: ROW-MATCH? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: row:ptr rowu:n kind:ptr kindu:n owner:ptr owneru:n :}
   row rowu GS-TAB 0 SPLIT-NEXT 0= if 2drop drop GS-FALSE exit then
   {: got-kind:ptr got-kindu:n next:n :}
   got-kind got-kindu kind kindu STR= 0= if GS-FALSE exit then
   row rowu GS-TAB next SPLIT-NEXT 0= if 2drop drop GS-FALSE exit then
   {: gen:ptr genu:n owner-off:n :}
   gen genu 2drop
   row rowu GS-TAB owner-off SPLIT-NEXT 2drop
   owner owneru STR= ;

: COUNT-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   ROW-MATCH? if ROW-N @ 1+ ROW-N ! then ;

public

: ROW-COUNT$ ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n kind:ptr kindu:n owner:ptr owneru:n :}
   0 ROW-N !
   0 ROW-START !
   0 ROW-I !
   begin ROW-I @ u <= while
      ROW-I @ u = if
         a ROW-START @ BYTE+ ROW-I @ ROW-START @ -
         kind kindu owner owneru COUNT-ROW
      else
         a ROW-I @ BYTE+ c@ GS-LF = if
            a ROW-START @ BYTE+ ROW-I @ ROW-START @ -
            kind kindu owner owneru COUNT-ROW
            ROW-I @ 1+ ROW-START !
         then
      then
      ROW-I @ 1+ ROW-I !
   repeat
   ROW-N @ ;

: FIELD$ ( n n n -- ptr u8 n ) {: off:n u:n field:n :}
   field 0 < if E-STR-BOUNDS throw then
   0 FIELD-I !
   0 FIELD-START !
   begin FIELD-I @ field < while
      off u FIELD-START @ GS-FIELD-END {: end:n :}
      end u >= if GS-BUF 0 exit then
      end 1+ FIELD-START !
      FIELD-I @ 1+ FIELD-I !
   repeat
   FIELD-START @ u >= if GS-BUF 0 exit then
   off u FIELD-START @ GS-FIELD-END {: end:n :}
   GS-BUF off FIELD-START @ + BYTE+ end FIELD-START @ - ;

: COUNT-EXEC ( n n -- ) {: off:n u:n :}
   EXECS GS-INC
   GS-HELPER-SPAWN GS-INC
   off u 3 FIELD$ s" candidate" STR= if
      CANDS GS-INC
   then ;

: COUNT-FORK ( n n -- ) {: off:n u:n :}
   FORKS GS-INC
   off u 4 FIELD$ s" reaper" STR= if REAPERS GS-INC then ;

public

: COUNT? ( n n -- bool ) {: off:n u:n :}
   off u s" process-exec" GS-KIND-PREFIX? if
      off u COUNT-EXEC GS-TRUE exit
   then
   off u s" process-fork" GS-KIND-PREFIX? if
      off u COUNT-FORK GS-TRUE exit
   then
   GS-FALSE ;

;package

: GS-COUNT-LINE ( n n -- ) {: off:n u:n :}
   off u GATE-PROCESS:COUNT? if exit then
   off u GS-COUNT-SPAN
   off u GS-COUNT-LOAD-SPAN
   off u s" top-phase-spawn" GS-LINE= if GS-TOP-PHASE GS-INC exit then
   off u s" top-capture-spawn" GS-LINE= if GS-TOP-CAPTURE GS-INC exit then
   off u s" under-phase-spawn" GS-LINE= if GS-UNDER-PHASE GS-INC exit then
   off u s" under-env" GS-LINE= if GS-UNDER-ENV GS-INC exit then
   off u s" runner-phase-spawn" GS-LINE= if GS-RUNNER-PHASE GS-INC exit then
   off u s" gate-runner-build" GS-LINE= if GS-RUNNER-BUILD GS-INC exit then
   off u s" inner-hb-spawn" GS-LINE-KEY? if GS-INNER-HB GS-INC exit then
   off u s" inner-hb-stdin" GS-LINE-KEY? if GS-INNER-HB-STDIN GS-INC exit then
   off u s" inprocess-eval" GS-LINE= if GS-INPROCESS-EVAL GS-INC exit then
   off u s" boundary-test" GS-LINE-KEY? if GS-BOUNDARY GS-INC exit then
   off u s" maker-cache-hit" GS-LINE= if GS-MAKER-HIT GS-INC exit then
   off u s" maker-cache-miss" GS-LINE= if GS-MAKER-MISS GS-INC exit then
   off u s" maker-build" GS-LINE= if GS-MAKER-BUILD GS-INC exit then
   off u s" maker-run" GS-LINE= if GS-MAKER-RUN GS-INC exit then
   off u s" artifact-cache-hit" GS-LINE= if GS-ARTIFACT-HIT GS-INC exit then
   off u s" artifact-cache-miss" GS-LINE= if GS-ARTIFACT-MISS GS-INC exit then
   off u s" candidate-build" GS-LINE= if GS-CANDIDATE GS-INC exit then
   off u s" candidate-cache-hit" GS-LINE= if GS-CANDIDATE-HIT GS-INC exit then
   off u s" candidate-cache-miss" GS-LINE= if GS-CANDIDATE-MISS GS-INC exit then
   off u s" candidate-cache-install" GS-LINE= if GS-CANDIDATE-INSTALL GS-INC exit then
   off u s" candidate-import" GS-LINE= if GS-CANDIDATE-IMPORT GS-INC exit then
   off u s" candidate-ready" GS-LINE= if GS-CANDIDATE-READY GS-INC exit then
   off u s" candidate-build-skip" GS-LINE= if GS-CANDIDATE-BUILD-SKIP GS-INC exit then
   off u s" candidate-validate" GS-LINE= if GS-CANDIDATE-VALIDATE GS-INC exit then
   off u s" candidate-cache-corrupt" GS-LINE= if GS-CANDIDATE-CORRUPT GS-INC exit then ;

: GS-SCAN ( -- )
   GS-RESET-COUNTS
   GS-INDEX-TESTS
   GS-CHECK-LABEL-DUPS
   0 GS-START !
   0 GS-I !
   begin GS-I @ GS-U @ < while
      GS-BUF GS-I @ BYTE+ c@ GS-LF = if
         GS-START @ GS-I @ GS-START @ - GS-COUNT-LINE
         GS-I @ 1 + GS-START !
      then
      GS-I @ 1 + GS-I !
   repeat
   GS-START @ GS-U @ < if
      GS-START @ GS-U @ GS-START @ - GS-COUNT-LINE
   then ;

: GS-READ ( -- )
   GS-PATH$ FILE? 0= if 0 GS-U ! exit then
   GS-PATH$ GS-BUF GS-CAP READ-ALL GS-U ! ;

: GS-ITEM. ( n ptr u8 n -- ) {: v:n a:ptr u:n :}
   a u type s" =" type v . ;

: GS-SUBJ-LINE. ( n -- ) {: i:n :}
   s" subject " type i GS-SUBJ$ type s" : " type
   i GS-SUBJ-COUNT-PTR @ s" spans" GS-ITEM.
   i GS-SUBJ-TOTAL-PTR @ s" total-ms" GS-ITEM.
   i GS-SUBJ-MAX-PTR @ s" max-ms" GS-ITEM. ;

: GS-SUBJ-REPORT ( -- )
   0 begin dup GS-SUBJ-N < while
      dup GS-SUBJ-LINE.
      1+
   repeat drop ;

\ Within-attempt cache-counter contract for the perf verdict (dot
\ habu-integrate-robust-verdict-7f26769e). After GS-SUMMARY has scanned one
\ attempt's stats, the candidate must have become ready at least once and no
\ cache-stamp corruption may have been observed. Expected within-attempt hits
\ (candidate restored once, shared tool base) are legitimate and NOT cross-attempt
\ reuse; a fresh-root retry attempt gets its own zeroed counters.
: GS-ATTEMPT-CACHE-OK? ( -- bool )
   GS-CANDIDATE-CORRUPT @ 0=
   GS-CANDIDATE-READY @ 0 > and ;

: GS-SUMMARY ( -- )
   GS-ON? 0= if exit then
   GS-READ
   GS-SCAN
   s" test counts: " type
   GS-TOP-PHASE @ s" top-phase" GS-ITEM.
   GS-TOP-CAPTURE @ s" top-capture" GS-ITEM.
   GS-UNDER-PHASE @ s" under-phase" GS-ITEM.
   GS-UNDER-ENV @ s" under-env" GS-ITEM.
   GS-RUNNER-PHASE @ s" runner-phase" GS-ITEM.
   GS-RUNNER-BUILD @ s" runner-build" GS-ITEM.
   GS-INNER-HB @ s" inner-hb" GS-ITEM.
   GS-INNER-HB-STDIN @ s" inner-hb-stdin" GS-ITEM.
   GS-INPROCESS-EVAL @ s" inprocess-eval" GS-ITEM.
   GS-BOUNDARY @ s" boundary" GS-ITEM.
   GS-MAKER-HIT @ s" maker-hit" GS-ITEM.
   GS-MAKER-MISS @ s" maker-miss" GS-ITEM.
   GS-MAKER-BUILD @ s" maker-build" GS-ITEM.
   GS-MAKER-RUN @ s" maker-run" GS-ITEM.
   GS-ARTIFACT-HIT @ s" artifact-hit" GS-ITEM.
   GS-ARTIFACT-MISS @ s" artifact-miss" GS-ITEM.
   GS-CANDIDATE @ s" candidate" GS-ITEM.
   GS-CANDIDATE-HIT @ s" candidate-hit" GS-ITEM.
   GS-CANDIDATE-MISS @ s" candidate-miss" GS-ITEM.
   GS-CANDIDATE-INSTALL @ s" candidate-install" GS-ITEM.
   GS-CANDIDATE-IMPORT @ s" candidate-import" GS-ITEM.
   GS-CANDIDATE-READY @ s" candidate-ready" GS-ITEM.
   GS-CANDIDATE-BUILD-SKIP @ s" candidate-build-skip" GS-ITEM.
   GS-CANDIDATE-VALIDATE @ s" candidate-validate" GS-ITEM.
   GS-CANDIDATE-CORRUPT @ s" candidate-corrupt" GS-ITEM.
   GS-HELPER-SPAWN @ s" helper-spawn" GS-ITEM.
   GATE-PROCESS:EXEC# s" process-exec" GS-ITEM.
   GATE-PROCESS:FORK# s" process-fork" GS-ITEM.
   GATE-PROCESS:REAPER# s" process-reaper" GS-ITEM.
   GATE-PROCESS:CANDIDATE# s" candidate-exec" GS-ITEM.
   GS-SPANS @ s" spans" GS-ITEM.
   GS-LOAD-SPANS @ s" load-spans" GS-ITEM.
   GS-LOAD-MS @ s" load-ms" GS-ITEM.
   GS-SPAN-STRAY @ s" span-stray" GS-ITEM.
   GS-SPAN-STRAY-EXPECTED @ s" span-stray-expected" GS-ITEM.
   GS-SPAN-STRAY-UNEXPECTED @ s" span-stray-unexpected" GS-ITEM.
   GS-LABEL-DUP @ s" label-dup" GS-ITEM.
   GS-SPANS @ 0 > if
      s" slowest-test-ms=" type GS-SLOW-MS @ .
      s" slowest-test=" type GS-SLOW-LABEL GS-SLOW-U @ type
   then
   cr
   GS-SUBJ-REPORT ;
