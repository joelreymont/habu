\ lsp-check.f - the language server's check of one open document, and the one
\ place the server meets the checker: CHECK:VERIFY-BYTES-AT
\ (tools/check-verify-core.f), which runs none of the text. Every check reads each
\ other open document in place of its file on disk, under the file's canonical
\ path. STORAGE CLASS. PROCESS-GLOBAL: the check in progress belongs to the
\ server's one task.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/process.f
require tools/check-verify-core.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-diag.f

package LSP-CHECK
using LSP-DOCS

private

\ A deadlock guard and nothing else: a check measured 43 ms for one definition
\ and 433 ms for tools/check-core.f's whole closure; check-verify-test's guard is
\ the same.
60000 constant CHECK-MS

10 constant LF

variable SUBJECT                         \ the slot being checked
variable PUBLISHABLE                     \ whether its check completed,
TYPED-VARIABLE VERIFIED bool             \ whether its verdict was verified,
variable CURSOR                          \ and the byte of its cursor, -1 for none
TYPED-VARIABLE SRC-A ptr u8              \ the bytes checked for uses,
variable SRC-U
TYPED-VARIABLE SRC-P ptr u8              \ its path,
variable SRC-PU
TYPED-VARIABLE SRC-T ptr u8              \ the tail of the word they are of,
variable SRC-TU
variable SRC-OWNER                      \ slot supplying a probe's subject, -1 for disk
40 BUFFER: OUTCOME-B                     \ and what its check came to
variable OUTCOME-U
variable BATCH-CODE
variable BATCH-K

: ERR ( ptr u8 n -- )
   {: a:ptr u:n :}
   FD:STDERR a u FD:WRITE-FULL ;

: NUMBER$ ( n -- ptr u8 n )  SB-RESET FMT:SB-INT SB$ ;

\ A check that did not complete says why on one stderr line; the verifier's prose
\ follows it verbatim, as it follows a completed check's verdict line:
\    lsp: URI: not checked: exit N | signal N | deadline passed
\    lsp: URI: not checked: the verifier's image holds it
\    lsp: URI: not checked: throw N
: HEAD ( -- )
   s" lsp: " ERR
   SUBJECT @ DOC-URI$ ERR
   s" : " ERR ;

: NEWLINE ( -- )  s\" \n" ERR ;

: RELAY ( -- )
   CHECK:VERIFY-LOG$ {: a:ptr u:n :}
   u 0= if exit then
   a u ERR
   a u + 1- c@ LF <> if NEWLINE then ;

\ A check that completed with this verdict: said only when the verifier wrote
\ prose.
: COMPLETED ( ptr u8 n -- )
   {: a:ptr u:n :}
   true PUBLISHABLE !
   CHECK:VERIFY-LOG$ nip 0= if exit then
   HEAD a u ERR NEWLINE RELAY ;

: UNFINISHED ( outcome -- )
   HEAD s" not checked: " ERR
   MATCH outcome
      exited OF s" exit " ERR NUMBER$ ERR ENDOF
      signaled OF s" signal " ERR NUMBER$ ERR ENDOF
      timeout OF s" deadline passed" ERR ENDOF
   ;MATCH
   NEWLINE RELAY ;

: OTHERS ( [ ptr u8 n ptr u8 n -- ] -- )  SUBJECT @ swap DOC-EACH-BUT ;

: VERIFY ( -- )
   SUBJECT @ {: slot:n :}
   slot DOC-TEXT$ slot DOC-PATH$ CURSOR @ [: OTHERS ;] CHECK-MS >MS
   CHECK:VERIFY-BYTES-AT
   MATCH CHECK:verdict
      verified OF true VERIFIED ! s" verified" COMPLETED ENDOF
      refused OF s" refused" COMPLETED ENDOF
      engine-provided OF s" engine-provided" COMPLETED ENDOF
      held OF HEAD s" not checked: the verifier's image holds it" ERR NEWLINE RELAY ENDOF
      incomplete OF UNFINISHED ENDOF
      deferred OF s" deferred" COMPLETED ENDOF
   ;MATCH ;

: OUTCOME! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a OUTCOME-B u BYTE-COPY
   u OUTCOME-U ! ;

: COMPLETE-OUTCOME ( ptr u8 n -- )
   true PUBLISHABLE ! OUTCOME! ;

\ The check of the bytes as the file at the path, for the uses of the tail,
\ its verdict's name in OUTCOME-B.
: USES-VERDICT ( CHECK:verdict -- )
   MATCH CHECK:verdict
      verified OF true VERIFIED ! s" verified" COMPLETE-OUTCOME ENDOF
      refused OF s" refused" COMPLETE-OUTCOME ENDOF
      engine-provided OF s" engine-provided" COMPLETE-OUTCOME ENDOF
      held OF s" held" OUTCOME! ENDOF
      incomplete OF
         MATCH outcome
            exited OF drop ENDOF
            signaled OF drop ENDOF
            timeout OF ENDOF
         ;MATCH
         s" incomplete" OUTCOME!
      ENDOF
      deferred OF s" deferred" COMPLETE-OUTCOME ENDOF
   ;MATCH ;

: CHECKED ( -- )
   SRC-A @ SRC-U @ SRC-P @ SRC-PU @ SRC-T @ SRC-TU @
   [: -1 swap DOC-EACH-BUT ;] CHECK-MS >MS
   CHECK:VERIFY-BYTES-OF USES-VERDICT ;

: PROBED ( [ -- ] -- [ -- ] )
   PUBLISHABLE @ if
      CHECK:VERIFY-FILES$ CHECK:VERIFY-DEFS$ CHECK:VERIFY-USES$ VERIFIED @
      SRC-OWNER @
      LSP-DEFS:DEFS-PROBE
   then
   dup execute ;

public

\ Packets are published whatever the verdict: a check stopped by a later
\ definition still publishes those written before it. A completed check (verified,
\ refused, deferred, or the engine providing the file) replaces the last
\ definitions, uses and verified flag (LSP-DEFS:DEFS-KEEP) and top-level loads
\ (CHECK:VERIFY-LOADS$ into LSP-DOCS:DOC-LOADS!, answering tools/lsp-links.f), and
\ publishes even when it wrote none. Otherwise (the verifier's image holds the
\ file, no verdict, a throw) the last definitions stay for workspace symbols, but
\ uses and loads are dropped at the start (LSP-DEFS:DEFS-USES-DROP): they are
\ bytes of the text a check completed on. With no packets nothing is published,
\ so the client keeps its last list. A negative cursor is none; with one, the
\ checker's spellings at that byte land in CHECK:VERIFY-CANDIDATES$. The
\ document is clean from here on.
: RUN-AT ( n n -- )
   {: slot:n at:n :}
   slot DOC-CLEAN
   slot LSP-DEFS:DEFS-USES-DROP
   NULL$ slot DOC-LOADS!
   slot SUBJECT !
   at CURSOR !
   false PUBLISHABLE !
   false VERIFIED !
   [: VERIFY ;] catch {: code:n :}
   code 0<> if
      HEAD s" not checked: throw " ERR code NUMBER$ ERR NEWLINE RELAY
   then
   PUBLISHABLE @ if
      CHECK:VERIFY-LOADS$ slot DOC-LOADS!
      CHECK:VERIFY-FILES$ CHECK:VERIFY-DEFS$ CHECK:VERIFY-USES$
      CHECK:VERIFY-UNREAD$ slot
      VERIFIED @ LSP-DEFS:DEFS-KEEP
   then
   CHECK:VERIFY-OUT$ nip 0<> PUBLISHABLE @ or if
      slot CHECK:VERIFY-OUT$ LSP-DIAG:PUBLISH
   then ;

: RUN ( n -- )  -1 RUN-AT ;

\ Checks the bytes (an open document's text or the file on disk) as the file at
\ the path, with no cursor, publishing nothing and writing nothing to stderr. Its
\ uses are those bound to a located declaration in the file and, in every other
\ file read, those bound to a declaration of the canonical tail
\ (CHECK:VERIFY-BYTES-OF). A completed check is the store's probe
\ (LSP-DEFS:DEFS-PROBE), with its verified flag, while GATHER runs; the probe is
\ removed once GATHER returns or throws, and an unfinished check adds none.
: USES ( ptr u8 n ptr u8 n ptr u8 n n [ -- ] -- )
   {: a:ptr u:n p:ptr pu:n t:ptr tu:n owner:n gather :}
   a SRC-A !
   u SRC-U !
   p SRC-P !
   pu SRC-PU !
   t SRC-T !
   tu SRC-TU !
   owner SRC-OWNER !
   false PUBLISHABLE !
   false VERIFIED !
   [: CHECKED ;] catch {: code:n :}
   code 0<> if
      SB-RESET s" throw " SB-APPEND code FMT:SB-INT SB$ OUTCOME!
   then
   gather [: PROBED ;] catch {: rc:n :} drop
   LSP-DEFS:DEFS-UNPROBE
   LSP-DEFS:PROBE-DISK-CLEAR
   rc 0<> if rc throw then ;

\ Primitive references can check a bounded group in one isolated child.
\ Each result is exposed as the usual single probe while GATHER runs.
: USES-BATCH-BEGIN ( ptr u8 n -- )
   0 BATCH-CODE ! CHECK:VERIFY-REF-BATCH-BEGIN ;

: USES-BATCH-ADD ( ptr u8 n ptr u8 n -- )
   [: -1 swap DOC-EACH-BUT ;] CHECK:VERIFY-BATCH-ADD ;

: USES-BATCH-RUN ( -- )
   [: CHECK-MS >MS CHECK:VERIFY-BATCH-RUN ;] catch BATCH-CODE ! ;

: USES-BATCH-WAVE-BEGIN ( -- ) CHECK:VERIFY-BATCH-WAVE-BEGIN ;
: USES-BATCH-WAVE-ADD ( n -- ) CHECK:VERIFY-BATCH-WAVE-ADD ;
: USES-BATCH-WAVE-RUN ( n -- ) CHECK-MS >MS CHECK:VERIFY-BATCH-WAVE-RUN ;
: USES-BATCH-WAVE-LOAD ( n -- ) CHECK:VERIFY-BATCH-WAVE-LOAD BATCH-CODE ! ;

: USES-BATCH-N ( -- n )  CHECK:VERIFY-BATCH-N ;

: USES-BATCH-CLEAN? ( -- bool )
   BATCH-CODE @ 0<> if false exit then
   CHECK:VERIFY-BATCH-CLEAN? ;

: USES-BATCH-CHECKED ( -- )
   BATCH-K @ CHECK:VERIFY-BATCH-RESULT USES-VERDICT ;

\ False leaves the subject untouched so the caller can retry it one-shot.
: USES-BATCH-RESULT ( n n [ -- ] -- bool )
   {: k:n owner:n gather :}
   false PUBLISHABLE !
   false VERIFIED !
   owner SRC-OWNER !
   BATCH-CODE @ 0<> if false exit then
   k BATCH-K !
   [: USES-BATCH-CHECKED ;] catch 0<> if false exit then
   CHECK:VERIFY-BATCH-COMPLETE? 0= if false exit then
   gather [: PROBED ;] catch {: rc:n :} drop
   LSP-DEFS:DEFS-UNPROBE
   LSP-DEFS:PROBE-DISK-CLEAR
   rc 0<> if rc throw then
   true ;

\ What the last USES check came to: its verdict's name, or `throw N`.
: OUTCOME$ ( -- ptr u8 n )  OUTCOME-B OUTCOME-U @ ;

;using
;package
