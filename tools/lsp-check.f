\ lsp-check.f - the language server's check of one open document, and the one
\ place the server meets the checker.
\
\ RUN checks a document's text as the file its URI names, in that file's load
\ context, through CHECK:VERIFY-BYTES-AT (tools/check-verify-core.f), which runs
\ none of it, and hands the packets the check wrote to LSP-DIAG:PUBLISH whatever
\ its verdict: a check stopped by a later definition still publishes the
\ packets written before it. A check that completed - the verdict verified,
\ refused or deferred, or the engine providing the file, which leaves nothing
\ to verify - keeps its definitions and uses, and whether its verdict was
\ verified, in place of the last (LSP-DEFS:DEFS-KEEP) and publishes even when
\ it wrote none. One that did not - the verifier's image holding the file, the
\ verifier ending without a verdict, or a throw - keeps the document's last
\ definitions, which workspace symbols go on listing, but none of its uses: RUN
\ drops them as it starts (LSP-DEFS:DEFS-USES-DROP), since a use is bytes of
\ the text a check completed on, and go to definition finds none until a check
\ of the document completes. When it wrote no packets it publishes nothing, so
\ the client keeps the last list it was sent. One stderr line says why it did
\ not complete:
\
\    lsp: URI: not checked: exit N | signal N | deadline passed
\    lsp: URI: not checked: the verifier's image holds it
\    lsp: URI: not checked: throw N
\
\ The verifier's prose follows that line verbatim, and follows a line naming
\ the verdict of a check that completed whenever it has any.
\
\ A check that completed also keeps the loads the verifier reported of the
\ document's top level (CHECK:VERIFY-LOADS$), which its links are answered
\ from (tools/lsp-links.f), in place of the last (LSP-DOCS:DOC-LOADS!). RUN
\ drops the last ones as it starts, as it drops the uses, since a load's
\ bytes are of the text a check completed on: a document whose last check did
\ not complete has none.
\
\ RUN-AT is RUN with a completion cursor at a byte of the text: the same check,
\ publishing and keeping what RUN's does, which leaves the spellings the
\ checker offers at that byte in CHECK:VERIFY-CANDIDATES$.
\
\ Every check reads each other document open in the server in place of that
\ file on disk, its text under the file's canonical path: RUN's and RUN-AT's
\ every open document but the one checked, USES's every open document.
\
\ USES checks a file for the uses of a word, as references gathers them: the
\ bytes given, an open document's text or the file on disk, as the file at
\ the path given, with no cursor, publishing nothing and writing nothing to
\ stderr. Its uses are every use the check binds to a located declaration in
\ the file and, in every other file the check reads, each one bound to a
\ declaration of the canonical tail given (CHECK:VERIFY-BYTES-OF). A check
\ that completed is the store's probe (LSP-DEFS:DEFS-PROBE) while the gather
\ given runs, with its actual verified flag; the store keeps no probe once
\ the gather returns or throws, the throw going on. An unfinished check
\ contributes no probe. OUTCOME$ names what the check came to: its verdict's
\ name, or `throw N` when it threw.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the check in progress belongs to the server's
\ one task.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd-io.f
require src/lib/process.f
require tools/check-verify-core.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-diag.f

package LSP-CHECK
using LSP-DOCS

private

\ How long the verifier may take, a deadlock guard and nothing else: a check
\ measured 43 ms for one definition and 433 ms for tools/check-core.f's whole
\ closure (2026-10-01), as long as check-verify-test's guard.
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
   2 >FD a u FD-IO:WRITE-FULL ;

\ A number's decimal text, in SB.
: NUMBER$ ( n -- ptr u8 n )  SB-RESET FMT:SB-INT SB$ ;

\ The start of the line about the document: `lsp: URI: `.
: HEAD ( -- )
   s" lsp: " ERR
   SUBJECT @ DOC-URI$ ERR
   s" : " ERR ;

: NEWLINE ( -- )  s\" \n" ERR ;

\ The verifier's prose, ended by LF.
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

\ Gives EACH every open document but the one being checked.
: OTHERS ( [ ptr u8 n ptr u8 n -- ] -- )  SUBJECT @ swap DOC-EACH-BUT ;

\ The check of the document's text as the file its path names.
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

\ A completed check's lines as the probe, then GATHER.
: PROBED ( [ -- ] -- [ -- ] )
   PUBLISHABLE @ if
      CHECK:VERIFY-FILES$ CHECK:VERIFY-DEFS$ CHECK:VERIFY-USES$ VERIFIED @
      SRC-OWNER @
      LSP-DEFS:DEFS-PROBE
   then
   dup execute ;

public

\ Checks the document in this slot, its cursor at this byte of its text, none
\ when it is negative, and publishes what its check found. The document is
\ clean from here on, whatever the check comes to, and has no uses or loads
\ unless the check completes.
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

\ Checks the document in this slot, with no cursor, and publishes what its
\ check found.
: RUN ( n -- )  -1 RUN-AT ;

\ Checks these bytes as the file at this path for the uses of the word with
\ this canonical tail, and runs GATHER with the check as the store's probe
\ when it completed, removing the probe once GATHER returns or
\ throws.
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

\ What the last check USES ran came to.
: OUTCOME$ ( -- ptr u8 n )  OUTCOME-B OUTCOME-U @ ;

;using
;package
