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
\ STORAGE CLASS. PROCESS-GLOBAL: the check in progress belongs to the server's
\ one task.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fd-io.f
require lib/process.f
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

\ The check of the document's text as the file its path names.
: VERIFY ( -- )
   SUBJECT @ {: slot:n :}
   slot DOC-TEXT$ slot DOC-PATH$ CURSOR @ CHECK-MS >MS CHECK:VERIFY-BYTES-AT
   MATCH CHECK:verdict
      verified OF true VERIFIED ! s" verified" COMPLETED ENDOF
      refused OF s" refused" COMPLETED ENDOF
      engine-provided OF s" engine-provided" COMPLETED ENDOF
      held OF HEAD s" not checked: the verifier's image holds it" ERR NEWLINE RELAY ENDOF
      incomplete OF UNFINISHED ENDOF
      deferred OF s" deferred" COMPLETED ENDOF
   ;MATCH ;

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
      CHECK:VERIFY-FILES$ CHECK:VERIFY-DEFS$ CHECK:VERIFY-USES$ slot
      VERIFIED @ LSP-DEFS:DEFS-KEEP
   then
   CHECK:VERIFY-OUT$ nip 0<> PUBLISHABLE @ or if
      slot CHECK:VERIFY-OUT$ LSP-DIAG:PUBLISH
   then ;

\ Checks the document in this slot, with no cursor, and publishes what its
\ check found.
: RUN ( n -- )  -1 RUN-AT ;

;using
;package
