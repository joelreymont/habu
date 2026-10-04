\ lsp-check.f - the language server's check of one open document, and the one
\ place the server meets the checker.
\
\ RUN checks a document's text as the file its URI names, in that file's load
\ context, through CHECK:VERIFY-BYTES (tools/check-verify-core.f), which runs
\ none of it. A check that completed - the verdict verified, refused or
\ deferred, or the engine providing the file, which leaves nothing to verify -
\ hands its packets to LSP-DIAG:PUBLISH. One that did not - the verifier's
\ image holding the file, the verifier ending without a verdict, or a throw -
\ publishes nothing, so the client keeps the last list it was sent, and one
\ stderr line says why:
\
\    lsp: URI: not checked: exit N | signal N | deadline passed
\    lsp: URI: not checked: the verifier's image holds it
\    lsp: URI: not checked: throw N
\
\ The verifier's prose follows that line verbatim, and follows a line naming
\ the verdict of a check that completed whenever it has any.
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
variable PUBLISHABLE                     \ whether its check completed

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
   slot DOC-TEXT$ slot DOC-PATH$ CHECK-MS >MS CHECK:VERIFY-BYTES
   MATCH CHECK:verdict
      verified OF s" verified" COMPLETED ENDOF
      refused OF s" refused" COMPLETED ENDOF
      engine-provided OF s" engine-provided" COMPLETED ENDOF
      held OF HEAD s" not checked: the verifier's image holds it" ERR NEWLINE RELAY ENDOF
      incomplete OF UNFINISHED ENDOF
      deferred OF s" deferred" COMPLETED ENDOF
   ;MATCH ;

public

\ Checks the document in this slot and publishes what its check found. The
\ document is clean from here on, whatever the check comes to.
: RUN ( n -- )
   {: slot:n :}
   slot DOC-CLEAN
   slot SUBJECT !
   false PUBLISHABLE !
   [: VERIFY ;] catch {: code:n :}
   code 0<> if
      HEAD s" not checked: throw " ERR code NUMBER$ ERR NEWLINE RELAY
      exit
   then
   PUBLISHABLE @ if slot CHECK:VERIFY-OUT$ LSP-DIAG:PUBLISH then ;

;using
;package
