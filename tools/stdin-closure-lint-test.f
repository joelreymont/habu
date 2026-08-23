\ stdin-closure-lint-test.f - fixtures for the stdin driver closure drift gate.
\ Run: bin/hb --load tools/stdin-closure-lint-test.f
\
\ This gate had no fixture file at all. It carried a baked self-test that asked
\ its detector for one token it knew was present and one sentinel it knew was
\ absent - which proves a detector is not wired shut, and nothing about whether
\ it can tell a CALL from a MENTION. It could not: `CONTAINS?` over the whole
\ file answered yes to a name in a comment.
\
\ So the fixtures below are the two evasions measured on master, in unit form.
\ Deleting the real `SDC-DECL$ type space` from tools/srclist.f and leaving the
\ name in a `\` comment left the gate at 0 findings; commenting out
\ `cat src/habu/aot-arm.f` in tools/bootstrap.sh did too. Both now report.
\
\ Text carrying a `"`, a `\` or a line break is composed from named byte helpers
\ rather than an escaped literal, the idiom tools/error-code-lint-test.f uses.
\
\ Load after lib/test.f and tools/stdin-closure-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/source-lex.f
require tools/bootstrap-src-lib.f
require tools/stdin-closure-lib.f
require tools/stdin-closure-lint-core.f

package STDIN-CLOSURE-LINT-TEST
using STDIN-CLOSURE

private

$400 constant FIX-CAP

create FIX FIX-CAP allot
variable FIX-U

: FIX-RESET ( -- )
   0 FIX-U ! ;

: FIX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-TEST-CAPACITY throw then
   a FIX FIX-U @ + u LINT-BMOVE
   FIX-U @ u + FIX-U ! ;

: FIX-C+ ( n -- ) {: c:n :}
   FIX-U @ 1+ FIX-CAP > if E-TEST-CAPACITY throw then
   c FIX FIX-U @ + c!
   FIX-U @ 1+ FIX-U ! ;

: Q+ ( -- )  DQUOTE FIX-C+ ;
: EOL+ ( -- )  10 FIX-C+ ;

: FIX$ ( -- ptr u8 n )
   FIX FIX-U @ ;

: CALLS? ( ptr u8 n -- bool )              \ does the fixture call SDC-AOT$
   s" SDC-AOT$" STDIN-CLOSURE-LINT:NAMES? ;

\ ---- a call is a live WORD token ---------------------------------------------
: ACCEPTS ( -- )
   s" : F ( -- ) SDC-AOT$ type ;"          CALLS? TTRUE
   \ the dictionary is case-insensitive, so this really is the same call
   s" : F ( -- ) sdc-aot$ type ;"          CALLS? TTRUE
   \ a consumer that qualifies instead of importing with `using` still calls it
   s" : F ( -- ) STDIN-CLOSURE:SDC-AOT$ type ;" CALLS? TTRUE ;

\ ---- fixtures built to fool the detector -------------------------------------
\ Every one of these contains the accessor's name, so the CONTAINS? this lint
\ used to run answered TRUE to all of them.
: FOOL ( -- )
   \ THE MEASURED EVASION: the real call replaced by a hardcoded path, the name
   \ left in a `\` comment. Gate 17e exists to catch exactly this edit.
   FIX-RESET
   s" \ SDC-AOT$ names the capture source" FIX+ EOL+
   s" : F ( -- ) s" FIX+ Q+ s"  src/habu/aot-capture.f" FIX+ Q+ s"  type ;" FIX+
   FIX$ CALLS? TFALSE
   \ inside a ( ) stack comment
   s" : F ( SDC-AOT$ -- ) drop ;"          CALLS? TFALSE
   \ inside a .( ) printing comment
   s" .( SDC-AOT$ is the accessor )"       CALLS? TFALSE
   \ inside a string literal payload - a mention, not a call
   FIX-RESET s" : F ( -- ) s" FIX+ Q+ s"  SDC-AOT$" FIX+ Q+ s"  type ;" FIX+
   FIX$ CALLS? TFALSE
   \ a near miss is not the accessor
   s" : F ( -- ) SDC-AOT type ;"           CALLS? TFALSE
   s" : F ( -- ) XSDC-AOT$ type ;"         CALLS? TFALSE
   \ a bare `[char] \"` must not blind the scan to the call that follows it
   FIX-RESET s" [char] " FIX+ Q+ s"  drop : F ( -- ) SDC-AOT$ type ;" FIX+
   FIX$ CALLS? TTRUE
   \ nor may a `\` inside a string body swallow the rest of the line
   FIX-RESET s" : G ( -- ) ." FIX+ Q+ s"  a \ b" FIX+ Q+ s"  ; : F ( -- ) SDC-AOT$ type ;" FIX+
   FIX$ CALLS? TTRUE ;

\ ---- a path with no accessor must be EMITTED ---------------------------------
: EMITS? ( ptr u8 n -- bool )
   s" src/habu/driver-io.f" STDIN-CLOSURE-LINT:QUOTES? ;

: PATHS ( -- )
   \ one word of a literal payload, which is how srclist.f writes it
   FIX-RESET s" : F ( -- ) s" FIX+ Q+ s"  src/habu/driver-io.f " FIX+ Q+ s"  type ;" FIX+
   FIX$ EMITS? TTRUE
   \ the same path in a comment emits nothing
   s" \ then src/habu/driver-io.f goes out"   EMITS? TFALSE
   \ a bare code token is not emitted text
   s" : F ( -- ) src/habu/driver-io.f ;"      EMITS? TFALSE
   \ a payload WORD, not a substring of one
   FIX-RESET s" : F ( -- ) s" FIX+ Q+ s"  xsrc/habu/driver-io.f" FIX+ Q+ s"  type ;" FIX+
   FIX$ EMITS? TFALSE ;

\ ---- the launcher's stdin emission -------------------------------------------
: SCRIPT-HEAD ( -- )
   FIX-RESET
   s" SRC_COMMON=(" FIX+ EOL+
   s"  " FIX+ SDC-DECL$ FIX+ EOL+
   s"  " FIX+ SDC-IDENT$ FIX+ EOL+
   s" )" FIX+ EOL+
   s" emit_src() {" FIX+ EOL+
   s"   for f in " FIX+ Q+ s" ${SRC_COMMON[@]}" FIX+ Q+ s" ; do" FIX+ EOL+
   s"     cat " FIX+ Q+ s" $f" FIX+ Q+ s"  >> " FIX+ Q+ s" $out" FIX+ Q+ EOL+
   s"   done" FIX+ EOL+
   s"   if [[ " FIX+ Q+ s" $driver" FIX+ Q+ s"  == " FIX+ Q+ SDC-DRIVER$ FIX+ Q+ s"  ]]; then" FIX+ EOL+ ;

: SCRIPT-ARM ( -- )                        \ the row the B probe commented out
   s"     cat " FIX+ SDC-ARM$ FIX+ s"  >> " FIX+ Q+ s" $out" FIX+ Q+ EOL+ ;

: SCRIPT-ARM-COMMENTED ( -- )
   s"     # cat " FIX+ SDC-ARM$ FIX+ s"  >> " FIX+ Q+ s" $out" FIX+ Q+ EOL+ ;

: SCRIPT-TAIL ( -- )
   s"     cat " FIX+ SDC-AOT$ FIX+ s"  >> " FIX+ Q+ s" $out" FIX+ Q+ EOL+
   s"     cat " FIX+ SDC-FILE$ FIX+ s"  >> " FIX+ Q+ s" $out" FIX+ Q+ EOL+
   s"   fi" FIX+ EOL+
   s"   cat " FIX+ Q+ s" $driver" FIX+ Q+ s"  >> " FIX+ Q+ s" $out" FIX+ Q+ EOL+
   s" }" FIX+ EOL+
   s" emit_src " FIX+ Q+ s" $T/stage2-src" FIX+ Q+ s"  " FIX+ SDC-DRIVER$ FIX+ EOL+ ;

: WHOLE-SCRIPT ( -- ptr u8 n )
   SCRIPT-HEAD SCRIPT-ARM SCRIPT-TAIL FIX$ ;

: SCRIPT-MINUS-ARM ( -- ptr u8 n )
   SCRIPT-HEAD SCRIPT-ARM-COMMENTED SCRIPT-TAIL FIX$ ;

: EMISSION ( -- )
   \ a launcher that cats every SDC-HOST file, including the four the stdin
   \ conditional guards, is clean
   STDIN-CLOSURE-LINT:RESET
   s" fixture.sh" WHOLE-SCRIPT STDIN-CLOSURE-LINT:EMISSION-CK
   STDIN-CLOSURE-LINT:FINDINGS 0 T=
   \ THE MEASURED EVASION: comment one cat row out and the row is gone from the
   \ emission, though its path is still in the script text
   STDIN-CLOSURE-LINT:RESET
   s" fixture.sh" SCRIPT-MINUS-ARM STDIN-CLOSURE-LINT:EMISSION-CK
   STDIN-CLOSURE-LINT:FINDINGS 1 T= ;

: LIVE ( -- )
   \ the real consumers and the real launcher are clean (enforcing check)
   STDIN-CLOSURE-LINT:STRICT ;

: MAIN ( -- )
   T-RESET
   ACCEPTS
   FOOL
   PATHS
   EMISSION
   T-REPORT
   LIVE ;

public

EXPORT MAIN

;package

STDIN-CLOSURE-LINT-TEST:MAIN
