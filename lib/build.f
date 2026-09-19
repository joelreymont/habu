\ build.f - checked helpers for Habu build scripts.
\
\ The module lives in `package BUILD`. A build script drives it through the
\ qualified public API: BUILD:CHECK certifies every checked definition in a
\ counted source file, BUILD:ARTIFACT builds a bounded artifact path under a
\ root, BUILD:RUN runs a counted command and requires its expected artifact, and
\ BUILD:STEP runs one checked step quotation and throws on a nonzero result code.
\ A build-step record is assembled and executed through BUILD:STEP-CLEAR, the
\ BUILD:STEP-NAME! / STEP-COMMAND! / STEP-ARGV! / STEP-TMP! / STEP-ARTIFACT!
\ setters, the matching STEP-NAME$ / STEP-COMMAND$ / STEP-ARGV$ / STEP-TMP$ /
\ STEP-ARTIFACT$ readers, STEP-STATE@ / STEP-RC@ / STEP-RC!, STEP-VALIDATE,
\ and STEP-RUN. Input setters invalidate the preceding result; refused edits
\ leave it intact. The source scanner, buffers and state variables are private.
\
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/process.f

package BUILD

public

STRUCTURE text 0 FIELD base ptr u8 FIELD size len ;STRUCTURE
ENUM state 0
   VARIANT pending ;VARIANT
   VARIANT completed FIELD code rc ;VARIANT
;ENUM
STRUCTURE step 0 DERIVE addr
   FIELD name text
   FIELD command text
   FIELD argv text
   FIELD tmp text
   FIELD artifact text
   FIELD state state
;STRUCTURE

private

65536 constant BUILD-SOURCE-CAP
10 constant BUILD-LF
13 constant BUILD-CR
32 constant BUILD-SP
58 constant BUILD-COLON
59 constant BUILD-SEMI

create BUILD-SOURCE-BUF BUILD-SOURCE-CAP allot
create BUILD-PATH-BUF FS-PATH-CAP allot

variable BUILD-SOURCE-LEN
variable BUILD-I
variable BUILD-DEFS
variable BUILD-START
variable BUILD-END

: BUILD-FALSE ( -- bool )
   0 0= 0= ;

: BUILD-TRUE ( -- bool )
   0 0= ;

: BUILD-WHITE? ( n -- bool ) {: c :}
   c BUILD-SP = if BUILD-TRUE exit then
   c BUILD-LF = if BUILD-TRUE exit then
   c BUILD-CR = ;

: BUILD-LENGTH ( n -- len )
   dup 0 < if E-BUILD-PATH throw then >LEN ;

: BUILD-EMPTY ( -- ptr u8 len ) BUILD-SOURCE-BUF 0 >LEN ;

: BUILD-PENDING ( ptr step -- )
   BUILD-STATE:pending swap BUILD-STEP:STATE ! ;

public

\ Caller-owned storage is TYPED-VARIABLE / TYPED-BUFFER of BUILD:step.
: STEP-CELLS ( -- n ) BUILD-STEP:CELLS ;

: STEP-CLEAR ( ptr step -- ) {: rec:ptr :}
   BUILD-EMPTY BUILD-TEXT:MAKE
   BUILD-EMPTY BUILD-TEXT:MAKE
   BUILD-EMPTY BUILD-TEXT:MAKE
   BUILD-EMPTY BUILD-TEXT:MAKE
   BUILD-EMPTY BUILD-TEXT:MAKE
   BUILD-STATE:pending BUILD-STEP:MAKE rec ! ;

: STEP-NAME! ( ptr u8 n ptr step -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-COMMAND throw then
   a u BUILD-LENGTH BUILD-TEXT:MAKE rec BUILD-STEP:NAME !
   rec BUILD-PENDING ;

: STEP-COMMAND! ( ptr u8 n ptr step -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-COMMAND throw then
   a u BUILD-LENGTH BUILD-TEXT:MAKE rec BUILD-STEP:COMMAND !
   rec BUILD-PENDING ;

: STEP-ARGV! ( ptr u8 n ptr step -- ) {: a:ptr u:n rec:ptr :}
   a u BUILD-LENGTH BUILD-TEXT:MAKE rec BUILD-STEP:ARGV !
   rec BUILD-PENDING ;

: STEP-TMP! ( ptr u8 n ptr step -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-PATH throw then
   a u BUILD-LENGTH BUILD-TEXT:MAKE rec BUILD-STEP:TMP !
   rec BUILD-PENDING ;

: STEP-ARTIFACT! ( ptr u8 n ptr step -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-PATH throw then
   a u BUILD-LENGTH BUILD-TEXT:MAKE rec BUILD-STEP:ARTIFACT !
   rec BUILD-PENDING ;

: STEP-NAME$ ( ptr step -- ptr u8 n )
   BUILD-STEP:NAME @ BUILD-TEXT:UNMAKE LEN>N ;

: STEP-COMMAND$ ( ptr step -- ptr u8 n )
   BUILD-STEP:COMMAND @ BUILD-TEXT:UNMAKE LEN>N ;

: STEP-ARGV$ ( ptr step -- ptr u8 n )
   BUILD-STEP:ARGV @ BUILD-TEXT:UNMAKE LEN>N ;

: STEP-TMP$ ( ptr step -- ptr u8 n )
   BUILD-STEP:TMP @ BUILD-TEXT:UNMAKE LEN>N ;

: STEP-ARTIFACT$ ( ptr step -- ptr u8 n )
   BUILD-STEP:ARTIFACT @ BUILD-TEXT:UNMAKE LEN>N ;

: STEP-STATE@ ( ptr step -- state ) BUILD-STEP:STATE @ ;

: STEP-RC@ ( ptr step -- n )
   STEP-STATE@ MATCH state
      pending OF -1 ENDOF
      completed OF RC>N ENDOF
   ;MATCH ;

\ The public numeric API retains -1 as its pending spelling; storage does not.
: STEP-RC! ( n ptr step -- ) {: code:n rec:ptr :}
   code -1 = if rec BUILD-PENDING exit then
   code >RC BUILD-STATE:completed rec BUILD-STEP:STATE ! ;

private

: BUILD-FIND-CHAR ( n n -- n ) {: start ch :}
   start begin dup BUILD-SOURCE-LEN @ < while
      dup BUILD-SOURCE-BUF + c@ ch = if exit then
      1+
   repeat drop -1 ;

: BUILD-SKIP-WHITE ( n -- n )
   begin dup BUILD-SOURCE-LEN @ < while
      dup BUILD-SOURCE-BUF + c@ BUILD-WHITE? if
         1+
      else
         exit
      then
   repeat ;

: BUILD-CHECK-ONE ( n n -- ) {: start finish :}
   finish start <= if E-BUILD-SOURCE throw then
   BUILD-SOURCE-BUF start + finish start - CHECK! -1 <> if
      E-BUILD-SOURCE throw
   then ;

: BUILD-READ-SOURCE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-SOURCE throw then
   a u FILE? 0= if E-BUILD-SOURCE throw then
   a u BUILD-SOURCE-BUF BUILD-SOURCE-CAP READ-ALL
   dup 0 <= if E-BUILD-SOURCE throw then
   BUILD-SOURCE-LEN ! ;

: BUILD-CHECK-NEXT ( n -- n ) {: start :}
   start BUILD-COLON BUILD-FIND-CHAR dup 0 < if exit then
   1+ BUILD-SKIP-WHITE BUILD-START !
   BUILD-START @ BUILD-SEMI BUILD-FIND-CHAR dup 0 < if E-BUILD-SOURCE throw then
   BUILD-END !
   BUILD-START @ BUILD-END @ BUILD-CHECK-ONE
   BUILD-DEFS @ 1+ BUILD-DEFS !
   BUILD-END @ 1+ ;

public

: CHECK ( ptr u8 n -- )
   BUILD-READ-SOURCE
   0 BUILD-DEFS !
   0 BUILD-I !
   begin BUILD-I @ BUILD-SOURCE-LEN @ < while
      BUILD-I @ BUILD-CHECK-NEXT dup 0 < if
         drop BUILD-SOURCE-LEN @ BUILD-I !
      else
         BUILD-I !
      then
   repeat
   BUILD-DEFS @ 0= if E-BUILD-SOURCE throw then ;

private

: BUILD-EXPECT ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FILE? 0= if E-BUILD-PATH throw then ;

public

: ARTIFACT ( ptr u8 n ptr u8 n -- ptr u8 n ) {: root:ptr rootu:n name:ptr nameu:n :}
   rootu 0 <= if E-BUILD-PATH throw then
   nameu 0 <= if E-BUILD-PATH throw then
   rootu 1 + nameu + FS-PATH-CAP > if E-BUILD-PATH throw then
   root rootu name nameu BUILD-PATH-BUF JOIN-PATH
   BUILD-PATH-BUF swap ;

: STEP ( ptr u8 n [ -- n ] -- ) {: name:ptr nameu:n q :}
   nameu 0 <= if E-BUILD-COMMAND throw then
   q execute {: rc:n :}
   rc 0 <> if E-BUILD-STATUS throw then ;

private

\ A build command is an artifact producer; it must never inherit the caller's
\ stdin. If it did (e.g. a gate pool worker's open, never-EOF pipe), a `bin/hb`
\ command script that falls into the stdin REPL after its body would block the
\ whole build. Give every build child /dev/null so the REPL sees immediate EOF.
: BUILD-DEV-NULL-RD ( -- fd )
   s" /dev/null" FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then >FD ;

public

: RUN ( ptr u8 n ptr u8 n -- n ) {: cmd:ptr cmdu:n artifact:ptr artifactu:n :}
   cmdu 0 <= if E-BUILD-COMMAND throw then
   cmd cmdu FILE? 0= if E-BUILD-COMMAND throw then
   BUILD-DEV-NULL-RD {: nullfd:fd :}
   cmd cmdu >LEN nullfd -1 >FD -1 >FD PROC-RUN-IO-RC   \ result<n,n> on the stack
   nullfd FD>N close                                    \ close nullfd; the result stays below
   MATCH result
     ok  OF ENDOF                                       \ clean exit: rc 0 left on the stack
     err OF drop E-BUILD-STATUS throw ENDOF              \ nonzero exit / signal -> build failure
   ;MATCH {: rc:n :}
   artifact artifactu BUILD-EXPECT
   rc ;

: STEP-VALIDATE ( ptr step -- ) {: rec:ptr :}
   rec STEP-NAME$ nip 0 <= if E-BUILD-COMMAND throw then
   rec STEP-COMMAND$ FILE? 0= if E-BUILD-COMMAND throw then
   rec STEP-TMP$ DIR? 0= if E-BUILD-PATH throw then
   rec STEP-ARTIFACT$ nip 0 <= if E-BUILD-PATH throw then ;

: STEP-RUN ( ptr step -- n ) {: rec:ptr :}
   rec STEP-VALIDATE
   rec BUILD-PENDING
   rec STEP-COMMAND$ rec STEP-ARTIFACT$ RUN {: rc:n :}
   rc rec STEP-RC!
   rc ;

;package
