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
\ STEP-ARTIFACT$ readers, STEP-RC@ / STEP-RC!, BUILD:STEP-VALIDATE, and
\ BUILD:STEP-RUN. The source scanner, the record cell plumbing, the CHECK! trust
\ boundary (BUILD-CHECK-RAW), and every buffer and state variable are
\ package-private.
\
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/process.f

package BUILD

65536 constant BUILD-SOURCE-CAP
10 constant BUILD-LF
13 constant BUILD-CR
32 constant BUILD-SP
58 constant BUILD-COLON
59 constant BUILD-SEMI

0 constant BUILD-STEP-NAME-A
1 constant BUILD-STEP-NAME-U
2 constant BUILD-STEP-CMD-A
3 constant BUILD-STEP-CMD-U
4 constant BUILD-STEP-ARGV-A
5 constant BUILD-STEP-ARGV-U
6 constant BUILD-STEP-TMP-A
7 constant BUILD-STEP-TMP-U
8 constant BUILD-STEP-ART-A
9 constant BUILD-STEP-ART-U
10 constant BUILD-STEP-RC-OFF
11 constant BUILD-STEP-CELLS

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

: BUILD-STEP-CHECK-OFF ( n -- ) {: off :}
   off 0 < if E-BUILD-PATH throw then
   off BUILD-STEP-CELLS >= if E-BUILD-PATH throw then ;

: BUILD-STEP-FIELD ( ptr a n -- ptr a ) {: rec:ptr off :}
   off BUILD-STEP-CHECK-OFF
   rec off cells + ;

: BUILD-STEP-A! ( ptr u8 ptr a n -- ) {: a:ptr rec:ptr off :}
   a rec off BUILD-STEP-FIELD ! ;

: BUILD-STEP-N! ( n ptr a n -- ) {: n rec:ptr off :}
   n rec off BUILD-STEP-FIELD ! ;

: BUILD-STEP-A@ ( ptr a n -- ptr u8 )
   BUILD-STEP-FIELD @ ;

: BUILD-STEP-N@ ( ptr a n -- n )
   BUILD-STEP-FIELD @ ;

: BUILD-STEP-PAIR! ( ptr u8 n ptr a n -- ) {: a:ptr u rec:ptr off :}
   u 0 < if E-BUILD-PATH throw then
   a rec off BUILD-STEP-A!
   u rec off 1 + BUILD-STEP-N! ;

: BUILD-STEP-PAIR$ ( ptr a n -- ptr u8 n ) {: rec:ptr off :}
   rec off BUILD-STEP-A@
   rec off 1 + BUILD-STEP-N@ ;

: BUILD-STEP-EMPTY! ( ptr a n -- ) {: rec:ptr off :}
   BUILD-SOURCE-BUF 0 rec off BUILD-STEP-PAIR! ;

public

\ Cell count of a caller-owned build-step record, so external callers can size
\ their own `create ... cells allot` storage before STEP-CLEAR.
: STEP-CELLS ( -- n )
   BUILD-STEP-CELLS ;

: STEP-CLEAR ( ptr a -- ) {: rec:ptr :}
   rec BUILD-STEP-NAME-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-CMD-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-ARGV-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-TMP-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-ART-A BUILD-STEP-EMPTY!
   -1 rec BUILD-STEP-RC-OFF BUILD-STEP-FIELD ! ;

: STEP-NAME! ( ptr u8 n ptr a -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-COMMAND throw then
   a u rec BUILD-STEP-NAME-A BUILD-STEP-PAIR! ;

: STEP-COMMAND! ( ptr u8 n ptr a -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-COMMAND throw then
   a u rec BUILD-STEP-CMD-A BUILD-STEP-PAIR! ;

: STEP-ARGV! ( ptr u8 n ptr a -- ) {: a:ptr u:n rec:ptr :}
   a u rec BUILD-STEP-ARGV-A BUILD-STEP-PAIR! ;

: STEP-TMP! ( ptr u8 n ptr a -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-PATH throw then
   a u rec BUILD-STEP-TMP-A BUILD-STEP-PAIR! ;

: STEP-ARTIFACT! ( ptr u8 n ptr a -- ) {: a:ptr u:n rec:ptr :}
   u 0 <= if E-BUILD-PATH throw then
   a u rec BUILD-STEP-ART-A BUILD-STEP-PAIR! ;

: STEP-NAME$ ( ptr a -- ptr u8 n )
   BUILD-STEP-NAME-A BUILD-STEP-PAIR$ ;

: STEP-COMMAND$ ( ptr a -- ptr u8 n )
   BUILD-STEP-CMD-A BUILD-STEP-PAIR$ ;

: STEP-ARGV$ ( ptr a -- ptr u8 n )
   BUILD-STEP-ARGV-A BUILD-STEP-PAIR$ ;

: STEP-TMP$ ( ptr a -- ptr u8 n )
   BUILD-STEP-TMP-A BUILD-STEP-PAIR$ ;

: STEP-ARTIFACT$ ( ptr a -- ptr u8 n )
   BUILD-STEP-ART-A BUILD-STEP-PAIR$ ;

: STEP-RC@ ( ptr a -- n )
   BUILD-STEP-RC-OFF BUILD-STEP-FIELD @ ;

: STEP-RC! ( n ptr a -- ) {: rc:n rec:ptr :}
   rc rec BUILD-STEP-RC-OFF BUILD-STEP-N! ;

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

\ CHECK! cannot recursively certify the definition that invokes it.
\ Retirement owner: habu-primitive-effect-axiom-1119f176.
TRUSTED: BUILD-CHECK-RAW ( ptr u8 n -- n )
   CHECK! ;

: BUILD-CHECK-ONE ( n n -- ) {: start finish :}
   finish start <= if E-BUILD-SOURCE throw then
   BUILD-SOURCE-BUF start + finish start - BUILD-CHECK-RAW -1 <> if
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

\ typed-local-lint: allow-bare-local - q is the step's checked action quotation.
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

: STEP-VALIDATE ( ptr a -- ) {: rec:ptr :}
   rec STEP-NAME$ nip 0 <= if E-BUILD-COMMAND throw then
   rec STEP-COMMAND$ FILE? 0= if E-BUILD-COMMAND throw then
   rec STEP-TMP$ DIR? 0= if E-BUILD-PATH throw then
   rec STEP-ARTIFACT$ nip 0 <= if E-BUILD-PATH throw then ;

: STEP-RUN ( ptr a -- n ) {: rec:ptr :}
   rec STEP-VALIDATE
   rec STEP-COMMAND$ rec STEP-ARTIFACT$ RUN {: rc:n :}
   rc rec STEP-RC!
   rc ;

;package
