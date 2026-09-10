\ engine-error-package.f - behavioral engine failure ABI regressions.
\ Native gates run it against HABU_UNDER_TEST; bootstrap.sh runs the same file
\ against the Gforth-recovered candidate. Besides exact exits 86..88, it proves
\ the post-seal checker bridge succeeds, then patches the sole embedded
\ checker-package lookup token and proves the same source fails closed with 70
\ AND names the refused state on fd 2 - the status alone cannot distinguish a
\ named refusal from any other rc-70 reject.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f

package ENGINE-ERROR-TEST
private

$800 constant CAP
10000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot

variable IMAGE-A
variable IMAGE-U
variable MATCH-OFF
variable MATCH-N
variable ROOT-U
variable PATCHED-U
variable OUT-U         \ bytes the last child wrote to fd 1
variable ERR-U         \ bytes the last child wrote to fd 2

create ROOT-BUF FS-PATH-CAP allot
create PATCHED-BUF FS-PATH-CAP allot

: IMAGE-A-FIELD ( -- ptr ptr u8 )
   IMAGE-A 0 ptr-field ;

: IMAGE ( -- ptr u8 )
   IMAGE-A-FIELD @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: PATCHED$ ( -- ptr u8 n )
   PATCHED-BUF PATCHED-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: SOURCE$ ( ptr u8 n -- ptr u8 n ) {: name:ptr u:n :}
   SB-RESET
   S\" s\" engine-error-test\" " SB-APPEND
   name u SB-APPEND
   s"  die" SB-APPEND
   SB$ ;

: RUN-SOURCE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   exe exeu >LEN src srcu >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N OUT-U ! e LEN>N ERR-U ! 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N OUT-U ! e LEN>N ERR-U ! c RC>N ENDOF
   ;MATCH ;

: CHILD-RC ( ptr u8 n -- n )
   SOURCE$ {: src:ptr u:n :}
   HB$ src u RUN-SOURCE ;

: PACKAGE-SOURCE$ ( -- ptr u8 n )
   s" package BRIDGE public : W ( -- n ) 1 ; ;package BRIDGE:W drop" ;

: PACKAGE-RC ( -- n )
   PACKAGE-SOURCE$ {: src:ptr u:n :}
   HB$ src u RUN-SOURCE ;

: COPY-PATH ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-engine-error" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY-PATH
   ROOT$ CLEANUP-TREE+
   ROOT$ s" missing-checker" PATCHED-BUF JOIN-PATH PATCHED-U ! ;

: LOAD-IMAGE ( -- )
   HB$ FILE-SIZE {: u:n :}
   u IMAGE-U !
   u MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop IMAGE-A-FIELD !
   HB$ IMAGE u READ-ALL u <> if
      s" engine-error-test: short engine read" 1 die
   then ;

: NAME-AT? ( n -- bool ) {: off:n :}
   IMAGE off BYTE+ 15 s" checker-package" STR= ;

: SCAN-NAME ( -- )
   0 MATCH-N !
   0 MATCH-OFF !
   0 begin dup 15 + IMAGE-U @ <= while
      dup NAME-AT? if
         dup MATCH-OFF !
         MATCH-N @ 1 + MATCH-N !
      then
      1+
   repeat drop
   MATCH-N @ 1 <> if
      s" engine-error-test: checker-package occurrence count" 1 die
   then ;

: PATCH-IMAGE ( -- )
   [char] x IMAGE MATCH-OFF @ BYTE+ c!
   PATCHED$ IMAGE IMAGE-U @ WRITE-ALL
   PATCHED$ CHMOD-X
   PATCHED$ CODESIGN:FORCE ;

: MISSING-CHECKER-RC ( -- n )
   PACKAGE-SOURCE$ {: src:ptr u:n :}
   PATCHED$ src u RUN-SOURCE ;

: MISSING-DEFINITION-NAME ( -- )
   s" a lone colon rejects at the engine reader" T-LABEL
   HB$ s" :" RUN-SOURCE $4A T=
   ERR$ s" missing definition name" CONTAINS? TTRUE ;

: EXACT-EXITS ( -- )
   s" callable ABI exits 86" T-LABEL
   s" ENGINE-ERROR:CALLABLE-ABI" CHILD-RC 86 T=
   s" CATCH stack exits 87" T-LABEL
   s" ENGINE-ERROR:CATCH-STACK" CHILD-RC 87 T=
   s" code certificate exits 88" T-LABEL
   s" ENGINE-ERROR:CODE-CERT" CHILD-RC 88 T= ;

: COMPILE-KEYWORD-SOURCE$ ( ptr u8 n -- ptr u8 n ) {: tok:ptr toku:n :}
   SB-RESET
   s" package KW-NAME private : " SB-APPEND
   tok toku SB-APPEND
   S\"  ( -- ) ; s\" AFTER\" type ;package" SB-APPEND
   SB$ ;

: COMPILE-KEYWORD-REJECT ( ptr u8 n -- ) {: tok:ptr toku:n :}
   tok toku COMPILE-KEYWORD-SOURCE$ {: src:ptr srcu:n :}
   s" compile keyword definition rejects at its name" T-LABEL
   HB$ src srcu RUN-SOURCE 70 T=
   s" compile keyword definition names the constraint" T-LABEL
   ERR$ s" compile keyword cannot be a definition name: " CONTAINS? TTRUE
   s" compile keyword definition stops before later source" T-LABEL
   OUT$ s" AFTER" CONTAINS? 0= TTRUE ;

: COMPILE-KEYWORD-DEFINITION ( -- )
   \ These are the complete control and loop rows consumed before dictionary
   \ lookup. All 28 used to publish silently. EXIT, RECURSE, UNLOOP, and {:
   \ were especially deceptive: a later same-name use could compile rc 0 while
   \ executing syntax semantics instead of the definition.
   s" if" COMPILE-KEYWORD-REJECT       s" then" COMPILE-KEYWORD-REJECT
   s" else" COMPILE-KEYWORD-REJECT     s" begin" COMPILE-KEYWORD-REJECT
   s" until" COMPILE-KEYWORD-REJECT    s" again" COMPILE-KEYWORD-REJECT
   s" while" COMPILE-KEYWORD-REJECT    s" repeat" COMPILE-KEYWORD-REJECT
   s" case" COMPILE-KEYWORD-REJECT     s" of" COMPILE-KEYWORD-REJECT
   s" endof" COMPILE-KEYWORD-REJECT    s" endcase" COMPILE-KEYWORD-REJECT
   s" construct" COMPILE-KEYWORD-REJECT s" match" COMPILE-KEYWORD-REJECT
   s" do" COMPILE-KEYWORD-REJECT       s" loop" COMPILE-KEYWORD-REJECT
   s" i" COMPILE-KEYWORD-REJECT        s" >r" COMPILE-KEYWORD-REJECT
   s" r>" COMPILE-KEYWORD-REJECT       s" r@" COMPILE-KEYWORD-REJECT
   s" exit" COMPILE-KEYWORD-REJECT     s" recurse" COMPILE-KEYWORD-REJECT
   s" ?do" COMPILE-KEYWORD-REJECT      s" +loop" COMPILE-KEYWORD-REJECT
   s" j" COMPILE-KEYWORD-REJECT        s" leave" COMPILE-KEYWORD-REJECT
   s" unloop" COMPILE-KEYWORD-REJECT   s" {:" COMPILE-KEYWORD-REJECT
   \ CASE is the minimal wrong-token reproducer: the diagnostic must include the
   \ guilty spelling itself, not a later body token where syntax finally trips.
   s" compile keyword definition names CASE" T-LABEL
   HB$ S\" package KW-UPPER private : CASE ( -- ) ; s\" AFTER\" type ;package"
      RUN-SOURCE 70 T=
   ERR$ s" compile keyword cannot be a definition name: CASE" CONTAINS? TTRUE
   s" qualified compile keyword definition rejects at its tail" T-LABEL
   HB$ s" : KW-QUAL:CASE ( -- ) ;" RUN-SOURCE 70 T=
   ERR$ s" compile keyword cannot be a definition name: CASE" CONTAINS? TTRUE
   \ FOLD is a normal, non-immediate dictionary word, not engine syntax. Its
   \ same-name private definition must remain callable through ordinary lookup.
   s" ordinary FOLD definition remains callable" T-LABEL
   HB$ s" package KW-FOLD private : FOLD ( n -- n ) 1+ ; : CALL-FOLD ( -- n ) 41 FOLD ; CALL-FOLD . ;package"
      RUN-SOURCE 0 T=
   s" ordinary FOLD definition returns through dictionary lookup" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE
   s" qualified ordinary FOLD definition remains callable" T-LABEL
   HB$ s" : KW-FOLD-Q:FOLD ( n -- n ) 1+ ; 41 KW-FOLD-Q:FOLD ." RUN-SOURCE 0 T=
   OUT$ s" 42" CONTAINS? TTRUE ;

: POST-SEAL-BRIDGE ( -- )
   s" post-seal package reaches checker bridge" T-LABEL
   PACKAGE-RC 0 T=
   PREPARE
   LOAD-IMAGE
   SCAN-NAME
   PATCH-IMAGE
   s" checking-off permits a pre-checker package reload" T-LABEL
   PATCHED$ s" 0 set-check package CHECK-OFF public : W ( -- n ) 1 ; ;package" RUN-SOURCE 0 T=
   s" post-seal missing checker fails closed" T-LABEL
   MISSING-CHECKER-RC 70 T=
   \ The exit status alone cannot tell a named refusal from a lucky one: 70 is
   \ also what an undefined word or a rejected body exits with. So the same run's
   \ fd 2 has to name the state the checker refused on, which is what turns the
   \ bare `hb: uncaught throw code 7136` (rc 67) this test used to get into a
   \ diagnostic a reader can act on.
   s" post-seal missing checker names the refused state" T-LABEL
   ERR$ s" no authenticated package context" CONTAINS? TTRUE
   CLEANUP-RUN ;

public

: RUN ( -- )
   T-RESET
   EXACT-EXITS
   MISSING-DEFINITION-NAME
   COMPILE-KEYWORD-DEFINITION
   POST-SEAL-BRIDGE
   T-REPORT
   s" engine-error-package: ok" type cr ;

;package

ENGINE-ERROR-TEST:RUN
