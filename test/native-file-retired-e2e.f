\ A FILE unit keeps a retired dictionary row and the code its old caller uses.
\ bin/hb --load test/native-file-retired-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/native-file-app-lib.f

package NATIVE-FILE-RETIRED-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create AT-BUF FS-PATH-CAP allot
create APP-BUF FS-PATH-CAP allot
create PROD-BUF FS-PATH-CAP allot
create CACHE-BUF FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-BUF JOIN-PATH AT-BUF swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" unit.f"
   S\" package FILE-RETIRED\npublic\n: VALUE ( -- n ) 1 ;\n: OLD-CALLER ( -- n ) VALUE ;\nundefine VALUE\n: VALUE ( -- n ) 2 ;\n;package\ns\" retire-source-ran\" type cr\n" PUT
   s" app.f"
   S\" require unit.f\n: MAIN ( -- ) FILE-RETIRED:VALUE . FILE-RETIRED:OLD-CALLER . ;\n" PUT
   s" prefix.f"
   S\" package FILE-PREFIX-RETIRED\npublic\n: VALUE ( -- n ) 1 ;\n: OLD-CALLER ( -- n ) VALUE ;\n;package\n" PUT
   s" drop.f"
   S\" require prefix.f\npackage FILE-PREFIX-RETIRED\npublic\nundefine VALUE\n;package\ns\" prefix-retire-source-ran\" type cr\n" PUT
   s" absent-app.f"
   S\" require drop.f\n: MAIN ( -- ) s\" FILE-PREFIX-RETIRED:VALUE\" XREF-FIND XREF-FOUND? if 1 else 0 then . FILE-PREFIX-RETIRED:OLD-CALLER . ;\n" PUT
   s" late.f"
      S\" require prefix.f\npackage FILE-PREFIX-RETIRED\npublic\n: NEW-CALLER ( -- n ) VALUE ;\nundefine VALUE\n;package\ns\" late-retire-source-ran\" type cr\n" PUT
   s" late-app.f"
      S\" require late.f\n: MAIN ( -- ) FILE-PREFIX-RETIRED:NEW-CALLER . ;\n" PUT ;

: BUILD ( ptr u8 n ptr u8 n -- n )
   {: source:ptr sourceu:n product:ptr productu:n :}
   ROOT$ source sourceu APP-BUF JOIN-PATH APP-BUF swap
   ROOT$ product productu PROD-BUF JOIN-PATH PROD-BUF swap
   ROOT$ s" cache" CACHE-BUF JOIN-PATH CACHE-BUF swap
      NATIVE-FILE-APP:BUILD {: rc:n :}
   NATIVE-FILE-APP:OUT$ {: out:ptr outu:n :}
   NATIVE-FILE-APP:ERR$ {: err:ptr erru:n :}
   out OUT outu BYTE-COPY outu OUT-U !
   err ERR erru BYTE-COPY erru ERR-U !
   rc ;

: CAPTURE ( ptr u8 n -- n )
   {: exe:ptr exeu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   exe exeu >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: RUN-PRODUCT ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n expected:ptr expectedu:n :}
   product productu AT CAPTURE SUCCESS
   OUT OUT-U @ expected expectedu CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" retired FILE row keeps old calls and warm import" T-LABEL
   s" native-file-retired" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   PREPARE
   s" app.f" s" cold-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits 0" CONTAINS? TTRUE
   OUT OUT-U @ s" retire-source-ran" CONTAINS? TTRUE
   s" app.f" s" warm-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits " CONTAINS? TTRUE
   OUT OUT-U @ s" file unit hits 0" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" retire-source-ran" CONTAINS? 0= TTRUE
   s" cold-hb" S\" 2\n1\n" RUN-PRODUCT
   s" warm-hb" S\" 2\n1\n" RUN-PRODUCT
   s" absent-app.f" s" absent-cold-hb" BUILD SUCCESS
   OUT OUT-U @ s" prefix-retire-source-ran" CONTAINS? TTRUE
   s" absent-app.f" s" absent-warm-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits 3" CONTAINS? TTRUE
   OUT OUT-U @ s" compiled 0" CONTAINS? TTRUE
   OUT OUT-U @ s" prefix-retire-source-ran" CONTAINS? 0= TTRUE
   s" absent-cold-hb" S\" 0\n1\n" RUN-PRODUCT
   s" absent-warm-hb" S\" 0\n1\n" RUN-PRODUCT
   s" new caller keeps its retired target on warm import" T-LABEL
   s" late-app.f" s" late-cold-hb" BUILD SUCCESS
   OUT OUT-U @ s" late-retire-source-ran" CONTAINS? TTRUE
   s" late-app.f" s" late-warm-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits 0" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" late-retire-source-ran" CONTAINS? 0= TTRUE
   s" late-cold-hb" S\" 1\n" RUN-PRODUCT
   s" late-warm-hb" S\" 1\n" RUN-PRODUCT
   T-REPORT
   s" native file retired artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-RETIRED-TEST:MAIN
