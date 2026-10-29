\ include-refusal-e2e.f - a loader refusal that ends the process names itself:
\ an uncaught throw below 256 is the exit status and prints nothing, so each
\ case pins exit 74 (INCLUDE-IO-RC), an empty stdout and a one-line stderr
\ naming the refusal. Children run the engine under test
\ (src/lib/engine-candidate.f) from this working directory; each case's program
\ and streams stay in the printed cases/ tree, which no missing path lies below.
require src/lib/test.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f
require test/app-image-engine.f

using SOURCE-ROOT
package INCLUDE-REFUSAL-E2E
private

$4000 constant IO-CAP
\ Guards against a hung child, not budgets: the host runs loaded.
120000 constant TIMEOUT-MS
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create PROG IO-CAP allot variable PROG-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ <root>/<rel>, until the next call.
: AT ( ptr u8 n -- ptr u8 n )
   {: rel:ptr relu:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH {: u:n :}
   PATH u ;

: SETUP ( -- )
   s" habu-include-refusal" FS:MKTEMP-DIR CANONICAL TTRUE {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cases" AT FS:MAKE-DIR
   s" plain.f" AT s" \ a file, not a directory" FS:WRITE-ALL ;

: PROG+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u IO-CAP PROG-U @ - > if E-STR-CAPACITY throw then
   a PROG PROG-U @ + u BYTE-COPY
   u PROG-U +! ;

: PLINE ( ptr u8 n -- ) PROG+ S\" \n" PROG+ ;

\ The string builder holds <prefix><root>/<rel> and a newline.
: SB-AT ( ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n rel:ptr relu:n :}
   SB-RESET pre preu SB-APPEND rel relu AT SB-APPEND 10 SB-APPEND-C ;

\ <root>/cases/<name><suffix>.
: NAMED ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n suffix:ptr suffixu:n :}
   SB-RESET s" cases/" SB-APPEND name nameu SB-APPEND suffix suffixu SB-APPEND SB$ AT ;

: RESULT! ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ The child's streams, beside its program.
: KEEP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s" .out" NAMED OUT OUT-U @ FS:WRITE-ALL
   name nameu s" .err" NAMED ERR$ FS:WRITE-ALL ;

\ The engine under test runs the program as <root>/cases/<name>.f with --load.
: LOAD-CASE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s" .f" NAMED PROG$ FS:WRITE-ALL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   name nameu s" .f" NAMED >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN CWD$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT!
   name nameu KEEP ;

: EXITS-74 ( -- )
   RC @ 74 T=
   OUT-U @ 0 T= ;

\ A program that scopes the root <root>/<rel>, and the refusal naming that path.
: WITH-AT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n rel:ptr relu:n :}
   0 PROG-U !
   s" package INCLUDE-REFUSAL-WITH" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ rel relu AT PROG+
   S\" \q [: ;] SOURCE-ROOT:WITH ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-WITH:RUN" PLINE
   name nameu LOAD-CASE
   EXITS-74
   s" source root: does not resolve to a searchable directory within PATH-CAP bytes: "
   rel relu SB-AT
   ERR$ SB$ T$= ;

\ A file's path resolves, so only the directory check refuses it as a root.
: WITH-REFUSED ( -- )
   s" SOURCE-ROOT:WITH on a missing directory exits 74 by name" T-LABEL
   s" with-absent" s" absent" WITH-AT
   s" so does SOURCE-ROOT:WITH on a file" T-LABEL
   s" with-file" s" plain.f" WITH-AT ;

\ A program whose one line requires the absolute path <root>/<rel>, and the
\ loader's open failure naming that path.
: REQUIRE-AT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n rel:ptr relu:n :}
   0 PROG-U !
   s" require " PROG+ rel relu AT PLINE
   name nameu LOAD-CASE
   EXITS-74
   s" include: cannot open " rel relu SB-AT
   ERR$ SB$ T$= ;

\ A missing directory cannot be a root, so the load keeps the current one.
: REQUIRE-ABSENT ( -- )
   s" a require below a missing directory names the file it cannot open" T-LABEL
   s" require-dir" s" absent/x.f" REQUIRE-AT ;

\ A canon that finds nothing, not even the file system root.
: CANON-NONE ( -- )
   0 PROG-U !
   s" package INCLUDE-REFUSAL-CANON" PLINE
   s" private" PLINE
   s" : NONE ( ptr u8 n -- ptr u8 n bool ) 0 0= 0= ;" PLINE
   s" public" PLINE
   s" : RUN ( -- )" PLINE
   s"    [: NONE ;] [: SOURCE-ROOT:READ-OS ;] SOURCE-INPUT:USE" PLINE
   S\"    s\q /absent/x\q SOURCE-ROOT:CANONICAL drop 2drop ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-CANON:RUN" PLINE
   s" canon-none" LOAD-CASE
   s" a path with no directory above it resolving exits 74 by name" T-LABEL
   EXITS-74
   ERR$ S\" source root: no directory above the path resolves: /absent/x\n" T$= ;

\ The statement a bundle (tools/bundle-lib.f) makes for each file it assumes.
: BUNDLE-ABSENT ( -- )
   0 PROG-U !
   S\" s\q src/lib/absent.f\q ?ENGINE-PROVIDES" PLINE
   s" bundle-absent" LOAD-CASE
   s" a bundle assuming a file the engine lacks exits 74 by name" T-LABEL
   EXITS-74
   ERR$ S\" bundle: this engine does not provide src/lib/absent.f\n" T$= ;

\ The program on stdin, at the top level a save runs at, with the image path as
\ its argument, on the engine with the saver loaded (test/app-image-engine.f).
\ The saver's image starts at tier 0, where APP-IMAGE:SAVE refuses retained
\ code, so the program sets tier 1: only the root scope can refuse it.
: SAVE-IN-WITH ( -- )
   APP-IMAGE-ENGINE:PATH$ {: eng:ptr engu:n :}
   0 PROG-U !
   s" 1 set-tier" PLINE
   s" package INCLUDE-REFUSAL-SAVE" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ ROOT$ PROG+
   S\" \q [: 0 SCRIPT-ARGV$ APP-IMAGE:SAVE ;] SOURCE-ROOT:WITH ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-SAVE:RUN" PLINE
   s" save-in-with" s" .f" NAMED PROG$ FS:WRITE-ALL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" image" AT >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   eng engu >LEN CWD$ >LEN PROG$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT!
   s" save-in-with" KEEP
   s" an image save inside SOURCE-ROOT:WITH exits 74 by name" T-LABEL
   EXITS-74
   ERR$ S\" source root: an image cannot be saved inside a root scope\n" T$=
   s" and writes no image" T-LABEL
   s" image" AT FS:EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET SETUP
   WITH-REFUSED REQUIRE-ABSENT CANON-NONE BUNDLE-ABSENT SAVE-IN-WITH
   s" include refusal tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

INCLUDE-REFUSAL-E2E:RUN
