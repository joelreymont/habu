\ include-refusal-e2e.f - a loader refusal that ends the process names itself.
\
\ An uncaught throw of a code below 256 is the process's exit status and prints
\ nothing, so a loader refusal thrown as INCLUDE-IO-RC ended a run with exit 74
\ and not one byte on either stream. Each case runs the engine under test
\ (lib/engine-candidate.f) as a child from this process's working directory,
\ through the words a program uses, and pins exit 74, an empty stdout and the
\ whole of stderr: one line, ending in one LF, that names the refusal:
\ - SOURCE-ROOT:WITH on a directory that does not exist, or on a file;
\ - a require of an absolute path outside every root, whose directory does not
\   exist, names the file it could not open, as one missing from a directory
\   does;
\ - a source canon (SOURCE-INPUT:USE) under which no directory above a path
\   resolves;
\ - an image save inside a top-level SOURCE-ROOT:WITH, which writes no image;
\ - a bundle's ?ENGINE-PROVIDES of a file the engine does not carry.
\ Each child's program, stdout and stderr stay in the case tree's cases/, whose
\ path is printed: a program there has cases/ as its root, and no missing path
\ lies below it or below the working directory.
require lib/test.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

using SOURCE-ROOT
package INCLUDE-REFUSAL-E2E
private

$4000 constant IO-CAP
\ Guards against a hung child, not budgets: the host runs loaded.
120000 constant TIMEOUT-MS
\ A save compiles src/habu/app-image.f from source, which a loaded host makes slow.
600000 constant SAVE-TIMEOUT-MS
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
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
   ROOT$ rel relu PATH JOIN-PATH {: u:n :}
   PATH u ;

: SETUP ( -- )
   s" habu-include-refusal" HB-TMP-MKDIR CANONICAL TTRUE {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cases" AT MAKE-DIR
   s" plain.f" AT s" \ a file, not a directory" WRITE-ALL ;

\ The child's program, built a piece at a time.
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
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 0 ENDOF
      err OF PCAP-FAILED:UNMAKE RC>N ENDOF
   ;MATCH
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC ! ;

\ The child's streams, beside its program.
: KEEP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s" .out" NAMED OUT OUT-U @ WRITE-ALL
   name nameu s" .err" NAMED ERR$ WRITE-ALL ;

\ The engine under test runs the program as <root>/cases/<name>.f with --load.
: LOAD-CASE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s" .f" NAMED PROG$ WRITE-ALL
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
   S\" s\q lib/absent.f\q ?ENGINE-PROVIDES" PLINE
   s" bundle-absent" LOAD-CASE
   s" a bundle assuming a file the engine lacks exits 74 by name" T-LABEL
   EXITS-74
   ERR$ S\" bundle: this engine does not provide lib/absent.f\n" T$= ;

\ The program on stdin, at the top level a save runs at, with the image path as
\ its argument.
: SAVE-IN-WITH ( -- )
   0 PROG-U !
   s" require src/habu/app-image.f" PLINE
   s" package INCLUDE-REFUSAL-SAVE" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ ROOT$ PROG+
   S\" \q [: 0 SCRIPT-ARGV$ APP-IMAGE:SAVE ;] SOURCE-ROOT:WITH ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-SAVE:RUN" PLINE
   s" save-in-with" s" .f" NAMED PROG$ WRITE-ALL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" image" AT >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN CWD$ >LEN PROG$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SAVE-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT!
   s" save-in-with" KEEP
   s" an image save inside SOURCE-ROOT:WITH exits 74 by name" T-LABEL
   EXITS-74
   ERR$ S\" source root: an image cannot be saved inside a root scope\n" T$=
   s" and writes no image" T-LABEL
   s" image" AT EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET SETUP
   WITH-REFUSED REQUIRE-ABSENT CANON-NONE BUNDLE-ABSENT SAVE-IN-WITH
   s" include refusal tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

INCLUDE-REFUSAL-E2E:RUN
