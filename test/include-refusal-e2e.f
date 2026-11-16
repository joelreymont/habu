\ include-refusal-e2e.f - a loader rejection that ends the process names itself:
\ each case pins the record's exit status, an empty stdout and a stderr record
\ carrying the rejection's code, the word or path it names and why. Children run the engine under test
\ (src/lib/engine-candidate.f) from this working directory; each case's program
\ and streams go in a temporary cases/ tree, which no missing path lies below
\ and the process removes at exit.
require src/lib/test.f
require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f

using SOURCE-ROOT
package INCLUDE-REFUSAL-E2E
private

$4000 constant IO-CAP
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create PROG IO-CAP allot variable PROG-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;
: PROG$ ( -- ^u8 n ) PROG PROG-U @ ;
: ERR$ ( -- ^u8 n ) ERR ERR-U @ ;

\ <root>/<rel>, until the next call.
: AT ( ^u8 n -- ^u8 n )
   {: rel:^ relu:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH {: u:n :}
   PATH u ;

: SETUP ( -- )
   s" habu-include-refusal" FS:MKTEMP-DIR CANONICAL TTRUE {: a:^ u:n :}
   a u FS:CLEANUP-TREE+
   a ROOT u BYTE-COPY u ROOT-U !
   s" cases" AT FS:MAKE-DIR
   s" plain.f" AT s" \ a file, not a directory" FS:WRITE-ALL ;

: PROG+ ( ^u8 n -- )
   {: a:^ u:n :}
   u IO-CAP PROG-U @ - > if E-STR-CAPACITY throw then
   a PROG PROG-U @ + u BYTE-COPY
   u PROG-U +! ;

: PLINE ( ^u8 n -- ) PROG+ S\" \n" PROG+ ;

\ <root>/cases/<name><suffix>.
: NAMED ( ^u8 n ^u8 n -- ^u8 n )
   {: name:^ nameu:n suffix:^ suffixu:n :}
   SB-RESET s" cases/" SB-APPEND name nameu SB-APPEND suffix suffixu SB-APPEND SB$ AT ;

: RESULT! ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ The child's streams, beside its program.
: KEEP ( ^u8 n -- )
   {: name:^ nameu:n :}
   name nameu s" .out" NAMED OUT OUT-U @ FS:WRITE-ALL
   name nameu s" .err" NAMED ERR$ FS:WRITE-ALL ;

\ The engine under test runs the program as <root>/cases/<name>.f with --load.
: LOAD-CASE ( ^u8 n -- )
   {: name:^ nameu:n :}
   name nameu s" .f" NAMED PROG$ FS:WRITE-ALL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   name nameu s" .f" NAMED >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN CWD$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT!
   name nameu KEEP ;

: EXITS ( n -- ) {: rc:n :}
   RC @ rc T=
   OUT-U @ 0 T= ;

: ERR-HAS ( ^u8 n -- ) {: a:^ u:n :}
   ERR$ a u CONTAINS? TTRUE ;

\ The string builder holds <pre><root>/<rel>`.
: SB-NAMES ( ^u8 n ^u8 n -- )
   {: pre:^ preu:n rel:^ relu:n :}
   SB-RESET pre preu SB-APPEND rel relu AT SB-APPEND 96 SB-APPEND-C ;

\ A program that scopes the root <root>/<rel>, and the rejection naming that path.
: WITH-AT ( ^u8 n ^u8 n -- )
   {: name:^ nameu:n rel:^ relu:n :}
   0 PROG-U !
   s" package INCLUDE-REFUSAL-WITH" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ rel relu AT PROG+
   S\" \q [: ;] SOURCE-ROOT:WITH ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-WITH:RUN" PLINE
   name nameu LOAD-CASE
   CHECKER-REJECT-RC EXITS
   s" error[E-SOURCE-ROOT]" ERR-HAS
   s" `SOURCE-ROOT:WITH` got `" rel relu SB-NAMES SB$ ERR-HAS
   s" does not resolve to a searchable directory" ERR-HAS ;

\ A file's path resolves, so only the directory check refuses it as a root.
: WITH-REFUSED ( -- )
   s" SOURCE-ROOT:WITH on a missing directory is rejected by name" T-LABEL
   s" with-absent" s" absent" WITH-AT
   s" so does SOURCE-ROOT:WITH on a file" T-LABEL
   s" with-file" s" plain.f" WITH-AT ;

\ A program whose one line requires the absolute path <root>/<rel>, and the
\ loader's missing-source rejection naming that path.
: REQUIRE-AT ( ^u8 n ^u8 n -- )
   {: name:^ nameu:n rel:^ relu:n :}
   0 PROG-U !
   s" require " PROG+ rel relu AT PLINE
   name nameu LOAD-CASE
   CHECKER-REJECT-RC EXITS
   s" error[E-MISSING-SOURCE]" ERR-HAS
   s" cannot open `" rel relu SB-NAMES SB$ ERR-HAS ;

\ A missing directory cannot be a root, so the load keeps the current one.
: REQUIRE-ABSENT ( -- )
   s" a require below a missing directory names the file it cannot open" T-LABEL
   s" require-dir" s" absent/x.f" REQUIRE-AT ;

\ A canon that finds nothing, not even the file system root.
: CANON-NONE ( -- )
   0 PROG-U !
   s" package INCLUDE-REFUSAL-CANON" PLINE
   s" private" PLINE
   s" : NONE ( ^u8 n -- ^u8 n bool ) 0 0= 0= ;" PLINE
   s" public" PLINE
   s" : RUN ( -- )" PLINE
   s"    [: NONE ;] [: SOURCE-ROOT:READ-OS ;] SOURCE-INPUT:USE" PLINE
   S\"    s\q /absent/x\q SOURCE-ROOT:CANONICAL drop 2drop ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-CANON:RUN" PLINE
   s" canon-none" LOAD-CASE
   s" a path with no directory above it resolving is rejected by name" T-LABEL
   CHECKER-REJECT-RC EXITS
   s" error[E-SOURCE-ROOT]" ERR-HAS
   s" got `/absent/x`" ERR-HAS
   s" has no directory above it" ERR-HAS ;

\ A word that reads another file through the loader's reader while its own file
\ is loading: that file's frame holds its bytes already, so the read is refused
\ before it touches the frame.
: READ-OS-WORD ( -- )
   0 PROG-U !
   s" package INCLUDE-REFUSAL-READ" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ s" plain.f" AT PROG+
   S\" \q SOURCE-ROOT:CURRENT$ SOURCE-ROOT:READ-OS 2drop ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-READ:RUN" PLINE
   s" read-os-word" LOAD-CASE
   s" a word's SOURCE-ROOT:READ-OS of another file is rejected by name" T-LABEL
   CHECKER-REJECT-RC EXITS
   s" error[E-SOURCE-ROOT]" ERR-HAS
   s" `SOURCE-ROOT:READ-OS` got `" s" plain.f" SB-NAMES SB$ ERR-HAS
   s" not the file a load is opening" ERR-HAS ;

0 constant NO-ACCESS
$1A4 constant MODE-FILE

\ A require of a file it finds but cannot read: one record, at the request. The
\ file's mode comes back even when the run throws, so the cleanup can read it.
: REQUIRE-UNREADABLE ( -- )
   s" a require of an unreadable file is rejected at the request" T-LABEL
   s" cases/locked.f" AT s" 1 drop" FS:WRITE-ALL
   s" cases/locked.f" AT NO-ACCESS FS:CHMOD-MODE
   0 PROG-U !
   s" require locked.f" PLINE
   [: s" require-unreadable" LOAD-CASE ;]
   [: s" cases/locked.f" AT MODE-FILE FS:CHMOD-MODE ;] finally
   INCLUDE-IO-RC EXITS
   ERR$ s" error[" COUNT-SUB 1 T=
   s" error[E-UNREADABLE-SOURCE]: cannot read `locked.f`" ERR-HAS
   SB-RESET s"  --> " SB-APPEND s" cases/require-unreadable.f" AT SB-APPEND
   s" :1:9" SB-APPEND SB$ ERR-HAS
   s" ^^^^^^^^ permission denied" ERR-HAS
   SB-RESET s" = help: if the file is yours, make it readable with `chmod u+r '" SB-APPEND
   s" cases/locked.f" AT SB-APPEND
   s" '`; else ask its owner to let you read it" SB-APPEND SB$ ERR-HAS ;

\ A load of an unreadable file whose name holds a blank and a `'`: the help's
\ chmod quotes the path as one shell word.
: LOAD-UNREADABLE-QUOTED ( -- )
   s" an unreadable file's chmod help quotes its path as one shell word" T-LABEL
   s" cases/sp ace'q.f" AT s" 1 drop" FS:WRITE-ALL
   s" cases/sp ace'q.f" AT NO-ACCESS FS:CHMOD-MODE
   0 PROG-U !
   S\" s\q " PROG+ s" cases/sp ace'q.f" AT PROG+ S\" \q included" PLINE
   [: s" load-unreadable-quoted" LOAD-CASE ;]
   [: s" cases/sp ace'q.f" AT MODE-FILE FS:CHMOD-MODE ;] finally
   INCLUDE-IO-RC EXITS
   ERR$ s" error[" COUNT-SUB 1 T=
   s" error[E-UNREADABLE-SOURCE]" ERR-HAS
   SB-RESET s" = help: if the file is yours, make it readable with `chmod u+r '" SB-APPEND
   s" cases/sp ace" AT SB-APPEND  S\" '\\''q.f'`; else ask its owner to let you read it"
   SB-APPEND SB$ ERR-HAS ;

\ A string loader given an empty path: one record, at the loader word, as the
\ literal's bytes lie in no source the run holds.
: REQUIRED-EMPTY ( -- )
   s" a required of an empty path is rejected at the request" T-LABEL
   0 PROG-U !
   S\" s\q \q required" PLINE
   s" required-empty" LOAD-CASE
   CHECKER-REJECT-RC EXITS
   ERR$ s" error[" COUNT-SUB 1 T=
   s" error[E-PATH-RANGE]: `required` got an empty path" ERR-HAS
   SB-RESET s"  --> " SB-APPEND s" cases/required-empty.f" AT SB-APPEND
   s" :1:6" SB-APPEND SB$ ERR-HAS
   s" = help: pass a path of 1 to " ERR-HAS ;

public

: RUN ( -- )
   T-RESET SETUP
   WITH-REFUSED REQUIRE-ABSENT CANON-NONE READ-OS-WORD REQUIRE-UNREADABLE
   LOAD-UNREADABLE-QUOTED REQUIRED-EMPTY
   T-REPORT ;

;package
;using

INCLUDE-REFUSAL-E2E:RUN
