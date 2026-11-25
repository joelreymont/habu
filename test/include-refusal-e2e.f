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

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: PROG$ ( -- str ) PROG PROG-U @ >str ;
: ERR$ ( -- str ) ERR ERR-U @ >str ;

\ <root>/<rel>, until the next call.
: AT ( str -- str )
   {: rel:str :}
   ROOT$ rel PATH FS:JOIN-PATH {: u:n :}
   PATH u >str ;

: SETUP ( -- )
   s" habu-include-refusal" FS:MKTEMP-DIR CANONICAL TTRUE {: d:str :}
   d FS:CLEANUP-TREE+
   d STR:DATA ROOT d STR:LEN BYTE-COPY d STR:LEN ROOT-U !
   s" cases" AT FS:MAKE-DIR
   s" plain.f" AT s" \ a file, not a directory" FS:WRITE-ALL ;

: PROG+ ( str -- )
   {: s:str :}
   s STR:LEN IO-CAP PROG-U @ - > if E-STR-CAPACITY throw then
   s STR:DATA PROG PROG-U @ + s STR:LEN BYTE-COPY
   s STR:LEN PROG-U +! ;

: PLINE ( str -- ) PROG+ S\" \n" PROG+ ;

\ <root>/cases/<name><suffix>.
: NAMED ( str str -- str )
   {: name:str suffix:str :}
   SB-RESET s" cases/" SB-APPEND name SB-APPEND suffix SB-APPEND SB$ AT ;

: RESULT! ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ The child's streams, beside its program.
: KEEP ( str -- )
   {: name:str :}
   name s" .out" NAMED OUT OUT-U @ >str FS:WRITE-ALL
   name s" .err" NAMED ERR$ FS:WRITE-ALL ;

\ The engine under test runs the program as <root>/cases/<name>.f with --load.
: LOAD-CASE ( str -- )
   {: name:str :}
   name s" .f" NAMED PROG$ FS:WRITE-ALL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" PROC-ARGV+
   name s" .f" NAMED PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ CWD$
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT!
   name KEEP ;

: EXITS ( n -- ) {: rc:n :}
   RC @ rc T=
   OUT-U @ 0 T= ;

: ERR-HAS ( str -- ) {: s:str :}
   ERR$ s STR:CONTAINS? TTRUE ;

\ The string builder holds <pre><root>/<rel>`.
: SB-NAMES ( str str -- )
   {: pre:str rel:str :}
   SB-RESET pre SB-APPEND rel AT SB-APPEND 96 SB-APPEND-C ;

\ A program that scopes the root <root>/<rel>, and the rejection naming that path.
: WITH-AT ( str str -- )
   {: name:str rel:str :}
   0 PROG-U !
   s" package INCLUDE-REFUSAL-WITH" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ rel AT PROG+
   S\" \q [: ;] SOURCE-ROOT:WITH ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-WITH:RUN" PLINE
   name LOAD-CASE
   70 EXITS
   s" error[E-SOURCE-ROOT]" ERR-HAS
   s" `SOURCE-ROOT:WITH` got `" rel SB-NAMES SB$ ERR-HAS
   s" does not resolve to a searchable directory" ERR-HAS ;

\ A file's path resolves, so only the directory check refuses it as a root.
: WITH-REFUSED ( -- )
   s" SOURCE-ROOT:WITH on a missing directory is rejected by name" T-LABEL
   s" with-absent" s" absent" WITH-AT
   s" so does SOURCE-ROOT:WITH on a file" T-LABEL
   s" with-file" s" plain.f" WITH-AT ;

\ A scope opened inside another scope's callback: its record names its own path
\ and stands nowhere on the line of the outer call, which passed a good one.
: WITH-NESTED ( -- )
   s" SOURCE-ROOT:WITH inside a WITH callback names its own path" T-LABEL
   0 PROG-U !
   S\" : INNER ( -- ) s\q " PROG+ s" absent" AT PROG+
   S\" \q [: ;] SOURCE-ROOT:WITH ;" PLINE
   S\" s\q " PROG+ s" cases" AT PROG+ S\" \q ' INNER SOURCE-ROOT:WITH" PLINE
   s" with-nested" LOAD-CASE
   70 EXITS
   s" `SOURCE-ROOT:WITH` got `" s" absent" SB-NAMES SB$ ERR-HAS
   ERR$ s" with-nested.f:2:" STR:CONTAINS? TFALSE ;

\ A loaded file is a closed program: its `;package` of the package its includer
\ opened is rejected where it stands, not at the includer's own `;package`.
: CLOSE-OUTER ( -- )
   s" a file's ;package of its includer's package is rejected in that file" T-LABEL
   s" cases/close-outer-inc.f" AT s" ;package" FS:WRITE-ALL
   0 PROG-U !
   s" package INCLUDE-REFUSAL-OUTER private package B" PLINE
   s" include " PROG+ s" cases/close-outer-inc.f" AT PLINE
   s" ;package ;package" PLINE
   s" close-outer" LOAD-CASE
   70 EXITS
   s" error[E-PKG-CONTEXT]" ERR-HAS
   s" closes `INCLUDE-REFUSAL-OUTER:B`," ERR-HAS
   SB-RESET s" cases/close-outer-inc.f" AT SB-APPEND s" :1:1" SB-APPEND SB$ ERR-HAS ;

\ A loader a scope's callback runs on bytes no source holds: its record stands
\ at no call, the one that opened the scope included.
: WITH-REQUIRED ( -- )
   s" a require in a WITH callback places its record at no call" T-LABEL
   0 PROG-U !
   s" create B 1024 allot" PLINE
   S\" : P ( -- ^u8 n ) s\q " PROG+ s" absent/x.f" AT PROG+
   S\" \q str> {: a:^ u:n :} a B u BYTE-COPY B u ;" PLINE
   s" : GO ( -- ) P >str required ;" PLINE
   S\" s\q " PROG+ s" cases" AT PROG+ S\" \q ' GO SOURCE-ROOT:WITH" PLINE
   s" with-required" LOAD-CASE
   70 EXITS
   s" cannot open `" s" absent/x.f" SB-NAMES SB$ ERR-HAS
   ERR$ s"  --> " STR:CONTAINS? TFALSE ;

\ A POLICY-WITH-ROOT opened inside another one's callback: its record names its
\ own path and stands nowhere on the line of the outer call.
: POLICY-NESTED ( -- )
   s" POLICY-WITH-ROOT inside another one's callback names its own path" T-LABEL
   0 PROG-U !
   s" : NOP ( -- ) ;" PLINE
   S\" : INNER ( -- ) s\q " PROG+ s" absent" AT PROG+
   S\" \q ['] NOP ['] NOP SOURCE-ROOT:POLICY-WITH-ROOT ;" PLINE
   S\" s\q " PROG+ s" cases" AT PROG+
   S\" \q ' INNER ' NOP SOURCE-ROOT:POLICY-WITH-ROOT" PLINE
   s" policy-nested" LOAD-CASE
   70 EXITS
   s" `SOURCE-ROOT:POLICY-WITH-ROOT` got `" s" absent" SB-NAMES SB$ ERR-HAS
   ERR$ s" policy-nested.f:3:" STR:CONTAINS? TFALSE ;

\ A program whose one line requires the absolute path <root>/<rel>, and the
\ loader's missing-source rejection naming that path.
: REQUIRE-AT ( str str -- )
   {: name:str rel:str :}
   0 PROG-U !
   s" require " PROG+ rel AT PLINE
   name LOAD-CASE
   70 EXITS
   s" error[E-MISSING-SOURCE]" ERR-HAS
   s" cannot open `" rel SB-NAMES SB$ ERR-HAS ;

\ A missing directory cannot be a root, so the load keeps the current one.
: REQUIRE-ABSENT ( -- )
   s" a require below a missing directory names the file it cannot open" T-LABEL
   s" require-dir" s" absent/x.f" REQUIRE-AT ;

\ A canon that finds nothing, not even the file system root.
: CANON-NONE ( -- )
   0 PROG-U !
   s" package INCLUDE-REFUSAL-CANON" PLINE
   s" private" PLINE
   s" : NONE ( str -- str bool ) 0 0= 0= ;" PLINE
   s" public" PLINE
   s" : CANON ( -- )" PLINE
   s"    [: NONE ;] [: SOURCE-ROOT:READ-OS ;] SOURCE-INPUT:USE" PLINE
   S\"    s\q /absent/x\q SOURCE-ROOT:CANONICAL drop drop ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-CANON:CANON" PLINE
   s" canon-none" LOAD-CASE
   s" a path with no directory above it resolving is rejected by name" T-LABEL
   70 EXITS
   s" error[E-SOURCE-ROOT]" ERR-HAS
   s" `SOURCE-INPUT:CANON` got `/absent/x`" ERR-HAS
   s" has no directory above it" ERR-HAS ;

\ A word that reads another file through the loader's reader while its own file
\ is loading: that file's frame holds its bytes already, so the read is refused
\ before it touches the frame.
: READ-OS-WORD ( -- )
   0 PROG-U !
   s" package INCLUDE-REFUSAL-READ" PLINE
   s" public" PLINE
   S\" : RUN ( -- ) s\q " PROG+ s" plain.f" AT PROG+
   S\" \q SOURCE-ROOT:READ-OS drop ;" PLINE
   s" ;package" PLINE
   s" INCLUDE-REFUSAL-READ:RUN" PLINE
   s" read-os-word" LOAD-CASE
   s" a word's SOURCE-ROOT:READ-OS of another file is rejected by name" T-LABEL
   70 EXITS
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
   74 EXITS
   ERR$ s" error[" STR:COUNT-SUB 1 T=
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
   74 EXITS
   ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" error[E-UNREADABLE-SOURCE]" ERR-HAS
   SB-RESET s" = help: if the file is yours, make it readable with `chmod u+r '" SB-APPEND
   s" cases/sp ace" AT SB-APPEND  S\" '\\''q.f'`; else ask its owner to let you read it"
   SB-APPEND SB$ ERR-HAS ;

\ A string loader given an empty path: one record, at the loader word, as an
\ empty path has no bytes to place.
: REQUIRED-EMPTY ( -- )
   s" a required of an empty path is rejected at the request" T-LABEL
   0 PROG-U !
   S\" s\q \q required" PLINE
   s" required-empty" LOAD-CASE
   70 EXITS
   ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" error[E-PATH-RANGE]: `required` got an empty path" ERR-HAS
   SB-RESET s"  --> " SB-APPEND s" cases/required-empty.f" AT SB-APPEND
   s" :1:6" SB-APPEND SB$ ERR-HAS
   s" = help: pass a path of 1 to " ERR-HAS ;

4200 constant LONG-U

\ LONG-U bytes of `x`, a path longer than PATH-CAP.
: LONG+ ( -- )
   LONG-U 0 ?do s" x" PROG+ loop ;

\ A loader word called by another spelling than its definition's: its record
\ names the word as the source wrote it.
: SPELLED-LOADERS ( -- )
   s" a too-long include names the word as written" T-LABEL
   0 PROG-U !
   s" INCLUDE " PROG+ LONG+ S\" \n" PROG+
   s" include-long" LOAD-CASE
   70 EXITS
   ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" error[E-PATH-RANGE]: `INCLUDE` got" ERR-HAS
   s" an exported required names the export as written" T-LABEL
   0 PROG-U !
   s" package INCLUDE-REFUSAL-RVX public EXPORT required ;package" PLINE
   S\" s\q " PROG+ LONG+ S\" \q INCLUDE-REFUSAL-RVX:required" PLINE
   s" required-export" LOAD-CASE
   70 EXITS
   ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" error[E-PATH-RANGE]: `INCLUDE-REFUSAL-RVX:required` got" ERR-HAS ;

public

: RUN ( -- )
   T-RESET SETUP
   WITH-REFUSED WITH-NESTED CLOSE-OUTER WITH-REQUIRED POLICY-NESTED REQUIRE-ABSENT CANON-NONE
   READ-OS-WORD REQUIRE-UNREADABLE LOAD-UNREADABLE-QUOTED REQUIRED-EMPTY SPELLED-LOADERS
   T-REPORT ;

;package
;using

INCLUDE-REFUSAL-E2E:RUN
