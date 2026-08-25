\ load-argv-test.f - the `bin/hb --load` argv loader contract.
\
\ Run: bin/hb --load lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f \
\   lib/process-argv.f tools/load-argv-test.f
\
\ `--load` means "make these files loaded". The row used to write the path into
\ the require registry as `provided` and then inline the file text anyway - it
\ maintained a registry it never read - so the same request got two answers:
\ `--load lib/errors.f` against an engine that already carries lib/errors.f died
\ on a duplicate definition, while `require lib/errors.f` from inside a file
\ no-opped cleanly. Since dot habu-make-load-consult-85c88fb3 the row emits
\ `s" <path>" required` and src/core/include.f decides.
\
\ Every case forks the real bin/hb with a real command line: this contract only
\ exists at the process entry point, so an in-process stand-in would prove
\ nothing about it.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

package LOAD-ARGV-TEST

$4000 constant LA-CAP
30000 constant LA-TIMEOUT-MS

\ A path the boot prefix already carries, and one it does not. Both are read
\ from the engine's own behaviour by the first two cases below rather than
\ asserted here, so this file does not have to track the prefix table.
: LA-SEEDED$ ( -- ptr u8 n )   s" lib/errors.f" ;

create LA-ROOT   FS-PATH-CAP allot
create LA-PROBE  FS-PATH-CAP allot
create LA-THROW  FS-PATH-CAP allot
create LA-DECOY  FS-PATH-CAP allot
create LA-MISS   FS-PATH-CAP allot
create LA-OUT    LA-CAP allot
create LA-ERR    LA-CAP allot

variable LA-ROOT-U
variable LA-PROBE-U
variable LA-THROW-U
variable LA-DECOY-U
variable LA-MISS-U

: LA-ROOT$ ( -- ptr u8 n )    LA-ROOT LA-ROOT-U @ ;
: LA-PROBE$ ( -- ptr u8 n )   LA-PROBE LA-PROBE-U @ ;
: LA-THROW$ ( -- ptr u8 n )   LA-THROW LA-THROW-U @ ;
: LA-DECOY$ ( -- ptr u8 n )   LA-DECOY LA-DECOY-U @ ;
: LA-MISS$ ( -- ptr u8 n )    LA-MISS LA-MISS-U @ ;

\ The probe both proves the run reached user source AND proves the seeded
\ module is usable, so a case cannot pass by loading nothing at all.
: LA-PROBE-SRC$ ( -- ptr u8 n )
   s\" E-A-FIRST . cr\ns\" load-argv: reached\" type cr\n" ;

: LA-THROW-SRC$ ( -- ptr u8 n )
   s\" : LA-T ( -- ) 7141 throw ;\nLA-T\n" ;

\ Adversarial fixture: a file that merely MENTIONS a seeded path, in a comment
\ and in a string, and defines a word of its own. A loader that matched on text
\ rather than on the registry key would skip it; it must load.
: LA-DECOY-SRC$ ( -- ptr u8 n )
   s\" \\ mentions lib/errors.f in a comment\n: LA-D ( -- ptr u8 n ) s\" lib/errors.f\" ;\ns\" load-argv: decoy loaded\" type cr\n" ;

: LA-SETUP ( -- )
   CLEANUP-RESET
   s" habu-load-argv" TMPDIR-MKDIR {: a:ptr u:n :}
   a LA-ROOT u BYTE-COPY
   u LA-ROOT-U !
   LA-ROOT$ CLEANUP-TREE+
   LA-ROOT$ s" probe.f"   LA-PROBE JOIN-PATH LA-PROBE-U !
   LA-ROOT$ s" thrower.f" LA-THROW JOIN-PATH LA-THROW-U !
   LA-ROOT$ s" decoy.f"   LA-DECOY JOIN-PATH LA-DECOY-U !
   LA-ROOT$ s" missing.f" LA-MISS  JOIN-PATH LA-MISS-U !
   LA-PROBE$ LA-PROBE-SRC$ WRITE-ALL
   LA-THROW$ LA-THROW-SRC$ WRITE-ALL
   LA-DECOY$ LA-DECOY-SRC$ WRITE-ALL ;

: LA-ARGV0 ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+ ;

: LA-ARG ( ptr u8 n -- )   >LEN PROC-ARGV+ ;

: LA-RUN ( -- n n n )                              \ -> outu erru rc
   s" bin/hb" >LEN LA-OUT LA-CAP >LEN LA-ERR LA-CAP >LEN LA-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: LA-OUT$ ( n -- ptr u8 n ) {: u:n :}   LA-OUT u ;
: LA-ERR$ ( n -- ptr u8 n ) {: u:n :}   LA-ERR u ;

\ ---- cases -----------------------------------------------------------------

\ The whole point of the dot: a path the boot prefix already loaded may still
\ be named on the command line. It used to be fatal.
: LA-SEEDED-PATH-OK ( -- )
   LA-ARGV0  LA-SEEDED$ LA-ARG  LA-PROBE$ LA-ARG
   LA-RUN {: outu:n erru:n rc:n :}
   s" --load of a boot-prefix path exits 0" T-LABEL
   rc 0 T=
   s" --load of a boot-prefix path reaches user source" T-LABEL
   outu LA-OUT$ s" load-argv: reached" CONTAINS? TTRUE
   s" --load of a boot-prefix path says nothing on stderr" T-LABEL
   erru 0 T= ;

\ The registry is keyed on the path, so a repeat on one command line collapses
\ too. Under the old row this was the same duplicate-definition death.
: LA-REPEATED-PATH-OK ( -- )
   LA-ARGV0  LA-SEEDED$ LA-ARG  LA-SEEDED$ LA-ARG  LA-PROBE$ LA-ARG
   LA-RUN {: outu:n erru:n rc:n :}
   s" a path repeated on one --load line is loaded once" T-LABEL
   rc 0 T=
   outu LA-OUT$ s" load-argv: reached" CONTAINS? TTRUE ;

\ ...and the loader did not simply become a no-op: a path the prefix does NOT
\ carry still loads, even though its text names a seeded path in a comment and
\ in a string. Text matching would skip it; the registry key does not.
: LA-UNSEEDED-PATH-LOADS ( -- )
   LA-ARGV0  LA-DECOY$ LA-ARG  LA-PROBE$ LA-ARG
   LA-RUN {: outu:n erru:n rc:n :}
   s" an unseeded --load file still loads" T-LABEL
   rc 0 T=
   outu LA-OUT$ s" load-argv: decoy loaded" CONTAINS? TTRUE
   outu LA-OUT$ s" load-argv: reached" CONTAINS? TTRUE ;

\ A mistyped command line has to name the path it could not open. The raw argv
\ reader this replaced always did; the include layer used to drop it.
: LA-MISSING-PATH-NAMED ( -- )
   LA-ARGV0  LA-MISS$ LA-ARG
   LA-RUN {: outu:n erru:n rc:n :}
   s" a missing --load file exits 74" T-LABEL
   rc 74 T=
   s" a missing --load file is named on stderr" T-LABEL
   erru LA-ERR$ LA-MISS$ CONTAINS? TTRUE
   erru LA-ERR$ s" cannot open" CONTAINS? TTRUE ;

\ Nesting each argv file in its own evaluate must not reshape an uncaught
\ throw: same line, same status as before the change.
: LA-THROW-SHAPE ( -- )
   LA-ARGV0  LA-THROW$ LA-ARG
   LA-RUN {: outu:n erru:n rc:n :}
   s" an uncaught throw in a --load file exits 67" T-LABEL
   rc 67 T=
   s" an uncaught throw in a --load file names its code" T-LABEL
   erru LA-ERR$ s" hb: uncaught throw code 7141" CONTAINS? TTRUE ;

public

: LOAD-ARGV-TEST-MAIN ( -- )
   T-RESET
   LA-SETUP
   LA-SEEDED-PATH-OK
   LA-REPEATED-PATH-OK
   LA-UNSEEDED-PATH-LOADS
   LA-MISSING-PATH-NAMED
   LA-THROW-SHAPE
   CLEANUP-RUN
   T-REPORT
   s" load-argv-test: ok" type cr ;

;package

LOAD-ARGV-TEST:LOAD-ARGV-TEST-MAIN
