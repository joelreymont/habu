\ hb-build-cli-errors-test.f - the hb-build CLI's errors (tools/hb-build-lib.f):
\ an out root that is a file, a source path or -o too long, an -o in no
\ directory, an -o that is a directory.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-cli-errors-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-BAD-OUT-ENV ( -- )
   PROC-ENV-RESET
   s" TMPDIR" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_OUT" >LEN HBT-BAD-OUT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-ADD-PATH-ERROR ( -- )
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+ ;

: HBT-PATH-ERROR-JSON$ ( -- ptr u8 n )
   SB-RESET
   S\" {\qschema\q:\qhb-build-error\q,\qversion\q:1,\qcode\q:\qE-BUILD-PATH\q}" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

\ The CLI's out root error: HBB-PREPARE-OUT-CLI catches the refused root,
\ cleans up and exits HBB-BUILD-RC with nothing on stdout and, under
\ --json-errors, the JSON object and one newline on stderr, byte for byte.
: CHECK-PATH-JSON ( -- )
   HBT-ARGV-BASE
   HBT-BAD-OUT-ENV
   s" --json-errors" >LEN PROC-ARGV+
   HBT-ADD-PATH-ERROR
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-BUILD-RC T=
   outu 0 T=
   HBT-ERR erru HBT-PATH-ERROR-JSON$ T$= ;

: CLI-PATH-ERROR ( -- )
   HBT-BAD-OUT s" out root file" FS:WRITE-ALL
   CHECK-PATH-JSON
   HBT-BAD-OUT FS:REMOVE-FILE ;

PATH-CAP 1+ BUFFER: HBT-LONG-OUT-BUF

\ A path of u bytes under the scratch root, its last name all `o`.
: HBT-LONG-OUT$ ( n -- ptr u8 n )
   {: u:n :}
   HBT-ROOT {: root:ptr rootu:n :}
   root HBT-LONG-OUT-BUF rootu BYTE-COPY
   [char] / HBT-LONG-OUT-BUF rootu + c!
   u rootu 1+ ?do [char] o HBT-LONG-OUT-BUF i + c! loop
   HBT-LONG-OUT-BUF u ;

: HBT-JOINED$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   HBT-LONG-OUT-BUF FS:JOIN-PATH HBT-LONG-OUT-BUF swap ;

\ The CLI's refusal of a source and an -o by name, before it builds anything:
\ the usage exit, the message on stderr and nothing on stdout.
: HBT-REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n want:ptr wantu:n :}
   HBT-ARGV-BASE
   src srcu >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: ou:n erru:n rc:n :}
   rc HBB-USAGE-RC T=
   ou 0 T=
   HBT-ERR erru want wantu T$= ;

: CLI-OUT-TOO-LONG ( -- )
   HBT-AOT-SRC PATH-CAP 1+ HBT-LONG-OUT$
   S\" hb-build: output path too long\n" HBT-REFUSED ;

\ The source path is held in a buffer of PATH-CAP bytes, so the CLI refuses
\ a longer one by name.
: CLI-SRC-TOO-LONG ( -- )
   PATH-CAP 1+ HBT-LONG-OUT$ HBT-AOT-OUT
   S\" hb-build: source path too long\n" HBT-REFUSED ;

\ The sibling is made in -o's directory, so the CLI refuses an -o whose
\ directory is missing by name rather than after a whole build.
: CLI-OUT-NO-DIR ( -- )
   HBT-AOT-SRC HBT-ROOT s" no-dir/aot" HBT-JOINED$
   S\" hb-build: no such output directory\n" HBT-REFUSED ;

\ -o is renamed over, so the CLI refuses by name one that is a directory.
: CLI-OUT-NOT-FILE ( -- )
   HBT-AOT-SRC HBT-ROOT
   S\" hb-build: output path is a directory\n" HBT-REFUSED ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-CLI-ERRORS-MAIN ( -- )
   T-RESET
   NULL$ HBT-KEYED!
   HBT-PREPARE
   CLI-PATH-ERROR
   CLI-OUT-TOO-LONG
   CLI-SRC-TOO-LONG
   CLI-OUT-NO-DIR
   CLI-OUT-NOT-FILE
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-cli-errors-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-CLI-ERRORS-MAIN
