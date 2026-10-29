\ hb-build-cli-errors-test.f - the hb-build CLI's errors (tools/hb-build-lib.f):
\ an out root that is a file (JSON and text), a source path or -o too long, an
\ -o in no directory, naming a directory or no file, or unwritable; a 250-byte
\ -o last name that builds; a MAIN that breaks the application contract.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-cli-errors-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require test/app-builder-engine.f

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

: HBT-PATH-ERROR-TEXT$ ( -- ptr u8 n )
   SB-RESET
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH" SB-APPEND
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

\ Without --json-errors the same refusal takes the text form, one line on
\ stderr. CHECK-PATH-JSON fails if the CLI picks text under --json-errors;
\ this fails if the text form, or the choice of it, changes.
: HBT-CHECK-PATH-TEXT ( -- )
   HBT-ARGV-BASE
   HBT-BAD-OUT-ENV
   HBT-ADD-PATH-ERROR
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-BUILD-RC T=
   outu 0 T=
   HBT-ERR erru HBT-PATH-ERROR-TEXT$ T$= ;

: CLI-PATH-ERROR ( -- )
   HBT-BAD-OUT s" out root file" FS:WRITE-ALL
   CHECK-PATH-JSON
   HBT-CHECK-PATH-TEXT
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

\ An -o under the scratch root whose last name is u bytes.
: HBT-LONG-NAME$ ( n -- ptr u8 n )
   HBT-ROOT nip 1+ + HBT-LONG-OUT$ ;

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
\ directory is missing, or is a file, by name rather than after a whole build.
: CLI-OUT-NO-DIR ( -- )
   HBT-AOT-SRC HBT-ROOT s" no-dir/aot" HBT-JOINED$
   S\" hb-build: no such output directory\n" HBT-REFUSED
   HBT-AOT-SRC HBT-AOT-SRC s" aot" HBT-JOINED$
   S\" hb-build: no such output directory\n" HBT-REFUSED ;

\ -o is renamed over, so the CLI refuses by name one that is a directory or
\ names no file in one.
: CLI-OUT-NOT-FILE ( -- )
   HBT-AOT-SRC HBT-ROOT
   S\" hb-build: output path is a directory\n" HBT-REFUSED
   HBT-AOT-SRC HBT-ROOT s" " HBT-JOINED$
   S\" hb-build: output path has no file name\n" HBT-REFUSED ;

\ The sibling's name never carries -o's, so the host alone judges the last
\ name: a 250-byte one, with room for its 4-byte `.map` beside it, passes the
\ argument check and is built and installed.
: CLI-OUT-LONG-NAME ( -- )
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   250 HBT-LONG-NAME$ {: out:ptr outu:n :}
   out outu >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD 0 T= 2drop
   out outu FS:FILE? TTRUE ;

$16D constant HBT-MODE-READ-ONLY            \ 0555: listable, not writable
$1ED constant HBT-MODE-WRITABLE             \ 0755: the owner may write again

: HBT-RO-DIR$ ( -- ptr u8 n )
   HBT-ROOT s" ro" HBT-JOINED$ ;

\ An -o in a directory the user cannot write passes every check the CLI can
\ make up front; the host refuses the sibling only at install, after the
\ build, and the CLI names that with its IO exit and nothing on stdout.
: CLI-OUT-READ-ONLY ( -- )
   HBT-RO-DIR$ FS:MAKE-DIR
   HBT-RO-DIR$ HBT-MODE-READ-ONLY FS:CHMOD-MODE
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-ROOT s" ro/aot" HBT-JOINED$ >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   HBT-RO-DIR$ HBT-MODE-WRITABLE FS:CHMOD-MODE
   rc HBB-BUILD-RC T=
   outu 0 T=
   HBT-ERR erru S\" hb-build: cannot replace output\n" T$= ;

\ MAIN must satisfy the application's empty input/output contract at build
\ time: the app-build child refuses one that takes or leaves a value when it
\ compiles the startup word that calls it, and says so `in enter`. The child's
\ stderr stays in HBB-ERR-BUF; its length is returned.
: HBT-REFUSE-MAIN-APP ( ptr u8 n -- n )
   HBT-REPL-BAD-SRC 2swap FS:WRITE-ALL
   HBT-REPL-BAD-SRC HBT-RUN-APP {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   HBB-ERR-BUF erru s" in enter" CONTAINS? TTRUE
   erru ;

\ The CLI's report of the child's refusal: tools/hb-build.f exits with the
\ child's code, its stderr is the child's byte for byte (the tool writes the
\ child's stdout to stderr first, so equality rests on HBT-REFUSE-MAIN-APP's
\ empty child stdout), nothing on stdout and no image installed.
\ tools/hb-build-stripped-test.f HBT-STRIPPED-NO-ENTRY is the AOT path's.
: HBT-REFUSE-MAIN-CLI ( n -- ) {: want:n :}
   HBT-ARGV-BASE-REPL
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   HBT-ERR erru HBB-ERR-BUF want T$=
   HBT-REPL-BAD-OUT FS:EXISTS? TFALSE ;

: HBT-BAD-MAIN-EFFECTS ( -- )
   s" : MAIN ( n -- n ) 1+ ;" HBT-REFUSE-MAIN-APP HBT-REFUSE-MAIN-CLI
   s" : MAIN ( -- n ) 7 ;" HBT-REFUSE-MAIN-APP drop ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-CLI-ERRORS-MAIN ( -- )
   T-RESET
   NULL$ HBT-KEYED!
   APP-BUILDER-ENGINE:ENSURE
   HBT-PREPARE
   CLI-PATH-ERROR
   CLI-OUT-TOO-LONG
   CLI-SRC-TOO-LONG
   CLI-OUT-NO-DIR
   CLI-OUT-NOT-FILE
   CLI-OUT-LONG-NAME
   CLI-OUT-READ-ONLY
   HBT-BAD-MAIN-EFFECTS
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-cli-errors-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-CLI-ERRORS-MAIN
