\ hb-build-cli-errors-test.f - checked fixture for tools/hb-build-lib.f: the
\ errors the hb-build CLI reports - a cache root that is a file, as JSON and as
\ text, an -o too long to replace, a preseeded entry name empty or too long
\ for the maker - and a MAIN whose effect breaks the application contract,
\ refused by the app-build child and passed through by the CLI.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-cli-errors-test.f

require tools/hb-build-test-lib.f
require test/app-image-engine.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

\ The quoted-path fixture below owns the writer's bytes; json-write holds none.
6 constant HBT-ESCAPE-MAX
FS-PATH-CAP HBT-ESCAPE-MAX * 2 + constant HBT-QUOTE-CAP
HBT-QUOTE-CAP BUFFER: HBT-QUOTE-BUF
TYPED-VARIABLE HBT-QUOTE-WRITER JSON-WRITE:writer

: HBT-BAD-CACHE-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-BAD-OUT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-ADD-PATH-ERROR ( -- )
   s" --json-errors" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+ ;

: HBT-BAD-OUT-QUOTED$ ( -- ptr u8 n )
   HBT-QUOTE-WRITER HBT-QUOTE-BUF HBT-QUOTE-CAP JSON-WRITE:OPEN
   HBT-BAD-OUT JSON-WRITE:STRING JSON-WRITE:$ ;

: HBT-PATH-ERROR-JSON$ ( -- ptr u8 n )
   SB-RESET
   S\" {\qschema\q:\qhb-build-error\q,\qversion\q:1,\qcode\q:\qE-BUILD-PATH\q,\qcache_selected\q:true,\qcache_root\q:" SB-APPEND
   HBT-BAD-OUT-QUOTED$ SB-APPEND
   S\" ,\qcache_source\q:\qexplicit\q,\qcause\q:\qE-FS-DIR\q}" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-PATH-ERROR-TEXT$ ( -- ptr u8 n )
   SB-RESET
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH cache_selected=true cache_root=" SB-APPEND
   HBT-BAD-OUT-QUOTED$ SB-APPEND
   s"  cache_source=explicit cause=E-FS-DIR" SB-APPEND
   SB$ ;

\ The CLI's cache path error: HBB-PREPARE-CACHE-CLI catches the refused root,
\ cleans up and exits HBB-BUILD-RC with nothing on stdout and, under
\ --json-errors, the JSON object and one newline on stderr, byte for byte.
: CHECK-PATH-JSON ( -- )
   HBT-ARGV-BASE
   HBT-BAD-CACHE-ENV
   HBT-ADD-PATH-ERROR
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-BUILD-RC T=
   outu 0 T=
   HBT-ERR erru HBT-PATH-ERROR-JSON$ T$= ;

\ Without --json-errors the same refusal takes the text form: HBB-PATH-ERROR$
\ is what HBB-PATH-ERROR writes, so it is asserted in this process on the root
\ the CLI refused above. CHECK-PATH-JSON fails if the CLI picks text under
\ --json-errors; this fails if the text form, or the choice of it, changes.
: HBT-CHECK-PATH-TEXT ( -- )
   HBB-RESET-OPTIONS
   HBT-BAD-OUT BUILD-CACHE:ROOT!
   [: HBB-PREPARE-ARTIFACT-CACHE ;] catch E-BUILD-PATH T=
   HBB-PATH-ERROR$ HBT-PATH-ERROR-TEXT$ T$=
   HBT-TMP BUILD-CACHE:ROOT! ;

: CLI-PATH-ERROR ( -- )
   HBT-BAD-OUT s" cache path file" WRITE-ALL
   CHECK-PATH-JSON
   HBT-CHECK-PATH-TEXT
   HBT-BAD-OUT REMOVE-FILE ;

FS-PATH-CAP 1+ BUFFER: HBT-LONG-OUT-BUF

\ An -o of u bytes under the scratch root, its last name all `o`.
: HBT-LONG-OUT$ ( n -- ptr u8 n )
   {: u:n :}
   HBT-ROOT {: root:ptr rootu:n :}
   root HBT-LONG-OUT-BUF rootu BYTE-COPY
   [char] / HBT-LONG-OUT-BUF rootu + c!
   u rootu 1+ ?do [char] o HBT-LONG-OUT-BUF i + c! loop
   HBT-LONG-OUT-BUF u ;

\ -o is replaced through a sibling with a longer name (lib/fs-mutate.f
\ SIBLING-PATH-MAX), so the CLI refuses an -o with no room for one by name
\ before it builds anything: the usage exit, the message on stderr and nothing
\ on stdout. One byte past FS-PATH-CAP is the same refusal.
: HBT-OUT-TOO-LONG ( n -- )
   {: u:n :}
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   u HBT-LONG-OUT$ >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-USAGE-RC T=
   outu 0 T=
   HBT-ERR erru S\" hb-build: output path too long\n" T$= ;

: CLI-OUT-TOO-LONG ( -- )
   SIBLING-PATH-MAX 1+ HBT-OUT-TOO-LONG
   FS-PATH-CAP 1+ HBT-OUT-TOO-LONG ;

HBB-ENTRY-NAME-CAP 1+ constant HBT-LONG-ENTRY-U
HBT-LONG-ENTRY-U BUFFER: HBT-LONG-ENTRY-BUF

\ The maker's AOT closure holds a preseeded entry name of one to
\ HBB-ENTRY-NAME-CAP bytes, so the CLI refuses an empty or longer one by name
\ before it builds anything, as it does a long -o.
: HBT-PRESEED-REFUSED ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n want:ptr wantu:n :}
   HBT-ARGV-BASE
   s" --preseed-entry" >LEN PROC-ARGV+
   name nameu >LEN PROC-ARGV+
   s" --preseed-seed" >LEN PROC-ARGV+
   s" 00" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-USAGE-RC T=
   outu 0 T=
   HBT-ERR erru want wantu T$= ;

: CLI-PRESEED-EMPTY ( -- )
   s" " S\" hb-build: --preseed-entry name empty\n" HBT-PRESEED-REFUSED ;

: CLI-PRESEED-TOO-LONG ( -- )
   HBT-LONG-ENTRY-U 0 ?do  [char] E HBT-LONG-ENTRY-BUF i + c!  loop
   HBT-LONG-ENTRY-BUF HBT-LONG-ENTRY-U
   S\" hb-build: --preseed-entry name too long\n" HBT-PRESEED-REFUSED ;

\ MAIN must satisfy the application's empty input/output contract at build
\ time: the app-build child refuses one that takes or leaves a value when it
\ compiles the startup word that calls it, and says so `in enter`. The child's
\ stderr stays in HBB-ERR-BUF; its length is returned.
: HBT-REFUSE-MAIN-APP ( ptr u8 n -- n )
   HBT-REPL-BAD-SRC 2swap WRITE-ALL
   HBT-REPL-BAD-SRC HBT-RUN-APP {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   HBB-ERR-BUF erru s" in enter" CONTAINS? TTRUE
   erru ;

\ A REPL build's refusal as the CLI reports it, on the source the child just
\ refused: tools/hb-build.f exits with the child's code, its stderr is the
\ child's stderr byte for byte (HBB-ERR-BUF; the CLI run captures into
\ HBT-ERR; the tool writes the child's stdout to stderr first, so equality
\ rests on HBT-REFUSE-MAIN-APP's empty child stdout), it prints nothing on
\ stdout and installs no image, so this case
\ fails if the tool swallows, rewrites, adds to or drops any of the child's
\ exit or diagnostic. tools/hb-build-stripped-test.f HBT-STRIPPED-NO-ENTRY is
\ the AOT path's.
: HBT-REFUSE-MAIN-CLI ( n -- ) {: want:n :}
   HBT-ARGV-BASE-REPL
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   HBT-ERR erru HBB-ERR-BUF want T$=
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-BAD-MAIN-EFFECTS ( -- )
   s" : MAIN ( n -- n ) 1+ ;" HBT-REFUSE-MAIN-APP HBT-REFUSE-MAIN-CLI
   s" : MAIN ( -- n ) 7 ;" HBT-REFUSE-MAIN-APP drop ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-CLI-ERRORS-MAIN ( -- )
   T-RESET
   NULL$ APP-IMAGE-ENGINE:PATH$ HBT-KEYED!
   HBT-PREPARE
   CLI-PATH-ERROR
   CLI-OUT-TOO-LONG
   CLI-PRESEED-EMPTY
   CLI-PRESEED-TOO-LONG
   HBT-BAD-MAIN-EFFECTS
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-cli-errors-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-CLI-ERRORS-MAIN
