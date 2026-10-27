\ The build cache root keeps one checksummed object per loaded file. An entry
\ a build does not use survives it, another engine's build leaves this
\ engine's entries, concurrent builds into one root leave entries their
\ rebuilds reuse, a corrupt entry is refused on fd 2 and rebuilt, and hb-build
\ prunes the entries nothing has used for a day.
\ Run: bin/hb --load test/native-file-storage-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/time.f
require test/whitebox-child.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-STORAGE-TEST

create CASE-PATH FS-PATH-CAP allot
create CACHE-PATH FS-PATH-CAP allot  variable CACHE-U
create ENTRY-PATH FS-PATH-CAP allot
$1000 constant NAMES-CAP
create COLD-NAMES NAMES-CAP allot  variable COLD-U
DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER PRODUCT-A u8  variable PRODUCT-A-U
DYNAMIC-BUFFER PRODUCT-B u8  variable PRODUCT-B-U
variable FLIP-N
variable CURSOR
variable SINCE
variable OBJECT-N
variable STALE-N

: LIB-V1$ ( -- ptr u8 n )
   S\" package STORE-LIB\npublic\n: LIB-X ( -- n ) 1 ;\n: LIB-Y ( -- n ) 2 ;\n: LIB-Z ( -- n ) 3 ;\n;package\n" ;
: LIB-V3$ ( -- ptr u8 n )
   S\" package STORE-LIB\npublic\n: LIB-X ( -- n ) 1 ;\n: LIB-W ( -- n ) 5 ;\n: LIB-Z ( -- n ) 3 ;\n;package\n" ;
: APP$ ( -- ptr u8 n )
   S\" require lib.f\n: MAIN ( -- ) STORE-LIB:LIB-X STORE-LIB:LIB-Z + . ;\n" ;
: APP-NO-LIB$ ( -- ptr u8 n )
   S\" : MAIN ( -- ) 7 . ;\n" ;
: APP-EDITED$ ( -- ptr u8 n )
   S\" require lib.f\n\\ edited\n: MAIN ( -- ) STORE-LIB:LIB-X STORE-LIB:LIB-Z + . ;\n" ;
: LIB-A$ ( -- ptr u8 n )
   S\" package STORE-A\npublic\n: A-ONE ( -- n ) 10 ;\n: A-TWO ( -- n ) A-ONE 1+ ;\n;package\n" ;
: LIB-B$ ( -- ptr u8 n )
   S\" package STORE-B\npublic\n: B-ONE ( -- n ) 20 ;\n: B-TWO ( -- n ) B-ONE 2 + ;\n: B-THREE ( -- n ) B-TWO 3 + ;\n;package\n" ;
: SAVED-SOURCE$ ( -- ptr u8 n )
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" ;

\ dir/name under the tree, in CASE-PATH.
: IN ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: dir:ptr diru:n name:ptr nameu:n :}
   dir diru AT name nameu CASE-PATH JOIN-PATH CASE-PATH swap ;

: WRITE-IN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n name:ptr nameu:n body:ptr bodyu:n :}
   dir diru name nameu IN body bodyu WRITE-ALL ;

\ Later builds use the fresh cache root dir/name.
: CACHE-IN ( ptr u8 n ptr u8 n -- )
   IN {: a:ptr u:n :}
   a CACHE-PATH u BYTE-COPY u CACHE-U !
   CACHE-PATH u MAKE-DIR ;

: CASE-DIR ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   dir diru AT MAKE-DIR
   dir diru s" cache" CACHE-IN ;

\ A build of dir/app into dir/out by engine, diagnostics on, in the cache root.
: STAGE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n app:ptr appu:n out:ptr outu:n engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN CACHE-PATH CACHE-U @ >LEN PROC-ENV+
   s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN engine engineu >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG
   dir diru app appu IN ARG
   dir diru out outu IN ARG ;

\ The saved builder image, as test/native-file-auto-e2e.f RUN-SAVED-IN.
: SAVED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: dir:ptr diru:n app:ptr appu:n out:ptr outu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   dir diru app appu out outu ENGINE-CANDIDATE:PATH$ STAGE
   builder builderu SAVED-SOURCE$ CAPTURE ;

: SAVED-APP ( ptr u8 n -- n ) s" app.f" s" app-hb" SAVED ;

\ The engine itself loading tools/app-build.f.
: DIRECT ( ptr u8 n ptr u8 n -- n )
   {: dir:ptr diru:n engine:ptr engineu:n :}
   dir diru s" app.f" s" app-hb" engine engineu STAGE
   engine engineu S\" require tools/app-build.f\nAPP-BUILD:RUN\n" CAPTURE ;

: RUN-IN ( ptr u8 n ptr u8 n -- n )
   IN {: a:ptr u:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   a u s" " CAPTURE ;

\ The number after label in the build's report, -1 when the report lacks it.
: COUNT ( ptr u8 n -- n )
   {: label:ptr labelu:n :}
   OUT$ {: out:ptr outu:n :}
   out outu label labelu FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N labelu + ENDOF
   ;MATCH {: at:n :}
   at 0 < if -1 exit then
   out at + outu at - 10 0 SPLIT-NEXT 2drop
      {: digits:ptr digitsu:n :}
   digits digitsu TRIM STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: REUSED ( -- n ) s" files reused " COUNT ;
: COMPILED ( -- n ) s" compiled " COUNT ;

: REFUSED? ( -- bool ) ERR$ s" native-cache: entry refused " CONTAINS? ;

\ The file's bytes in BYTES.
: LOAD ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size BYTES-RESERVE
   path pathu 0 BYTES size READ-ALL size T=
   size ;

: KEEP-A ( ptr u8 n -- )
   LOAD {: size:n :}
   size PRODUCT-A-RESERVE
   0 BYTES 0 PRODUCT-A size BYTE-COPY size PRODUCT-A-U ! ;

: KEEP-B ( ptr u8 n -- )
   LOAD {: size:n :}
   size PRODUCT-B-RESERVE
   0 BYTES 0 PRODUCT-B size BYTE-COPY size PRODUCT-B-U ! ;

: SAME-A? ( ptr u8 n -- bool )
   LOAD {: size:n :}
   0 BYTES size 0 PRODUCT-A PRODUCT-A-U @ STR= ;

: SAME-B? ( ptr u8 n -- bool )
   LOAD {: size:n :}
   0 BYTES size 0 PRODUCT-B PRODUCT-B-U @ STR= ;

\ The cache root's entry name, in ENTRY-PATH.
: ENTRY$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   CACHE-PATH CACHE-U @ name nameu ENTRY-PATH JOIN-PATH ENTRY-PATH swap ;

\ A file object: hb-file-, 64 hex digits, .hfo.
: OBJECT? ( ptr u8 n -- bool )
   {: name:ptr nameu:n :}
   name nameu s" hb-file-" STARTS-WITH? name nameu s" .hfo" ENDS-WITH? and
   nameu 76 = and ;

\ Count a file object still in the root, and whether it was last dated
\ before SINCE.
: TALLY ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu OBJECT? 0= if exit then
   name nameu ENTRY$ FS-TRY-LSTAT 0= if exit then
   1 OBJECT-N +!
   FS-STAT-MTIME-SEC@ SINCE @ < if 1 STALE-N +! then ;

: TALLY-RESET ( -- ) 0 OBJECT-N ! 0 STALE-N ! ;

: TALLY-ROOT ( -- )
   TALLY-RESET
   CACHE-PATH CACHE-U @ [: TALLY ;] FS-LIST:EACH ;

\ Flip the last byte of one cache entry, which its checksum covers.
: FLIP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu OBJECT? 0= if exit then
   name nameu ENTRY$ {: path:ptr u:n :}
   path u LOAD {: size:n :}
   size 1- BYTES dup c@ $FF xor swap c!
   path u 0 BYTES size WRITE-ALL
   1 FLIP-N +! ;

: FLIP-ALL ( -- n )
   0 FLIP-N !
   CACHE-PATH CACHE-U @ [: FLIP ;] FS-LIST:EACH
   FLIP-N @ ;

\ 1. A file a build does not load keeps its object for the next build that
\ does: an app that drops lib.f and requires it again reuses lib.f's object.
: KEEP-CASE ( -- )
   s" an entry survives a build that does not use it" T-LABEL
   s" keep" CASE-DIR
   s" keep" s" lib.f" LIB-V1$ WRITE-IN
   s" keep" s" app.f" APP$ WRITE-IN
   s" keep" SAVED-APP SUCCESS
   s" keep" s" app.f" APP-NO-LIB$ WRITE-IN
   s" keep" SAVED-APP SUCCESS
   s" keep" s" app.f" APP$ WRITE-IN
   s" keep" SAVED-APP SUCCESS
   ERR$ s" name=LIB-" CONTAINS? TFALSE
   \ A file key names the file's place in the load, not its text: the second
   \ build stored its app.f under the first's name, so only MAIN compiles.
   REUSED 1 T=
   COMPILED 1 T=
   s" keep" s" app-hb" RUN-IN SUCCESS
   OUT$ s" 4" CONTAINS? TTRUE ;

\ 2. Another engine, whose checker and kernel identity differ, writes its own
\ entries beside this engine's in the same root and leaves this engine's.
: ALTERNATE-CASE ( -- )
   s" another engine's build keeps this engine's entries" T-LABEL
   s" alternate" CASE-DIR
   s" alternate" s" lib.f" LIB-V1$ WRITE-IN
   s" alternate" s" app.f" APP$ WRITE-IN
   s" alternate" ENGINE-CANDIDATE:PATH$ DIRECT SUCCESS
   s" alternate" ENGINE-CANDIDATE:PATH$ DIRECT SUCCESS
   REUSED {: warm:n :}
   COMPILED {: warm-compiled:n :}
   warm 0 > TTRUE
   TALLY-ROOT OBJECT-N @ {: first-objects:n :}
   s" alternate" WHITEBOX-CHILD:ENGINE$ DIRECT SUCCESS
   TALLY-ROOT OBJECT-N @ first-objects > TTRUE
   s" alternate" ENGINE-CANDIDATE:PATH$ DIRECT SUCCESS
   REUSED warm T=
   COMPILED warm-compiled T=
   s" alternate" s" app-hb" RUN-IN SUCCESS
   OUT$ s" 4" CONTAINS? TTRUE ;

: ALTERNATE-CASES ( -- )
   [: s" native-file-storage-engine" WHITEBOX-CHILD:PROVIDE
      ALTERNATE-CASE ;]
   [: WHITEBOX-CHILD:REMOVE ;] finally ;

\ A concurrent build of race/app into race/out, streams to race/log, stdin
\ race/drive.
: SPAWN ( ptr u8 n ptr u8 n ptr u8 n -- pid )
   {: app:ptr appu:n out:ptr outu:n log:ptr logu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   s" race" s" drive" IN FS-PATHZ open-rd {: in:n :}
   in 0 < if E-FS-OPEN throw then
   s" race" log logu IN OPEN-APPEND-FD {: fd:n :}
   s" race" app appu out outu ENGINE-CANDIDATE:PATH$ STAGE
   builder builderu >LEN in >FD fd >FD fd >FD PROC-SPAWN-ARGV-ENV-IO
   in close
   fd close ;

: EXITED ( pid -- n )
   PROC-WAIT-RC MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

\ 3. Two builds racing into one root, both loading lib-a.f and lib-b.f, leave
\ every entry each made: a key covers the requiring file, so the two keep
\ entries for the same definitions side by side. A rebuild of each app reuses
\ all of its own, and the products are the ones a serial pair builds into a
\ fresh root.
: CONCURRENT-CASE ( -- )
   s" concurrent builds share one root" T-LABEL
   s" race" CASE-DIR
   s" race" s" lib-a.f" LIB-A$ WRITE-IN
   s" race" s" lib-b.f" LIB-B$ WRITE-IN
   s" race" s" app-a.f"
      S\" require lib-a.f\nrequire lib-b.f\n: MAIN ( -- ) STORE-A:A-TWO . ;\n" WRITE-IN
   s" race" s" app-b.f"
      S\" require lib-a.f\nrequire lib-b.f\n: MAIN ( -- ) STORE-B:B-THREE . ;\n" WRITE-IN
   s" race" s" drive" SAVED-SOURCE$ WRITE-IN
   s" app-a.f" s" a-hb" s" log-a" SPAWN {: pa:pid :}
   s" app-b.f" s" b-hb" s" log-b" SPAWN {: pb:pid :}
   pa EXITED 0 T=
   pb EXITED 0 T=
   s" race" s" a-hb" IN KEEP-A
   s" race" s" b-hb" IN KEEP-B
   s" race" s" app-a.f" s" a-hb" SAVED SUCCESS
   REUSED 3 T=
   COMPILED 0 T=
   s" race" s" a-hb" RUN-IN SUCCESS
   OUT$ s" 11" CONTAINS? TTRUE
   s" race" s" app-b.f" s" b-hb" SAVED SUCCESS
   REUSED 3 T=
   COMPILED 0 T=
   s" race" s" b-hb" RUN-IN SUCCESS
   OUT$ s" 25" CONTAINS? TTRUE
   s" race" s" serial-cache" CACHE-IN
   s" race" s" app-a.f" s" a-hb" SAVED SUCCESS
   s" race" s" app-b.f" s" b-hb" SAVED SUCCESS
   s" race" s" a-hb" IN SAME-A? TTRUE
   s" race" s" b-hb" IN SAME-B? TTRUE ;

\ 4. A flipped byte in every entry is refused and rebuilt.
: BYTE-CASE ( -- )
   s" a corrupt entry is refused and rebuilt" T-LABEL
   s" flip" CASE-DIR
   s" flip" s" lib.f" LIB-V1$ WRITE-IN
   s" flip" s" app.f" APP$ WRITE-IN
   s" flip" SAVED-APP SUCCESS
   COMPILED {: cold:n :}
   s" flip" SAVED-APP SUCCESS
   REUSED 2 T=
   FLIP-ALL 2 T=
   s" flip" s" app.f" APP-EDITED$ WRITE-IN
   s" flip" SAVED-APP SUCCESS
   REFUSED? TTRUE
   ERR$ s" : checksum" CONTAINS? TTRUE
   COMPILED cold T=
   s" flip" s" app-hb" RUN-IN SUCCESS
   OUT$ s" 4" CONTAINS? TTRUE
   s" flip" SAVED-APP SUCCESS
   REFUSED? TFALSE
   REUSED 2 T=
   COMPILED 0 T= ;

\ tools/hb-build.f on the candidate engine, as BUILD-APP: dir/app.f into
\ dir/app-hb as a REPL build, in the cache root. It runs at tier 1
\ (test/compiler/aot-mode.f): hb-build builds at its caller's tier, and only a
\ tier 1 product compiles through the file objects the prune case dates.
\ The tool library loads first, at the default tier, so only the product
\ compiles at tier 1.
: HB-BUILD ( ptr u8 n -- n )
   {: dir:ptr diru:n :}
   NATIVE-FILE-APP:BUILDER$ 2drop
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN CACHE-PATH CACHE-U @ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build-lib.f" ARG s" test/compiler/aot-mode.f" ARG
   s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG dir diru s" app.f" IN ARG
   s" -o" ARG dir diru s" app-hb" IN ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

\ Of the entries in COLD-NAMES, those still in the root.
: TALLY-COLD ( -- )
   TALLY-RESET
   0 CURSOR !
   begin COLD-NAMES COLD-U @ 10 CURSOR @ SPLIT-NEXT while
      CURSOR ! TALLY
   repeat drop 2drop ;

: BACKDATE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu OBJECT? 0= if exit then
   name nameu ENTRY$ ARG ;

\ Date every entry in the root to 2020, as src/lib/build-cache-retain-test.f
\ dates its planted entries, and remove the sweep stamps: a prune runs at
\ most once per SWEEP-SECONDS (src/lib/build-cache.f, SWEEPS ARE THROTTLED).
: BACKDATE-ALL ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" -t" ARG s" 202001010000" ARG
   CACHE-PATH CACHE-U @ [: BACKDATE ;] FS-LIST:EACH
   s" /usr/bin/touch" s" " CAPTURE SUCCESS
   s" build-cache-meta" ENTRY$ REMOVE-TREE ;

\ 5. After a build, hb-build removes the entries nothing has used for a day.
\ A cold build's entries are dated to 2020, then an edit renames LIB-Y: the
\ rebuild compiles LIB-W, hits the other three, and the prune takes LIB-Y's
\ entry and keeps the three hit, which their use dated, and the one stored.
: PRUNE-CASE ( -- )
   s" hb-build prunes the entries nothing used for a day" T-LABEL
   s" prune" CASE-DIR
   s" prune" s" lib.f" LIB-V1$ WRITE-IN
   s" prune" s" app.f" APP$ WRITE-IN
   s" prune" HB-BUILD SUCCESS
   CACHE-PATH CACHE-U @ COLD-NAMES NAMES-CAP FS-LIST:NAMES COLD-U !
   BACKDATE-ALL
   TIME:EPOCH-SECONDS SINCE !
   TALLY-COLD
   OBJECT-N @ 2 T=
   STALE-N @ 2 T=
   s" prune" s" lib.f" LIB-V3$ WRITE-IN
   s" prune" HB-BUILD SUCCESS
   \ A file key names the file's place in the load, not its text: lib.f's
   \ edit restores its object under the cold name, and app.f's is reused or
   \ restored there too, so both cold names stay, newly dated.
   TALLY-COLD
   OBJECT-N @ 2 T=
   STALE-N @ 0 T=
   TALLY-ROOT
   OBJECT-N @ 2 T=
   STALE-N @ 0 T= ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-storage" TREE
   KEEP-CASE
   ALTERNATE-CASES
   CONCURRENT-CASE
   BYTE-CASE
   PRUNE-CASE
   T-REPORT
   s" native file storage artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STORAGE-TEST:MAIN
