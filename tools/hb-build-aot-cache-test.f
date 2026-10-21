\ hb-build-aot-cache-test.f - checked fixture for tools/hb-build-lib.f: the
\ AOT groups about the object cache and its keys - the tier refusal, a stored
\ object's hit, a call through an EXPORT alias, the engine and producer keys, a
\ wrong object and a cache hit's failed publication.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-aot-cache-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require test/linker-builder-engine.f

package HB-BUILD-CLI
: HBT-LOAD-EXIT ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" tools/hb-build-aot-cache-x64.f" required exit
   then
   s" tools/hb-build-aot-cache-arm.f" required ;
' HBT-LOAD-EXIT
;package
execute

using BUILD-FIXPOINT                     \ the build tmp root and engine override

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

variable HBT-ENG-U
create HBT-ENG-BUF FS-PATH-CAP allot
create HBT-ABI-ALT 96 allot
create HBT-SRC-KEY 80 allot
create HBT-FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context

create HBT-MAP-HEX 64 allot
create HBT-OUTER-ROWS FS-PATH-CAP 2 * allot
variable HBT-OUTER-CANON-N
variable HBT-OUTER-READ-N
create HBT-PAIR-MAP FS-PATH-CAP allot
variable HBT-PAIR-MAP-U
create HBT-PAIR-EXE-HEX 64 allot
create HBT-PAIR-MAP-HEX 64 allot
create HBT-PAIR-GOT-HEX 64 allot

: HBT-BOUND-MAP ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n path:ptr pathu:n :}
   HBT-FSHA-CTX image imageu HBT-MAP-HEX SHA256-FILE-HEX-IN 0 T=
   SB-RESET
   S\" habu-names 1\ncolumns rec named start len wid name kind origin source\nimage-sha256 " SB-APPEND
   HBT-MAP-HEX 64 SB-APPEND
   10 SB-APPEND-C
   path pathu SB$ WRITE-ALL ;

: HBT-AOT-JIT-REJECT ( -- )
   HBT-REPL-BAD-SRC s" 0 set-tier : MAIN ( -- ) ;" WRITE-ALL
   HBT-REPL-BAD-SRC HBT-RUN-MAKER {: outu:n erru:n rc:n :}
   rc 70 T=
   HB-TARGET-LINUX-X86-64? if
      HBB-ERR-BUF erru s" set-tier: x86-64 runs tier 1 only" CONTAINS? TTRUE
   else
      HBB-ERR-BUF erru s" executable build requires native tier 1" CONTAINS? TTRUE
   then ;

: HBT-AOT-SOURCE-KEY! ( -- )
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$
   HBT-SRC-KEY OBJIDX:SOURCE-KEY-HEX ;

: HBT-BUILD-EXIT-OBJ ( ptr u8 n -- )
   {: target:ptr targetu:n :}
   \ These injected objects have a self-contained source, with no dynamic loads.
   HBT-AOT-SRC HBB-SRC!
   HBB-SRC-CLOSURE-HEX!
   HBB-MAKER-KEY!
   HBB-AOT-INPUTS-PATH!
   NFILE-INPUTS:RESET
   HBT-AOT-SRC s" " NFILE-INPUTS:ADD-MTIME
   HBB-AOT-INPUTS$ NFILE-INPUTS:BYTES$ ATOMIC-WRITE-FILE
   NFILE-INPUTS:CLOSE
   HBT-AOT-HEX!
   OBJ:RESET
   HBT-AOT-HEX HBT-KEY-U OBJ:SOURCE!
   target targetu OBJ:TARGET!
   HBB-CHECKER-ABI$ OBJ:CHECKER!
   HBB-COMPILER-ABI$ OBJ:COMPILER!
   HBT-ADD-EXIT-TEXT
   s" MAIN" s" --" OBJ:EXPORT+
   s" MAIN" 0 s" --" OBJ:DEF+ ;

: HBT-STORE-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   HBB-TARGET-ABI$ HBT-BUILD-EXIT-OBJ
   OBJRES:STORE nip HBT-KEY-U T= ;

: HBT-STORE-WRONG-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   s" wrong-aarch64" HBT-BUILD-EXIT-OBJ
   OBJSTORE:STORE {: key:ptr keyu:n :}
   HBT-AOT-SOURCE-KEY!
   HBT-SRC-KEY HBT-KEY-U key keyu OBJIDX:STORE ;

: HBT-PAIR-HASH ( ptr u8 n ptr u8 -- )
   {: path:ptr pathu:n hex:ptr :}
   HBT-FSHA-CTX path pathu hex SHA256-FILE-HEX-IN 0 T= ;

: HBT-PAIR-RUN ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
   HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
   RUN-CAPTURE HBT-CAPTURE>N {: outu:n erru:n rc:n :}
   rc 0 T= erru 0 T=
   HBT-RUN-OUT outu want wantu T$= ;

: HBT-PAIR-SAME ( -- )
   HBT-AOT-OUT HBT-PAIR-GOT-HEX HBT-PAIR-HASH
   HBT-PAIR-GOT-HEX 64 HBT-PAIR-EXE-HEX 64 T$=
   HBT-AOT-OUT HBB-MAP-DST!
   HBB-MAP-DST$ HBT-PAIR-GOT-HEX HBT-PAIR-HASH
   HBT-PAIR-GOT-HEX 64 HBT-PAIR-MAP-HEX 64 T$=
   S\" 222\n\n25\n\n25\n\n" HBT-PAIR-RUN ;

\ An EXPORT alias in a stripped AOT program (dot habu-compiler-pkg-re-688212c1):
\ a program calling a word through its defining package and through a
\ re-exported alias builds, runs clean and prints the body's result once per
\ name. Both source-race programs exercise these calls with distinct markers.
: HBT-PAIR-SRC$ ( ptr u8 n -- ptr u8 n )
   {: marker:ptr markeru:n :}
   SB-RESET
   s" package XA" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" : W ( i64 -- i64 ) dup * ;" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C
   s" package XB" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" EXPORT XA:W" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C
   s" : MAIN ( -- ) " SB-APPEND marker markeru SB-APPEND
   s"  . cr 5 XA:W . cr 5 XB:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-ENG ( -- ptr u8 n )
   HBT-ENG-BUF HBT-ENG-U @ ;

: HBT-WRITE-ENG ( -- )
   HBT-ROOT s" engine-alt" HBT-ENG-BUF HBT-ENG-U HBT-PATH!
   HBT-ENG s" not-the-real-engine" WRITE-ALL ;

: HBT-ABI-MAKER-SUFFIX ( -- )
   HBB-CHECKER-ABI$ {: a:ptr u:n :}
   a u HBT-KEY-U - BYTE+ HBT-KEY-U HBB-MAKER-KEY-HEX HBT-KEY-U STR= TTRUE ;

: HBT-ABI-MAKER-QUALIFIED ( -- )
   HBB-RESET-OPTIONS
   HBB-MAKER-KEY!
   HBB-CHECKER-ABI$ s" checker-effect-v1+" STARTS-WITH? TTRUE
   HB-TARGET-LINUX-X86-64? if
      HBB-COMPILER-ABI$ s" hb-x86-64-v1+" STARTS-WITH? TTRUE
      HBB-COMPILER-ABI$ nip 77 T=
   else
      HBB-COMPILER-ABI$ s" hb-arm64-v1+" STARTS-WITH? TTRUE
      HBB-COMPILER-ABI$ nip 76 T=
   then
   HBB-CHECKER-ABI$ nip 82 T=
   HBT-ABI-MAKER-SUFFIX ;

\ Store an object under the row's engine, the keyed linker image selected for
\ every case. The flip back restores that engine rather than the default one.
: HBT-ENGINE-KEY-FLIP ( -- )
   HBT-STORE-AOT-OBJ
   HBT-ABI-MAKER-QUALIFIED
   HBT-WRITE-ENG
   HBT-OBJ-LOAD? TTRUE
   HBT-ENG BF-ENGINE!
   HBT-OBJ-LOAD? TFALSE
   HBT-LINKER HBT-ENGINE!
   HBT-OBJ-LOAD? TTRUE ;

: HBT-ALT-CHECKER$ ( -- ptr u8 n )
   HBB-CHECKER-ABI$ {: a:ptr u:n :}
   a HBT-ABI-ALT u BYTE-COPY
   HBT-ABI-ALT u 1 - + c@ STR-ZERO = if STR-ZERO 1 + else STR-ZERO then
   HBT-ABI-ALT u 1 - + c!
   HBT-ABI-ALT u ;

: HBT-PRODUCER-KEY-MISS ( -- )
   HBB-RESET-OPTIONS
   HBB-MAKER-KEY!
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-ALT-CHECKER$ {: alt:ptr altu:n :}
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ alt altu HBB-COMPILER-ABI$ OBJRES:LOAD TFALSE ;

: HBT-OUTER-CANON ( ptr u8 n -- ptr u8 n bool )
   1 HBT-OUTER-CANON-N +! SOURCE-ROOT:CANON-OS ;
: HBT-OUTER-READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   1 HBT-OUTER-READ-N +! SOURCE-ROOT:READ-OS ;

: HBT-MAKER-OUTER-HOLDS ( n n -- )
   {: end:n active:n :}
   NFILE-INPUTS:MARK end T=
   NFILE-INPUTS:BYTES$ nip active T=
   0 end NFILE-INPUTS:SPAN HBT-OUTER-ROWS end T$=
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK
   0 HBT-OUTER-CANON-N ! 0 HBT-OUTER-READ-N !
   HBT-AOT-SRC SOURCE-INPUT:CANON 2drop drop
   HBT-AOT-SRC s" " SOURCE-INPUT:READ 2drop
   HBT-OUTER-CANON-N @ 1 T=
   HBT-OUTER-READ-N @ 1 T= ;

\ Model the input range and loader pair owned by an enclosing
\ NDEF-CACHE:LOAD-WITH-INPUTS. Maker selection returns both even when
\ publishing the selector fails after discovery. A private root by claim: the
\ case deletes and damages the maker-support record, which is keyed by engine
\ only, so in the run's root it would break concurrent rows on that engine.
: HBT-MAKER-NESTED-INPUTS ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBB-RESET-OPTIONS
   HBB-MAKER-KEY!
   HBB-MAKER-SUPPORT-PATH$ REMOVE-FILE
   NFILE-INPUTS:RESET
   HBT-AOT-SRC s" " NFILE-INPUTS:ADD-CONTENT
   NFILE-INPUTS:MARK NFILE-INPUTS:FROM
   HBT-AOT-SRC s" " NFILE-INPUTS:ADD-CONTENT
   NFILE-INPUTS:MARK {: end:n :}
   NFILE-INPUTS:BYTES$ nip {: active:n :}
   0 end NFILE-INPUTS:SPAN drop HBT-OUTER-ROWS end BYTE-COPY
   [: HBT-OUTER-CANON ;] [: HBT-OUTER-READ ;] SOURCE-INPUT:USE
   HBB-MAKER-KEY!
   end active HBT-MAKER-OUTER-HOLDS
   HBB-MAKER-SUPPORT-PATH$ REMOVE-FILE
   HBB-MAKER-SUPPORT-PATH$ MAKE-DIR
   [: HBB-MAKER-KEY! ;] E-FS-IO TTHROWSQ
   end active HBT-MAKER-OUTER-HOLDS
   HBB-MAKER-SUPPORT-PATH$ REMOVE-TREE
   SOURCE-INPUT:RESET
   NFILE-INPUTS:CLOSE
   BUILD-CACHE:RESET ;

: HBT-BUILD-AOT-WRONG-OBJECT-FAILS ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-AOT-OUT FILE? if HBT-AOT-OUT REMOVE-FILE then
   HBT-STORE-WRONG-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] E-OBJ-SCHEMA TTHROWSQ
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-AOT-OUT EXISTS? TFALSE ;

\ A PUBLICATION THAT FAILS LEAVES -o AS IT WAS. A cache hit replaces -o through
\ a sibling (src/lib/fs-mutate.f REPLACE-STAGED). An unreadable artifact fails
\ before publication; an object with no text fails the image writer. A sibling
\ left behind is an extra file beside the executable and its map.
73 constant HBT-EXEC-ONLY                  \ 0111: runnable, unreadable
493 constant HBT-EXEC-READ                 \ 0755
16 constant HBT-PUB-CAP
HBT-PUB-CAP BUFFER: HBT-PUB-BUF
FS-PATH-CAP SPAN-BUFFER: HBT-PUB-LINK
FS-PATH-CAP BUFFER: HBT-PUB-DIR-BUF
FS-PATH-CAP BUFFER: HBT-PUB-OUT-BUF
variable HBT-PUB-DIR-U
variable HBT-PUB-OUT-U
variable HBT-PUB-FILES

: HBT-PUB-DIR ( -- ptr u8 n )
   HBT-PUB-DIR-BUF HBT-PUB-DIR-U @ ;

: HBT-PUB-OUT ( -- ptr u8 n )
   HBT-PUB-OUT-BUF HBT-PUB-OUT-U @ ;

: HBT-PUB-COUNT ( ptr u8 n -- )
   2drop HBT-PUB-FILES @ 1+ HBT-PUB-FILES ! ;

: HBT-PUB-ALONE ( n -- ) {: count:n :}
   0 HBT-PUB-FILES !
   HBT-PUB-DIR [: HBT-PUB-COUNT ;] WALK-FILES
   HBT-PUB-FILES @ count T= ;

: HBT-PUB-HOLDS ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-PUB-OUT FILE? {: there:bool :}
   there TTRUE
   there 0= if exit then
   HBT-PUB-OUT HBT-PUB-BUF HBT-PUB-CAP READ-ALL {: u:n :}
   HBT-PUB-BUF u want wantu T$= ;

: HBT-PUB-MAP-HOLDS ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-PUB-OUT HBB-MAP-DST!
   HBB-MAP-DST$ HBT-PUB-BUF HBT-PUB-CAP READ-ALL {: u:n :}
   HBT-PUB-BUF u want wantu T$= ;

: HBT-PUB-OLD-MAP ( -- )
   HBT-PUB-OUT HBB-MAP-DST!
   HBB-MAP-DST$ s" prior-map" WRITE-ALL ;

: HBT-PUB-SET-MAKER ( -- )
   HBT-TMP BF-TMP!
   HBB-GOT-NAME$ BF-A$ s" source-image" WRITE-ALL
   HBB-GOT-NAME$ BF-A$ HBB-MAP-SRC!
   HBB-MAP-SRC$ s" source-map" WRITE-ALL ;

: HBT-PUB-MAKER-CLEAN ( -- )
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   HBB-GOT-NAME$ BF-A$ REMOVE-FILE
   BF-TMP-RESET ;

\ The artifact and maker both reach the real install paths. A directory at -o
\ refuses the final executable rename after the new map has been published.
\ The prior map's bytes and the directory must survive both failures.
: HBT-PUB-PAIR-RENAME-FAILS ( -- )
   HBT-PUB-OLD-MAP
   HBT-PUB-SET-MAKER
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   [: HBB-INSTALL-OUT ;] E-BUILD-PATH TTHROWSQ
   s" previous" HBT-PUB-HOLDS
   s" prior-map" HBT-PUB-MAP-HOLDS
   2 HBT-PUB-ALONE
   HBT-PUB-SET-MAKER
   HBB-GOT-NAME$ BF-A$ HBT-EXEC-ONLY CHMOD-MODE
   [: HBB-INSTALL-OUT ;] E-FS-OPEN TTHROWSQ
   HBB-GOT-NAME$ BF-A$ HBT-EXEC-READ CHMOD-MODE
   s" previous" HBT-PUB-HOLDS
   s" prior-map" HBT-PUB-MAP-HOLDS
   2 HBT-PUB-ALONE
   HBT-PUB-OUT REMOVE-FILE
   HBT-PUB-OUT MAKE-DIR
   70 0 ?do [: HBB-RESTORE-PAIR ;] E-BUILD-INSTALL TTHROWSQ loop
   HBT-PUB-OUT DIR? TTRUE
   s" prior-map" HBT-PUB-MAP-HOLDS
   1 HBT-PUB-ALONE
   [: HBB-INSTALL-OUT ;] E-BUILD-INSTALL TTHROWSQ
   HBT-PUB-OUT DIR? TTRUE
   s" prior-map" HBT-PUB-MAP-HOLDS
   1 HBT-PUB-ALONE
   HBT-PUB-OUT HBB-MAP-DST!
   HBB-MAP-DST$ REMOVE-FILE
   s" missing-target" HBB-MAP-DST$ MAKE-SYMLINK
   [: HBB-RESTORE-PAIR ;] E-BUILD-INSTALL TTHROWSQ
   HBB-MAP-DST$ SYMLINK? TTRUE
   HBB-MAP-DST$ HBT-PUB-LINK READ-LINK {: linku:n :}
   HBT-PUB-LINK linku SPAN:TAKE SPAN:$ s" missing-target" T$=
   HBB-MAP-DST$ REMOVE-FILE
   [: HBB-RESTORE-PAIR ;] E-BUILD-INSTALL TTHROWSQ
   HBB-MAP-DST$ FS-TRY-LSTAT TFALSE
   0 HBT-PUB-ALONE
   HBT-PUB-MAKER-CLEAN
   HBT-PUB-OUT REMOVE-DIR
   HBT-REMOVE-ARTIFACT
   HBT-PUB-DIR REMOVE-DIR ;

: HBT-NO-TEXT-OBJ ( -- )
   HBT-AOT-HEX!
   OBJ:RESET
   HBT-AOT-HEX HBT-KEY-U OBJ:SOURCE!
   HBB-TARGET-ABI$ OBJ:TARGET!
   HBB-CHECKER-ABI$ OBJ:CHECKER!
   HBB-COMPILER-ABI$ OBJ:COMPILER! ;

: HBT-PUB-PREPARE ( -- )
   HBT-ROOT s" publish" HBT-PUB-DIR-BUF HBT-PUB-DIR-U HBT-PATH!
   HBT-PUB-DIR s" out" HBT-PUB-OUT-BUF HBT-PUB-OUT-U HBT-PATH!
   HBT-PUB-DIR MAKE-DIR
   HBT-PUB-OUT s" previous" WRITE-ALL
   HBB-RESET-OPTIONS
   HBT-AOT-SRC HBT-PUB-OUT HBB-PATHS!
   HBB-PREPARE-ARTIFACT-CACHE
   -1 HBB-ARTIFACT-CACHE !
   HBB-ARTIFACT$ s" artifact" WRITE-ALL
   HBB-ARTIFACT$ HBB-MAP-SRC!
   HBB-ARTIFACT$ HBB-MAP-SRC$ HBT-BOUND-MAP ;

: HBT-PUBLISH-FAILS-CLEAN ( -- )
   HBT-PUB-PREPARE
   HBT-PUB-OLD-MAP
   s" an unreadable artifact reports its hash error and leaves -o" T-LABEL
   HBB-ARTIFACT$ HBT-EXEC-ONLY CHMOD-MODE
   [: HBB-RESTORE-ARTIFACT? drop ;] SHA-E-OPEN TTHROWSQ
   s" previous" HBT-PUB-HOLDS
   s" prior-map" HBT-PUB-MAP-HOLDS
   2 HBT-PUB-ALONE
   s" a restore replaces -o whole and runnable" T-LABEL
   HBB-ARTIFACT$ HBT-EXEC-READ CHMOD-MODE
   HBB-RESTORE-ARTIFACT? TTRUE
   s" artifact" HBT-PUB-HOLDS
   HBT-PUB-OUT EXECUTABLE? TTRUE
   2 HBT-PUB-ALONE
   s" an object image the writer refuses leaves -o" T-LABEL
   HBT-NO-TEXT-OBJ
   [: HBB-WRITE-OBJECT? drop ;] E-OBJ-SCHEMA TTHROWSQ
   s" artifact" HBT-PUB-HOLDS
   2 HBT-PUB-ALONE
   HBT-REMOVE-ARTIFACT ;

\ A real maker has read A when the source becomes B. Its result keeps A's
\ source key, so the next ordinary build compiles and runs B.
: HBT-SOURCE-CHANGED-DURING-MAKER ( -- )
   HBT-AOT-SRC s" 111" HBT-PAIR-SRC$ WRITE-ALL
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD-BEGIN
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-RUN-MAKER
   HBB-MAKER-RUN @ 0<> TTRUE
   HBT-AOT-SRC s" 222" HBT-PAIR-SRC$ WRITE-ALL
   HBB-STORE-OBJECT
   HBB-OBJECT-STORE @ 0<> TTRUE
   HBB-INSTALL-OUT
   HBB-INSTALL-ARTIFACT
   S\" 111\n\n25\n\n25\n\n" HBT-PAIR-RUN
   HBT-ROOT s" old.map" HBT-PAIR-MAP HBT-PAIR-MAP-U HBT-PATH!
   HBB-ARTIFACT$ HBB-MAP-SRC!
   HBB-MAP-SRC$ HBT-PAIR-MAP HBT-PAIR-MAP-U @ COPY-FILE-STREAM
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0<> TTRUE
   HBB-OBJECT-STORE @ 0<> TTRUE
   S\" 222\n\n25\n\n25\n\n" HBT-PAIR-RUN
   \ Interrupted publication can pair B's executable with A's valid map.
   s" a mismatched cached pair is rebuilt before an identical cache restore" T-LABEL
   HBT-AOT-OUT HBT-PAIR-EXE-HEX HBT-PAIR-HASH
   HBT-AOT-OUT HBB-MAP-DST!
   HBB-MAP-DST$ HBT-PAIR-MAP-HEX HBT-PAIR-HASH
   HBB-ARTIFACT$ HBB-MAP-SRC!
   HBT-PAIR-MAP HBT-PAIR-MAP-U @ HBB-MAP-SRC$ COPY-FILE-STREAM
   HBT-REMOVE-AOT-OUT
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0<> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HBT-PAIR-SAME
   HBT-REMOVE-AOT-OUT
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0<> TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-PAIR-SAME
   HBT-REMOVE-ARTIFACT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-AOT-OUT HBT-REMOVE-FILE? ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-CACHE-MAIN ( -- )
   T-RESET
   LINKER-BUILDER-ENGINE:ENSURE LINKER-BUILDER-ENGINE:HOST$ HBT-KEYED!
   HBT-LINKER HBT-ENGINE!
   HBT-PREPARE
   HBT-PUB-PREPARE
   HBT-PUB-PAIR-RENAME-FAILS
   HBT-AOT-JIT-REJECT
   HBT-ENGINE-KEY-FLIP
   HBT-PRODUCER-KEY-MISS
   HBT-MAKER-NESTED-INPUTS
   HBT-BUILD-AOT-WRONG-OBJECT-FAILS
   HBT-PUBLISH-FAILS-CLEAN
   HBT-SOURCE-CHANGED-DURING-MAKER
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-cache-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-CACHE-MAIN
