\ hb-build-aot-cache-test.f - checked fixture for tools/hb-build-lib.f: the
\ AOT groups about the object cache and its keys - the preseeded entry, the
\ tier refusal, a stored object's hit, one body behind an EXPORT, the engine
\ and producer keys, a wrong object and a cache hit's failed publication.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-aot-cache-test.f

require tools/hb-build-test-lib.f
require src/arch/arm64/icode.f
require test/preloaded-engine.f

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

variable HBT-EXP-SRC-U
variable HBT-EXP-OUT-U
create HBT-EXP-SRC-BUF FS-PATH-CAP allot
create HBT-EXP-OUT-BUF FS-PATH-CAP allot
create HBT-EXP-DG 32 allot
create HBT-EXP-HEX1 64 allot
create HBT-EXP-HEX2 64 allot

: BUILD-AOT-PRESEED ( -- )
   HBT-AOT-SRC s" : ALTERNATE ( n n -- ) 42 <> if -9042 throw then 10 <> if -9043 throw then ; : MAIN ( -- ) -9044 throw ;" WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-ARGV-BASE
   s" --preseed-entry" >LEN PROC-ARGV+
   s" ALTERNATE" >LEN PROC-ARGV+
   s" --preseed-seed" >LEN PROC-ARGV+
   s" 000000000000000A000000000000002a" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0 T= erru 0 T=
   HBT-OUT outu s" hb-build OK" CONTAINS? TTRUE
   HBT-RUN-AOT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

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

: HBT-BUILD-EXIT-OBJ ( ptr u8 n -- ) {: target:ptr targetu:n :}
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
   HBT-TMP OBJRES:ROOT!
   HBB-TARGET-ABI$ HBT-BUILD-EXIT-OBJ
   OBJRES:STORE nip HBT-KEY-U T= ;

: HBT-STORE-WRONG-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   s" wrong-aarch64" HBT-BUILD-EXIT-OBJ
   OBJSTORE:STORE {: key:ptr keyu:n :}
   HBT-AOT-SOURCE-KEY!
   HBT-SRC-KEY HBT-KEY-U key keyu OBJIDX:STORE ;

: HBT-BUILD-AOT-OBJECT-HIT ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-STORE-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HBT-AOT-OUT FILE? TTRUE
   HBT-RUN-AOT ;

\ EXPORT keeps one body (dot habu-compiler-pkg-re-688212c1): the stripped AOT
\ binary carries no names, so a program calling a word through its defining
\ package AND a re-exported alias must be byte-identical to the same program
\ calling the defining name twice — a second body or a diverged call target
\ changes the bytes. The signature identifier is no variable here: a stripped
\ image is signed with src/habu/sign-id.f SIGN-ID:PROG$, whatever its path.
\ The alias variant also runs, proving both names execute the one body.
: HBT-EXP-SRC ( -- ptr u8 n )
   HBT-EXP-SRC-BUF HBT-EXP-SRC-U @ ;

: HBT-EXP-OUT ( -- ptr u8 n )
   HBT-EXP-OUT-BUF HBT-EXP-OUT-U @ ;

: HBT-EXP-COMMON ( -- )
   s" package XA" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" : W ( i64 -- i64 ) dup * ;" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C ;

: HBT-EXP-REF-SRC$ ( -- ptr u8 n )
   SB-RESET
   HBT-EXP-COMMON
   s" : MAIN ( -- ) 5 XA:W . cr 5 XA:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-EXP-ALIAS-SRC$ ( -- ptr u8 n )
   SB-RESET
   HBT-EXP-COMMON
   s" package XB" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" EXPORT XA:W" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C
   s" : MAIN ( -- ) 5 XA:W . cr 5 XB:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-EXP-BUILD ( ptr u8 n -- ) {: sa:ptr su:n :}
   HBT-EXP-SRC sa su WRITE-ALL
   HBT-EXP-OUT HBT-REMOVE-FILE?
   HBT-EXP-SRC HBT-EXP-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBT-REMOVE-ARTIFACT ;

: HBT-EXP-HASH ( ptr u8 -- ) {: hex:ptr :}
   HBT-FSHA-CTX HBT-EXP-OUT HBT-EXP-DG SHA256-FILE-IN 0 T=
   HBT-EXP-DG hex SHA256>HEX ;

: HBT-EXP-RUN ( -- )
   HBT-EXP-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn s" 25" CONTAINS? TTRUE ;

: HBT-BUILD-AOT-EXPORT-ONE-BODY ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-ROOT s" exp.f" HBT-EXP-SRC-BUF HBT-EXP-SRC-U HBT-PATH!
   HBT-ROOT s" exp" HBT-EXP-OUT-BUF HBT-EXP-OUT-U HBT-PATH!
   HBT-EXP-REF-SRC$ HBT-EXP-BUILD
   HBT-EXP-HEX1 HBT-EXP-HASH
   HBT-EXP-ALIAS-SRC$ HBT-EXP-BUILD
   HBT-EXP-HEX2 HBT-EXP-HASH
   HBT-EXP-HEX1 64 HBT-EXP-HEX2 64 T$=
   HBT-EXP-RUN
   HBT-EXP-OUT HBT-REMOVE-FILE?
   BF-TMP-RESET ;

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

\ The object this loads is the one HBT-BUILD-AOT-OBJECT-HIT stored, so this
\ case runs after that one in the same scratch tree. It was stored under the
\ row's engine, the keyed linker image HBT-AOT-CACHE-MAIN selects for every
\ case, so the flip back restores that engine rather than the default one.
: HBT-ENGINE-KEY-FLIP ( -- )
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
   HBT-TMP OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-ALT-CHECKER$ {: alt:ptr altu:n :}
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ alt altu HBB-COMPILER-ABI$ OBJRES:LOAD TFALSE ;

: HBT-BUILD-AOT-WRONG-OBJECT-FAILS ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-AOT-OUT FILE? if HBT-AOT-OUT REMOVE-FILE then
   HBT-STORE-WRONG-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] E-OBJ-SCHEMA TTHROWSQ
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-AOT-OUT EXISTS? TFALSE ;

\ A PUBLICATION THAT FAILS LEAVES -o AS IT WAS. A cache hit replaces -o through
\ a sibling (lib/fs-mutate.f REPLACE-STAGED), whether it restores the artifact
\ or writes the cached object's image, so -o names its previous file until the
\ whole new one is in place, and a failure on the way takes the sibling with
\ it. An artifact its owner cannot read fails the restore's copy; an object
\ with no text fails the image writer. -o lives alone in its directory, so a
\ sibling left behind is a second file there.
73 constant HBT-EXEC-ONLY                  \ 0111: runnable, unreadable
493 constant HBT-EXEC-READ                 \ 0755
16 constant HBT-PUB-CAP
HBT-PUB-CAP BUFFER: HBT-PUB-BUF
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

: HBT-PUB-ALONE ( -- )
   0 HBT-PUB-FILES !
   HBT-PUB-DIR [: HBT-PUB-COUNT ;] WALK-FILES
   HBT-PUB-FILES @ 1 T= ;

: HBT-PUB-HOLDS ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-PUB-OUT FILE? {: there:bool :}
   there TTRUE
   there 0= if exit then
   HBT-PUB-OUT HBT-PUB-BUF HBT-PUB-CAP READ-ALL {: u:n :}
   HBT-PUB-BUF u want wantu T$= ;

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
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-PUB-OUT HBB-PATHS!
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-ARTIFACT$ s" artifact" WRITE-ALL ;

: HBT-PUBLISH-FAILS-CLEAN ( -- )
   HBT-PUB-PREPARE
   s" a restore that cannot read the artifact leaves -o" T-LABEL
   HBB-ARTIFACT$ HBT-EXEC-ONLY CHMOD-MODE
   [: HBB-RESTORE-ARTIFACT? drop ;] E-FS-OPEN TTHROWSQ
   s" previous" HBT-PUB-HOLDS
   HBT-PUB-ALONE
   s" a restore replaces -o whole and runnable" T-LABEL
   HBB-ARTIFACT$ HBT-EXEC-READ CHMOD-MODE
   HBB-RESTORE-ARTIFACT? TTRUE
   s" artifact" HBT-PUB-HOLDS
   HBT-PUB-OUT EXECUTABLE? TTRUE
   HBT-PUB-ALONE
   s" an object image the writer refuses leaves -o" T-LABEL
   HBT-NO-TEXT-OBJ
   [: HBB-WRITE-OBJECT ;] E-OBJ-SCHEMA TTHROWSQ
   s" artifact" HBT-PUB-HOLDS
   HBT-PUB-ALONE
   HBT-REMOVE-ARTIFACT ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-CACHE-MAIN ( -- )
   T-RESET
   PRELOADED-ENGINE:LINKER$ APP-IMAGE-ENGINE:PATH$ HBT-KEYED!
   HBT-LINKER HBT-ENGINE!
   HBT-PREPARE
   BUILD-AOT-PRESEED
   HBT-AOT-JIT-REJECT
   HBT-BUILD-AOT-OBJECT-HIT
   HBT-BUILD-AOT-EXPORT-ONE-BODY
   HBT-ENGINE-KEY-FLIP
   HBT-PRODUCER-KEY-MISS
   HBT-BUILD-AOT-WRONG-OBJECT-FAILS
   HBT-PUBLISH-FAILS-CLEAN
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-cache-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-CACHE-MAIN
