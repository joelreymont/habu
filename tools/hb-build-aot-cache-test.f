\ hb-build-aot-cache-test.f - checked fixture for tools/hb-build-lib.f: the
\ AOT groups about the object cache and its keys - the preseeded entry, the
\ tier refusal, a stored object's hit, one body behind an EXPORT, the engine
\ and producer keys and a wrong object. tools/hb-build-test-lib.f lists the
\ other hb-build rows.
\ Run: bin/hb --load tools/hb-build-aot-cache-test.f

require tools/hb-build-test-lib.f

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
   HBB-ERR-BUF erru s" executable build requires native tier 1" CONTAINS? TTRUE ;

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
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   CODE ASM-LEN OBJ:TEXT+
   s" MAIN" s" --" OBJ:EXPORT+
   s" MAIN" 0 s" --" OBJ:DEF+ ;

: HBT-STORE-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBB-MAKER-KEY!
   HBT-TMP OBJRES:ROOT!
   HBB-TARGET-ABI$ HBT-BUILD-EXIT-OBJ
   OBJRES:STORE nip HBT-KEY-U T= ;

: HBT-STORE-WRONG-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBB-MAKER-KEY!
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
\ changes the bytes. Both variants build to the SAME output path so the ad-hoc
\ signature identifier cannot differ; the alias variant also runs, proving
\ both names execute the one body.
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
   HBB-MAKER-KEY!
   HBB-CHECKER-ABI$ s" checker-effect-v1+" STARTS-WITH? TTRUE
   HBB-COMPILER-ABI$ s" hb-arm64-v1+" STARTS-WITH? TTRUE
   HBB-CHECKER-ABI$ nip 82 T=
   HBB-COMPILER-ABI$ nip 76 T=
   HBT-ABI-MAKER-SUFFIX ;

\ The object this loads is the one HBT-BUILD-AOT-OBJECT-HIT stored, so this
\ case runs after that one in the same scratch tree.
: HBT-ENGINE-KEY-FLIP ( -- )
   HBT-ABI-MAKER-QUALIFIED
   HBT-WRITE-ENG
   HBT-OBJ-LOAD? TTRUE
   HBT-ENG BF-ENGINE!
   HBT-OBJ-LOAD? TFALSE
   BF-ENGINE-RESET
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

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-CACHE-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   BUILD-AOT-PRESEED
   HBT-AOT-JIT-REJECT
   HBT-BUILD-AOT-OBJECT-HIT
   HBT-BUILD-AOT-EXPORT-ONE-BODY
   HBT-ENGINE-KEY-FLIP
   HBT-PRODUCER-KEY-MISS
   HBT-BUILD-AOT-WRONG-OBJECT-FAILS
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-cache-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-CACHE-MAIN
