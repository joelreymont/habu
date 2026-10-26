\ Resolve the stripped root MAIN by the same global token as the engine: the
\ default build of a subject whose private, public and global MAINs collide by
\ name runs the global one, and a subject with no global MAIN is refused by the
\ maker child alone.
require test/gate-common.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f

package STRIPPED-ENTRY-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
create GOT FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
variable GOT-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: GOT$ ( -- ptr u8 n ) GOT GOT-U @ ;

: COLLISIONS$ ( -- ptr u8 n )
   S\" package STRIPPED-ENTRY-HOSTILE\nprivate\n: MAIN ( -- ) s\q private-main\q type cr ;\npublic\n: MAIN ( -- ) s\q public-main\q type cr ;\n;package\n: MAIN ( -- ) s\q global-main\q type cr ;\n" ;

: NO-GLOBAL$ ( -- ptr u8 n )
   S\" package STRIPPED-ENTRY-NO-GLOBAL\nprivate\n: MAIN ( -- ) ;\npublic\n: MAIN ( -- ) ;\n;package\n" ;

: WRITE-SUBJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SUBJECT$ a u WRITE-ALL ;

: PREPARE ( -- )
   s" stripped-entry" GT-START
   AOT-BUILDER-ENGINE:ENSURE
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" hb-aot-got" GOT GT-PATH GOT-U !
   COLLISIONS$ WRITE-SUBJECT ;

: BUILD ( -- )
   IMAGE$ EXISTS? if IMAGE$ REMOVE-FILE then
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+
   s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+
   SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS GE-RUN-ENV ;

: BUILT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GE-EXPECT-OK
   GT-ERR$ nip 0<> if label labelu GE-FAIL then
   IMAGE$ EXECUTABLE? 0= if label labelu GE-FAIL then ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: want:ptr wantu:n label:ptr labelu:n :}
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   GT-ERR$ nip 0<> if label labelu GE-FAIL then
   want wantu label labelu GE-EXPECT-OUT ;

\ The maker child tools/hb-build-lib.f HBB-RUN-MAKER-CMD starts, with the same
\ argv and stdin. The linker resolves MAIN and refuses a missing one
\ (src/habu/aot-closure.f), so a refusal needs nothing the tool loads.
\ tools/hb-build-stripped-test.f HBT-STRIPPED-NO-ENTRY proves the tool passes
\ the refusal's exit 74 and diagnostic through. The maker writes GOT$.
: MAKER-BUILD ( -- )
   GOT$ EXISTS? if GOT$ REMOVE-FILE then
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   GE-HB-RESET
   maker makeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   maker makeru
   AOT-BUILDER-ENGINE:STDIN$
   TIMEOUT-MS GE-RUN-STDIN ;

: REFUSED ( ptr u8 n -- ) {: label:ptr labelu:n :}
   74 label labelu GE-EXPECT-RC
   s" aot: entry word not found:" label labelu GE-EXPECT-ERR-HAS
   GOT$ EXISTS? if label labelu GE-FAIL then ;

: DEFAULT-ENTRY ( -- )
   BUILD
   s" stripped default global MAIN build" BUILT
   S\" global-main\n" s" stripped default global MAIN run" RUN-IMAGE ;

: MISSING-GLOBAL ( -- )
   NO-GLOBAL$ WRITE-SUBJECT
   MAKER-BUILD
   s" stripped missing global MAIN refused" REFUSED ;

: BODY ( -- )
   PREPARE
   DEFAULT-ENTRY
   MISSING-GLOBAL
   s" PASS: stripped global entry identity" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
