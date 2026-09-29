\ stripped-entry-lib.f - the fixture the stripped-entry gate rows share: the
\ hostile subject whose private, public and global words collide by name, the
\ stripped build through tools/hb-build.f, the image run, and the refusal check
\ on the maker child alone.
\ Loaded by test/stripped-entry.f and test/stripped-entry-qualified.f. It runs
\ nothing; each row reopens package STRIPPED-ENTRY-TEST and runs the cases it
\ owns in a tree of its own.
require test/gate-common.f
require lib/engine-candidate.f

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
: SEED$ ( -- ptr u8 n ) s" 000000000000002a" ;

: COLLISIONS$ ( -- ptr u8 n )
   S\" package STRIPPED-ENTRY-HOSTILE\nprivate\n: MAIN ( -- ) s\q private-main\q type cr ;\n: HLP ( n -- ) drop s\q private-helper\q type cr ;\n: SECRET ( n -- ) drop ;\npublic\n: MAIN ( -- ) s\q public-main\q type cr ;\n: HLP ( n -- ) 42 <> if -9061 throw then s\q public-helper\q type cr ;\n;package\n: HLP ( n -- ) 42 <> if -9062 throw then s\q global-helper\q type cr ;\n: MAIN ( -- ) s\q global-main\q type cr ;\n" ;

: NO-GLOBAL$ ( -- ptr u8 n )
   S\" package STRIPPED-ENTRY-NO-GLOBAL\nprivate\n: MAIN ( -- ) ;\npublic\n: MAIN ( -- ) ;\n;package\n" ;

: WRITE-SUBJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SUBJECT$ a u WRITE-ALL ;

: PREPARE ( ptr u8 n -- )
   GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" hb-aot-got" GOT GT-PATH GOT-U !
   COLLISIONS$ WRITE-SUBJECT ;

: BUILD ( ptr u8 n -- ) {: entry:ptr entryu:n :}
   IMAGE$ EXISTS? if IMAGE$ REMOVE-FILE then
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+
   s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+
   entryu 0 > if
      s" --preseed-entry" GE-ARG+ entry entryu GE-ARG+
      s" --preseed-seed" GE-ARG+ SEED$ GE-ARG+
   then
   SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
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
\ argv and stdin. The linker resolves the entry and refuses a missing one
\ (src/habu/aot-closure.f), so a refusal needs nothing the tool loads. The
\ preseed builds prove the tool forwards the entry, and
\ tools/hb-build-stripped-test.f HBT-STRIPPED-NO-ENTRY that it passes the
\ refusal's exit 74 and diagnostic through. The maker writes GOT$.
: MAKER-BUILD ( ptr u8 n -- ) {: entry:ptr entryu:n :}
   GOT$ EXISTS? if GOT$ REMOVE-FILE then
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   entryu 0 > if entry entryu GE-ARG+ SEED$ GE-ARG+ then
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
   TIMEOUT-MS GE-RUN-STDIN ;

: REFUSED ( ptr u8 n -- ) {: label:ptr labelu:n :}
   74 label labelu GE-EXPECT-RC
   s" aot: entry word not found:" label labelu GE-EXPECT-ERR-HAS
   GOT$ EXISTS? if label labelu GE-FAIL then ;

;package
