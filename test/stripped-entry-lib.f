\ stripped-entry-lib.f - the fixture the stripped-entry gate rows share: the
\ hostile subject whose private, public and global words collide by name, the
\ stripped build through tools/hb-build.f, the image run and the refusal check.
\ Loaded by test/stripped-entry.f and test/stripped-entry-qualified.f. It runs
\ nothing; each row reopens package STRIPPED-ENTRY-TEST and runs the cases it
\ owns in a tree of its own.
require test/gate-common.f
require lib/engine-candidate.f

package STRIPPED-ENTRY-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

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
      s" --preseed-seed" GE-ARG+ s" 000000000000002a" GE-ARG+
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

: REFUSED ( ptr u8 n -- ) {: label:ptr labelu:n :}
   74 label labelu GE-EXPECT-RC
   s" aot: entry word not found:" label labelu GE-EXPECT-ERR-HAS
   IMAGE$ EXISTS? if label labelu GE-FAIL then ;

;package
