\ Resolve stripped roots by the same global/public-qualified token as the engine.
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

: PREPARE ( -- )
   s" stripped-entry" GT-START
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

: DEFAULT-ENTRY ( -- )
   s" " BUILD
   s" stripped default global MAIN build" BUILT
   S\" global-main\n" s" stripped default global MAIN run" RUN-IMAGE ;

: GLOBAL-PRESEED ( -- )
   s" hLp" BUILD
   s" stripped explicit global HLP build" BUILT
   S\" global-helper\n" s" stripped explicit global HLP run" RUN-IMAGE ;

: PUBLIC-PRESEED ( -- )
   s" stripped-entry-hostile:hLp" BUILD
   s" stripped explicit public HLP build" BUILT
   S\" public-helper\n" s" stripped explicit public HLP run" RUN-IMAGE ;

: REFUSALS ( -- )
   s" STRIPPED-ENTRY-HOSTILE:SECRET" BUILD
   s" stripped private entry refused" REFUSED
   NO-GLOBAL$ WRITE-SUBJECT
   s" " BUILD
   s" stripped missing global MAIN refused" REFUSED ;

: BODY ( -- )
   PREPARE
   DEFAULT-ENTRY
   GLOBAL-PRESEED
   PUBLIC-PRESEED
   REFUSALS
   s" PASS: stripped global and public-qualified entry identity" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
