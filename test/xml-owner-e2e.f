\ Run one public XML owner consumer from source and as a native application.
\ Its deterministic edited XML artifacts remain in the printed temporary tree.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/suite-budget.f
require test/pcap-result.f
require test/aot-builder-engine.f

package XML-OWNER-E2E
private

$8000 constant IO-CAP
512 constant FILE-CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create SUBJECT FS-PATH-CAP allot variable SUBJECT-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
create FILE-BYTES FILE-CAP allot
create EXPECTED FILE-CAP allot
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: APP$ ( -- ptr u8 n ) ROOT$ s" xml-owner-app" PATH JOIN-PATH PATH swap ;
: FILE$ ( ptr u8 n -- ptr u8 n ) {: rel:ptr size:n :}
   ROOT$ rel size PATH JOIN-PATH PATH swap ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- )
   {: program size:n input input-size:n :}
   PROC-ENV-INHERIT-MISSING
   program size >LEN SOURCE-ROOT:CWD$ >LEN input input-size >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: MARKER ( ptr u8 n -- ) {: marker size:n :}
   OUT OUT-U @ marker size CONTAINS? TTRUE ;

: SOURCE-ARGS ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   ROOT$ >LEN PROC-ARGV+ ;

: SOURCE-RUN ( -- )
   SOURCE-ARGS
   ENGINE-CANDIDATE:PATH$
   S\" require test/xml-owner-consumer.f\nMAIN\n" RUN-INPUT
   s" public source load opens, edits and cleans multiple parts" T-LABEL
   NEED-OK s" xml-owner-consumer: ok" MARKER ;

: BUILD ( -- )
   AOT-BUILDER-ENGINE:ENSURE
   \ A subject under this run's fresh root keys a new artifact, so every run links.
   s" consumer.f" FILE$ SUBJECT swap dup SUBJECT-U ! BYTE-COPY
   s" test/xml-owner-consumer.f" SUBJECT SUBJECT-U @ COPY-FILE-STREAM
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SUBJECT SUBJECT-U @ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   APP$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$
   s" " RUN-INPUT
   s" the same public consumer builds as a native application" T-LABEL
   NEED-OK APP$ EXECUTABLE? TTRUE ;

: NATIVE-RUN ( -- )
   SOURCE-ARGS
   APP$ s" " RUN-INPUT
   s" the native application edits the same wire parts" T-LABEL
   NEED-OK s" xml-owner-consumer: ok" MARKER ;

: FAIL-LOAD ( ptr u8 n ptr u8 n -- )
   {: source size:n token token-size:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ source size RUN-INPUT
   RC @ 70 T=
   ERR ERR-U @ token token-size CONTAINS? TTRUE ;

: REFUSALS ( -- )
   s" a linear source cannot be saved in a dynamic record" T-LABEL
   S\" require src/lib/xml.f\nDYNAMIC-BUFFER BAD XML:source\n"
   s" cannot store" FAIL-LOAD
   s" the owner constructor stays private" T-LABEL
   S\" require src/lib/xml.f\n: BAD ( ptr n -- XML:source-owner ) XML:SOURCE-OWNER-MAKE ;\n"
   s" SOURCE-OWNER-MAKE" FAIL-LOAD
   s" the owner unpacker stays private" T-LABEL
   S\" require src/lib/xml.f\n: BAD ( XML:source-owner -- ptr n ) XML:SOURCE-OWNER-UNMAKE ;\n"
   s" SOURCE-OWNER-UNMAKE" FAIL-LOAD
   s" the source token eraser stays private" T-LABEL
   S\" require src/lib/xml.f\n: BAD ( XML:source -- ptr n ) XML:SRC-ERASE ;\n"
   s" SRC-ERASE" FAIL-LOAD
   s" the source token mint stays private" T-LABEL
   S\" require src/lib/xml.f\n: BAD ( ptr n -- XML:source ) XML:SRC-MINT ;\n"
   s" SRC-MINT" FAIL-LOAD ;

: FILE-READ ( ptr u8 n -- n )
   FILE$ FILE-BYTES FILE-CAP READ-ALL ;

: UTF8-ARTIFACT ( -- )
   s" part-utf8.xml" FILE-READ
   FILE-BYTES swap
   s\" \xEF\xBB\xBF<r q='aQb'>Ax🚗Y</r><!--keep--><!--late-->" T$= ;

: PUT-UNIT ( n bool n -- n )
   {: unit:n little:bool at:n :}
   little if
      unit $FF and EXPECTED at + c!
      unit 8 rshift EXPECTED at 1+ + c!
   else
      unit 8 rshift EXPECTED at + c!
      unit $FF and EXPECTED at 1+ + c!
   then
   at 2 + ;

: PUT-ASCII ( ptr u8 n bool n -- n )
   {: chars size:n little:bool at:n :}
   size 0 ?do
      chars i + c@ little at i 2 * + PUT-UNIT drop
   loop
   at size 2 * + ;

: EXPECT16 ( bool -- ptr u8 n )
   {: little:bool :}
   little if $FF EXPECTED c! $FE EXPECTED 1+ c!
   else $FE EXPECTED c! $FF EXPECTED 1+ c! then
   s" <r q='aQb'>Ax" little 2 PUT-ASCII {: after-prefix:n :}
   $D83D little after-prefix PUT-UNIT {: after-high:n :}
   $DE97 little after-high PUT-UNIT {: after-pair:n :}
   s" Y</r><!--keep--><!--late-->" little after-pair PUT-ASCII
   EXPECTED swap ;

: UTF16-ARTIFACT ( bool -- )
   {: little:bool :}
   little if s" part-utf16le.xml" else s" part-utf16be.xml" then
   FILE-READ {: actual:n :}
   little EXPECT16 FILE-BYTES actual 2swap T$= ;

public
: RUN ( -- )
   T-RESET
   s" xml-owner-e2e" HB-TMP-MKDIR {: root:ptr size:n :}
   root ROOT size BYTE-COPY size ROOT-U !
   SOURCE-RUN REFUSALS BUILD NATIVE-RUN
   UTF8-ARTIFACT true UTF16-ARTIFACT false UTF16-ARTIFACT
   APP$ REMOVE-FILE
   s" xml-owner-e2e tree: " type ROOT$ type cr
   T-REPORT ;
;package

XML-OWNER-E2E:RUN
