\ Run one public XML owner consumer from source; its deterministic edited XML
\ artifacts go in a temporary tree the process removes at exit.
require src/lib/test/assert.f
require src/lib/test/eval.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/process.f
require test/pcap-result.f
require src/lib/xml.f

package XML-OWNER-E2E
private

$8000 constant IO-CAP
512 constant FILE-CAP
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
create FILE-BYTES FILE-CAP allot
create EXPECTED FILE-CAP allot
variable RC

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: FILE$ ( str -- str ) {: rel:str :}
   ROOT$ rel PATH FS:JOIN-PATH PATH swap >str ;

: RUN-INPUT ( str str -- )
   {: program:str input:str :}
   PROC-ENV-INHERIT-MISSING
   program SOURCE-ROOT:CWD$ input
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ >str type ERR ERR-U @ >str type then
   RC @ 0 T= ;

: MARKER ( str -- ) {: marker:str :}
   OUT OUT-U @ >str marker STR:CONTAINS? TTRUE ;

: ENTRY$ ( -- str ) s" source-run.f" FILE$ ;

: SOURCE-ARGS ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENTRY$ PROC-ARGV+
   ROOT$ PROC-ARGV+ ;

: SOURCE-RUN ( -- )
   ENTRY$ S\" require test/xml-owner-consumer.f\nMAIN\n" FS:WRITE-ALL
   SOURCE-ARGS
   ENGINE-CANDIDATE:PATH$ s" " RUN-INPUT
   s" public source load opens, edits and cleans multiple parts" T-LABEL
   NEED-OK s" xml-owner-consumer: ok" MARKER ;

: REJECTIONS ( -- )
   s" a linear source cannot be saved in a dynamic record" T-LABEL
   s" DYNAMIC-BUFFER BAD XML:source" TEST-EVAL:RC 70 T=
   s" the owner constructor stays private" T-LABEL
   s" : BAD ( ^n -- XML:source-owner ) XML:SOURCE-OWNER-MAKE ;" TEST-EVAL:RC 70 T=
   s" the owner unpacker stays private" T-LABEL
   s" : BAD ( XML:source-owner -- ^n ) XML:SOURCE-OWNER-UNMAKE ;" TEST-EVAL:RC 70 T=
   s" the source token eraser stays private" T-LABEL
   s" : BAD ( XML:source -- ^n ) XML:SRC-ERASE ;" TEST-EVAL:RC 70 T=
   s" the source token mint stays private" T-LABEL
   s" : BAD ( ^n -- XML:source ) XML:SRC-MINT ;" TEST-EVAL:RC 70 T= ;

: FILE-READ ( str -- n )
   FILE$ FILE-BYTES FILE-CAP FS:READ-ALL ;

: UTF8-ARTIFACT ( -- )
   s" part-utf8.xml" FILE-READ
   FILE-BYTES swap >str
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

: PUT-ASCII ( str bool n -- n )
   {: chars:str little:bool at:n :}
   chars STR:LEN 0 ?do
      chars STR:DATA i + c@ little at i 2 * + PUT-UNIT drop
   loop
   at chars STR:LEN 2 * + ;

: EXPECT16 ( bool -- str )
   {: little:bool :}
   little if $FF EXPECTED c! $FE EXPECTED 1+ c!
   else $FE EXPECTED c! $FF EXPECTED 1+ c! then
   s" <r q='aQb'>Ax" little 2 PUT-ASCII {: after-prefix:n :}
   $D83D little after-prefix PUT-UNIT {: after-high:n :}
   $DE97 little after-high PUT-UNIT {: after-pair:n :}
   s" Y</r><!--keep--><!--late-->" little after-pair PUT-ASCII
   EXPECTED swap >str ;

: UTF16-ARTIFACT ( bool -- )
   {: little:bool :}
   little if s" part-utf16le.xml" else s" part-utf16be.xml" then
   FILE-READ {: actual:n :}
   little EXPECT16 FILE-BYTES actual >str swap T$= ;

public
: RUN ( -- )
   T-RESET
   s" xml-owner-e2e" FS:MKTEMP-DIR {: root:str :}
   root FS:CLEANUP-TREE+
   root STR:DATA ROOT root STR:LEN BYTE-COPY root STR:LEN ROOT-U !

   SOURCE-RUN REJECTIONS
   UTF8-ARTIFACT true UTF16-ARTIFACT false UTF16-ARTIFACT
   T-REPORT ;
;package

XML-OWNER-E2E:RUN
