\ Lex, release the tables as a capture does and lex again, then save an image
\ and lex in it.
\ A fresh process and a saved image are the boundary under test: the lexer's
\ mapped tables belong to the process that mapped them.
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package SOURCE-LEX-IMAGE-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot variable ROOT-U
create IMAGE-BUF PATH-CAP allot variable IMAGE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: PREPARE ( -- )
   s" source-lex-image" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" lexer" IMAGE-BUF FS:JOIN-PATH IMAGE-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         erru LEN>N 0<> if OUT outu LEN>N type ERR erru LEN>N type then
         erru LEN>N 0 T=
         outu LEN>N
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         OUT outu LEN>N type ERR erru LEN>N type
         rc RC>N throw
      ENDOF
   ;MATCH ;

: ENVIRONMENT ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

\ APP-IMAGE:SAVE ends the process, so the live lex after a release follows
\ IMAGE-LIFECYCLE:PREPARE, the release step every capture runs.
: BUILD ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/lib/source-lex.f\ns\" : X 1 ;\" LINT-LEX:SOURCE\nIMAGE-LIFECYCLE:PREPARE\ns\" : Y 2 ;\" LINT-LEX:SOURCE LINT-LEX:COUNT .\nS\\\" : Q s\\q\" LINT-LEX:SOURCE LINT-LEX:ERROR-KIND@ .\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" 4\n1\n" T$=
   IMAGE$ FS:EXECUTABLE? TTRUE ;

\ The saved image starts with no tokens and no error, and lexes into fresh
\ tables.
: RUN-SAVED ( -- )
   ENVIRONMENT
   IMAGE$ >LEN
   S\" LINT-LEX:ERROR-KIND@ . LINT-LEX:COUNT . s\" : X 1 ;\" LINT-LEX:SOURCE LINT-LEX:COUNT . 1 LINT-LEX:TOKEN type\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" 0\n0\n4\nX" T$= ;

: RUN ( -- )
   T-RESET FS:CLEANUP-RESET
   [: PREPARE BUILD RUN-SAVED ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
