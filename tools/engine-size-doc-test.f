\ engine-size-doc-test.f - keep the checked size tables beside the image they
\ describe. The generated block in docs/engine-size.md is the tool's complete
\ row output, so a capture change fails with the named rows in the assertion's
\ expected/got report.

require lib/test.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/string.f
require lib/adt/option.f

package ENGINE-SIZE-DOC-TEST

65536 constant DOC-CAP
8192 constant OUT-CAP

create DOC DOC-CAP allot
create OUT OUT-CAP allot
create ERR OUT-CAP allot
variable DOC-U

: DOC$ ( -- ptr u8 n ) DOC DOC-U @ ;

: LOAD-DOC ( -- )
   s" docs/engine-size.md" DOC DOC-CAP READ-ALL DOC-U ! ;

: MARK-OFF ( ptr u8 n ptr u8 n -- n )
   FIND-SUB MATCH option
      none OF s" engine-size-doc-test: missing generated marker" 74 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: EXPECTED$ ( -- ptr u8 n )
   DOC$ {: a:ptr u:n :}
   a u s" <!-- ENGINE-SIZE-ROWS-BEGIN -->" MARK-OFF
   s" <!-- ENGINE-SIZE-ROWS-BEGIN -->" nip 1+ + {: start:n :}
   a u s" <!-- ENGINE-SIZE-ROWS-END -->" MARK-OFF {: end:n :}
   a start + end start - ;

: RUN-SIZE ( -- n n n )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/engine-size.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" bin/hb" >LEN PROC-ARGV+
   s" bin/hb" >LEN OUT OUT-CAP >LEN ERR OUT-CAP >LEN
   30000 >MS RUN-ARGV-CAPTURE MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: ou:len eu:len :} ou LEN>N eu LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: ou:len eu:len rc:rc :} ou LEN>N eu LEN>N rc RC>N ENDOF
   ;MATCH ;

: TEST-SIZE-DOC ( -- )
   LOAD-DOC
   RUN-SIZE {: outu:n erru:n rc:n :}
   rc 74 = outu 0= and
   ERR erru s" image-size: not a fixed-base arm64 image" STR= and if
      s" SKIP: engine-size-doc (bin/hb is not a fixed-base arm64 image)" type cr
      exit
   then
   s" engine-size tool exits successfully" T-LABEL
   rc 0 T=
   s" engine-size tables match docs/engine-size.md" T-LABEL
   erru 0 T=
   OUT outu EXPECTED$ T$= ;

T-RESET
TEST-SIZE-DOC
T-REPORT
;package
