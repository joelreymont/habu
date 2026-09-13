\ The process caches must be fresh in every restored generation.
require lib/process-command.f
require lib/test.f

package PROCESS-IMAGE-SUBJECT
private

: CHECK-DEFAULT ( -- )
   s" HABU_IMAGE_DEFAULT" >LEN PROC-ENV-DEFAULT$? TTRUE
   LEN>N s" default-value" T$= ;

: COMMAND ( -- )
   PROC-CMD:RESET
   PROC-CMD:ENV-HERMETIC
   s" HABU_IMAGE_ENV" >LEN s" command-value" >LEN PROC-CMD:ENV+
   s" -c" >LEN PROC-CMD:ARG+
   S\" test \q$HABU_IMAGE_ENV\q = command-value && printf process-image-ok" >LEN
   PROC-CMD:ARG+
   s" /bin/sh" >LEN 10000 >MS PROC-CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N 0 T=
   PROC-CMD:OUT$ s" process-image-ok" T$=
   PROC-CMD:ERR$ 0 T= drop ;

: CHECK-ROWS ( -- )
   PROC-ARGV-MAX 1+ 0 ?do
      PROC-ARGV-TABLE i ptr-field @ 0= TTRUE
   loop ;

public

: CLEAN ( -- )
   T-RESET
   PROC-ARGV-BUF@ 0= TTRUE
   PROC-ENV-TABLE@ 0= TTRUE
   PROC-ENV-BUF@ 0= TTRUE
   PROC-ENV-DEF-TABLE@ 0= TTRUE
   PROC-ENV-DEF-BUF@ 0= TTRUE
   PROC-ARGV-N @ COUNT>N 0 T= PROC-ARGV-OFF @ OFF>N 0 T=
   PROC-ENV-N @ COUNT>N 0 T= PROC-ENV-OFF @ OFF>N 0 T=
   PROC-ENV-DEF-N @ COUNT>N 0 T= PROC-ENV-DEF-OFF @ OFF>N 0 T=
   s" HABU_IMAGE_DEFAULT" >LEN PROC-ENV-DEFAULT$? TFALSE 2drop
   CHECK-ROWS
   T-REPORT ;

\ This path publishes a pointer row without allocating the argv byte cache.
\ Repeated preparation also checks that an already completed cleanup is inert.
: EMPTY-ARGV ( -- )
   s" /bin/sh" >LEN PROC-ARGV-PREPARE 2drop
   IMAGE-LIFECYCLE:PREPARE IMAGE-LIFECYCLE:PREPARE
   CLEAN ;

: USE ( -- )
   T-RESET
   PROC-ENV-DEFAULT-RESET
   s" HABU_IMAGE_DEFAULT" >LEN s" default-value" >LEN PROC-ENV-DEFAULT+
   CHECK-DEFAULT
   COMMAND
   CHECK-DEFAULT
   \ Leave active rows and all five mappings for capture to clear.
   PROC-ARGV-ENV-RESET
   s" pending-argument" >LEN PROC-ARGV+
   s" HABU_IMAGE_ENV" >LEN s" pending-value" >LEN PROC-ENV+
   PROC-ARGV-N @ COUNT>N 1 T= PROC-ENV-N @ COUNT>N 1 T=
   T-REPORT ;

;package
