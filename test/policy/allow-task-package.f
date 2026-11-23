\ allow-task-package.f - a task admits a package after the main thread admitted
\ another, then a second task refuses one publishing a keyword, as main does.
require src/lib/vocabulary.f
require src/lib/task.f

package PTMAIN
public
: ONE ( -- n ) 1 ;
;package

package PTASK
public
: TWO ( -- n ) 2 ;
;package

package PTKW
public
: DUP ( n n -- n ) drop ;
;package

TASK:MIN-STACK TASK:TASK ADMITTER
TASK:MIN-STACK TASK:TASK REFUSER

: ADMIT ( -- )
   s" PTASK" VOCABULARY:ALLOW-PACKAGE s" admitted" type cr 0 TASK:RETURN ;
: REFUSE ( -- ) s" PTKW" VOCABULARY:ALLOW-PACKAGE 0 TASK:RETURN ;

: JOIN-CODE ( result<n,n> -- n )
   MATCH result ok OF drop 0 ENDOF err OF ENDOF ;MATCH ;

: RUN ( -- )
   s" PTMAIN" VOCABULARY:ALLOW-PACKAGE
   ['] ADMIT ADMITTER TASK:ACTIVATE
   ADMITTER TASK:JOIN JOIN-CODE .
   ['] REFUSE REFUSER TASK:ACTIVATE
   REFUSER TASK:JOIN JOIN-CODE . ;
RUN
