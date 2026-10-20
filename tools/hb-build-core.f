\ One native build selection and one FILE source session. Product drivers own
\ their validation, source window, and image writer through three continuations.
require tools/build-target.f
require tools/native-file-cache.f

package BUILD-CORE
private

TYPED-VARIABLE PREPARE [ -- ]
TYPED-VARIABLE LOAD [ -- ]
TYPED-VARIABLE FINISH [ -- ]
PTR-VARIABLE MODE
variable MODE-U
variable RESULT

: RUN ( -- )
   PREPARE @ execute
   MODE @ MODE-U @ LOAD @ NFILE-CACHE:WITH
   FINISH @ execute ;

: ACTION ( RTARGET:build-action -- )
   drop
   ['] RUN catch RESULT ! ;

public

: WITH ( ptr u8 n RTARGET:build-action [ -- ] [ -- ] [ -- ] -- n )
   {: mode:ptr modeu:n action:RTARGET:build-action prepare load finish :}
   BUILD-TARGET:IDLE-CK
   mode MODE ! modeu MODE-U !
   prepare PREPARE !
   load LOAD !
   finish FINISH !
   0 RESULT !
   action ['] ACTION BUILD-TARGET:WITH
   RESULT @ ;

;package
