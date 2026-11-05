\ checker-rollback-signature-suite.f - a definition the checker rejects rolls
\ its scope back, so the name can take a different effect that holds its callers.

require src/lib/test.f

package ROLLBACK-SIGNATURE-TEST

T-RESET
variable TC                    \ last caught throw code

: SCX-BADDEF ( -- ) s" : SCXB ( n -- n ) drop ;" evaluate-closed ;
: SCX-GOODDEF ( -- ) s" : SCXB ( n -- ) drop ;" evaluate-closed ;
: SCX-GOODUSE ( -- ) s" : SCXBU ( n -- ) SCXB ;" evaluate-closed ;
: SCX-BADUSE ( -- ) s" : SCXBU2 ( n -- n ) SCXB ;" evaluate-closed ;

' SCX-BADDEF catch TC !   TC @ 0 <> TTRUE            \ rejected: the body drops its output
' SCX-GOODDEF catch TC !  TC @ 0 T=
' SCX-GOODUSE catch TC !  TC @ 0 T=                  \ a caller certifies against the new effect
' SCX-BADUSE catch TC !   TC @ 0 <> TTRUE            \ ... and against nothing else

T-REPORT

;package
