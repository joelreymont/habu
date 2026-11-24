\ catch-frame-require.f - test/catch-frame.f requires it in a text that then
\ throws, and again after.

package CATCH-FRAME-FIXTURE
public
: ANSWER ( -- n ) 42 ;
;package
