split-str
\ postpone-stored-use.f - a program that builds types of its own, then a word
\ over the type test/fixtures/postpone-stored.f stored, whose id a later node
\ may hold by then.

require test/fixtures/postpone-stored.f

package POSTPONE-STORED-USE
: ONE ( -- n ) 1 ;
: MAKE ( ^u8 n -- )
   POSTPONE-STORED:HELD @ define-word  postpone ONE  postpone ; ;

EFFECT:EMPTY EFFECT:N EFFECT:IN drop
s" STORED-ONE" MAKE
;package

: MAIN ( -- ) ;
