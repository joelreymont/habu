\ native-generated-constructor.f - generated SUM/ENUM bodies use construct.
\
\ A payload constructor is the smallest shape whose raw runtime cells differ
\ from its one logical output bundle. The declaration drives the production
\ generator and native compiler at genuine top level, where RESULT:OK-shaped
\ definitions can collide with an older bare binding.

require src/compiler/native/compiler.f

s" ENUM nctorresult 0 VARIANT ok FIELD value n ;VARIANT VARIANT err FIELD error n ;VARIANT ;ENUM"
INCLUDE-EVALUATE
s" test: ok" type cr
