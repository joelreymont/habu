\ Source buffers consumed during one native build. The driver binds OBSERVE to SOURCE-INPUT's
\ selected reader before each loader handover. No filesystem probe occurs here.
\ The emitter role covers whole observed prefix FILEs, including the manifest,
\ through compiler.f; a helper edit in one of those FILEs changes the role.
require src/core/sha256.f
require src/core/include.f

package BUILD-PRODUCER
private

create COMPILER-CONTEXT SHA256-CTX-BYTES allot
create COPY SHA256-CTX-BYTES allot
create CHECKER-DIGEST 32 allot
create COMPILER-DIGEST 32 allot
create CURRENT-DIGEST 32 allot
variable LENGTH
variable INPUTS
variable STARTED
variable CHECKER-KNOWN
variable COMPILER-KNOWN

: COMPILER-SPAN ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size LENGTH !
   COMPILER-CONTEXT LENGTH CELL SHA256-FEED
   COMPILER-CONTEXT bytes size SHA256-FEED ;

: SNAPSHOT ( ptr u8 ptr u8 n ptr u8 -- )
   {: ctx:ptr role:ptr roleu:n digest:ptr :}
   ctx COPY SHA256-CTX-BYTES BYTE-COPY
   COPY role roleu SHA256-FEED
   COPY digest SHA256-END ;

public

: START ( -- )
   COMPILER-CONTEXT SHA256-BEGIN
   COMPILER-CONTEXT s" portable-emitting-compiler-inputs-v1" SHA256-FEED
   0 INPUTS !
   true STARTED !
   false CHECKER-KNOWN !
   false COMPILER-KNOWN ! ;

\ A path relative to its root distinguishes equal source buffers loaded as
\ different files; the same tree at another location observes the same inputs.
\ Content comes from the selected reader, not a separate probe.
: OBSERVE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n :}
   STARTED @ 0= if s" build-producer: not started" 76 die then
   COMPILER-CONTEXT s" source" SHA256-FEED
   path pathu root rootu SOURCE-ROOT:RELATIVE COMPILER-SPAN
   bytes size COMPILER-SPAN
   1 INPUTS +! ;

: OPTION ( ptr u8 n -- )
   STARTED @ 0= if s" build-producer: not started" 76 die then
   COMPILER-CONTEXT s" option" SHA256-FEED
   COMPILER-SPAN ;

: CURRENT$ ( -- ptr u8 n bool )
   STARTED @ 0= if NULL$ false exit then
   COMPILER-CONTEXT s" current" CURRENT-DIGEST SNAPSHOT
   CURRENT-DIGEST 32 true ;

: FREEZE-CHECKER ( -- )
   INPUTS @ 0= if s" build-producer: no checker inputs observed" 76 die then
   COMPILER-CONTEXT s" checker" CHECKER-DIGEST SNAPSHOT
   true CHECKER-KNOWN ! ;

\ A completed compiler.f load includes its nested selected backend sources.
\ Later REPL, debugger and C2 rows cannot change the emitter used for FILEs.
: COMPILER-DONE ( ptr u8 n ptr u8 n -- )
   SOURCE-ROOT:RELATIVE
   s" src/compiler/native/compiler.f" STR= 0= if exit then
   COMPILER-KNOWN @ if exit then
   INPUTS @ 0= if s" build-producer: no compiler inputs observed" 76 die then
   COMPILER-CONTEXT s" emitting-compiler" COMPILER-DIGEST SNAPSHOT
   true COMPILER-KNOWN ! ;

: CHECKER$ ( -- ptr u8 n bool )
   CHECKER-KNOWN @ 0= if NULL$ false exit then
   CHECKER-DIGEST 32 true ;

: COMPILER$ ( -- ptr u8 n bool )
   COMPILER-KNOWN @ 0= if NULL$ false exit then
   COMPILER-DIGEST 32 true ;

;package
