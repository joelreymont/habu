\ kernel-perf-lint-core.f - require evidence for kernel-codegen changes.
\ Load after the lint, filesystem, and profile-registry libraries.

require tools/lint/diff.f

package KERNEL-PERF-LINT
private

16 constant WATCH-MAX
32 constant WADD-MAX
10 constant LF-C

create ONE 1 allot
create WATCH-PATHS WATCH-MAX FS-PATH-CAP * allot
create WATCH-US WATCH-MAX cells allot

\ Waivers added by the diff, captured by canonical identity for the lifecycle
\ resolution (their measurement siblings are only counted, see MEAS#).
create WADD-KP WADD-MAX FS-PATH-CAP * allot   \ owning kernel
create WADD-EP WADD-MAX FS-PATH-CAP * allot   \ owning emitter
create WADD-KU WADD-MAX cells allot
create WADD-EU WADD-MAX cells allot
create WADD-V  WADD-MAX cells allot           \ waiver version
create REG-LOAD-BUF FS-PATH-CAP allot         \ registry file to resolve standing waivers

variable BAD
variable ROWS+
variable CUR-REG
variable WATCH#
variable SCAN-START
variable WADD#
variable MEAS#
variable REG-LOAD-U
variable WV-N

: REG-PATH$ ( -- ptr u8 n )   \ the registry path a diff addresses (row detection + messages)
   s" tools/ptx/perf-rows.tsv" ;

: REG-LOAD$ ( -- ptr u8 n )   \ the file loaded to enumerate standing waivers (REG-PATH$ by default)
   REG-LOAD-U @ 0= if REG-PATH$ exit then
   REG-LOAD-BUF REG-LOAD-U @ ;

: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

: EMIT-C ( n -- ) {: c:n :}
   c ONE c!
   ONE 1 OUT ;

: BAD+ ( -- )
   BAD @ 1+ BAD ! ;

: WATCHED? ( ptr u8 n -- bool )   \ a canonical PTX producer (perf-watch owns the table)
   PERF-WATCH:PRODUCER? ;

: WATCH$ ( n -- ptr u8 n ) {: idx:n :}
   WATCH-PATHS idx FS-PATH-CAP * +
   WATCH-US idx cells + @ ;

: KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup WATCH# @ < while
      dup WATCH$ a u LINT-STR= if drop true exit then
      1+
   repeat drop false ;

: WATCH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u KNOWN? if exit then
   WATCH# @ WATCH-MAX < 0= if E-PERF-CAP throw then
   u FS-PATH-CAP > if E-PERF-CAP throw then
   a WATCH-PATHS WATCH# @ FS-PATH-CAP * + u LINT-BMOVE
   u WATCH-US WATCH# @ cells + !
   WATCH# @ 1+ WATCH# ! ;

: BAD-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-BAD-ROW " OUT REG-PATH$ OUT
   s" : invalid added registry row: " OUT a u OUT
   LF-C EMIT-C ;

: MISSING ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-ROW-MISSING " OUT a u OUT
   s" : kernel codegen changed without a profile/waiver row in " OUT
   REG-PATH$ OUT LF-C EMIT-C ;

: WADD-K$ ( n -- ptr u8 n ) {: i:n :}
   WADD-KP i FS-PATH-CAP * +  WADD-KU i cells + @ ;

: WADD-E$ ( n -- ptr u8 n ) {: i:n :}
   WADD-EP i FS-PATH-CAP * +  WADD-EU i cells + @ ;

: WADD-V@ ( n -- n )
   cells WADD-V + @ ;

: CAPTURE-ADDED ( -- )   \ record the just-parsed row (PERF slot ROW#-1) by identity
   PERF:ROW# 1- {: r:n :}
   r PERF:WAIVER? 0= if MEAS# @ 1+ MEAS# ! exit then
   WADD# @ WADD-MAX < 0= if E-PERF-CAP throw then
   r PERF:KERNEL$ {: ka:ptr ku:n :}
   ku FS-PATH-CAP > if E-PERF-CAP throw then
   ka WADD-KP WADD# @ FS-PATH-CAP * + ku LINT-BMOVE  ku WADD-KU WADD# @ cells + !
   r PERF:EMITTER$ {: ea:ptr eu:n :}
   eu FS-PATH-CAP > if E-PERF-CAP throw then
   ea WADD-EP WADD# @ FS-PATH-CAP * + eu LINT-BMOVE  eu WADD-EU WADD# @ cells + !
   r PERF:WVID@ WADD# @ cells WADD-V + !
   WADD# @ 1+ WADD# ! ;

variable RL-A
variable RL-U

: RL-A-FIELD ( -- ptr ptr u8 )   \ checked-pointer slot for the pending added line
   RL-A 0 ptr-field ;

: REG-LINE ( ptr u8 n -- ) {: a:ptr u:n :}   \ validate + capture one added registry line
   a u PERF:LINE-DATA? 0= if exit then
   a RL-A-FIELD !
   u RL-U !
   [: RL-A-FIELD @ RL-U @ PERF:ADD-LINE ;] catch {: code:n :}
   code 0= if
      ROWS+ @ 1+ ROWS+ !
      CAPTURE-ADDED
   else
      RL-A-FIELD @ RL-U @ BAD-ROW
   then ;

: ADDED ( ptr u8 n -- )
   CUR-REG @ 0<> if REG-LINE else 2drop then ;

: MARK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WATCHED? if a u WATCH+ then ;

: NEW! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MARK
   a u REG-PATH$ LINT-STR= CUR-REG ! ;

: SECTION ( ptr u8 n ptr u8 n DIFF:form bool -- )
   {: oa:ptr ou:n na:ptr nu:n kind:DIFF:form body:bool :}
   oa ou MARK na nu NEW!
   kind drop body drop ;

: EVENT ( DIFF:event -- )
   MATCH DIFF:event
      none OF ENDOF
      section OF
         DIFF:SECTION-OLD$ DIFF:SECTION-NEW$
         DIFF:SECTION-FORM DIFF:SECTION-BODY? SECTION
      ENDOF
      hunk OF ENDOF
      add OF DIFF:CONTENT$ ADDED ENDOF
      context OF ENDOF
      delete OF ENDOF
   ;MATCH ;

: LINE ( ptr u8 n -- )
   DIFF:LINE EVENT ;

: SOURCE-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIFF:SOURCE-VALIDATE
   0 SCAN-START !
   0 begin dup u < while
      dup a + c@ LF-C = if
         a SCAN-START @ + over SCAN-START @ - LINE
         dup 1+ SCAN-START !
      then
      1+
   repeat drop ;

: END-ARTIFACT ( -- )
   DIFF:FINISH EVENT
   DIFF:RESET
   false CUR-REG ! ;

: CROSS ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-WAIVER-CROSS " OUT a u OUT
   s" : added waiver owns an emitter this change does not touch, in " OUT
   REG-PATH$ OUT LF-C EMIT-C ;

: STALE ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-WAIVER-STALE " OUT a u OUT
   s" : added waiver is not newer than the standing waiver for that emitter in " OUT
   REG-LOAD$ OUT LF-C EMIT-C ;

: DUP-WAIVER ( -- )
   BAD+
   s" E-PERF-WAIVER-DUP " OUT REG-LOAD$ OUT
   s" : duplicate live waiver (kernel+emitter+version)" OUT LF-C EMIT-C ;

: REG-UNPARSED ( -- )
   BAD+
   s" E-PERF-BAD-ROW " OUT REG-LOAD$ OUT
   s" : registry does not parse canonically" OUT LF-C EMIT-C ;

: WAIVER-COVERS? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ an added waiver owns emitter (a,u)
   0 begin dup WADD# @ < while
      dup WADD-E$ a u LINT-STR= if drop true exit then
      1+
   repeat drop false ;

: COVER-CHECK ( -- )   \ every touched emitter needs a same-change replacement
   WATCH# @ 0= if exit then
   ROWS+ @ 0= if
      0 begin dup WATCH# @ < while
         dup WATCH$ MISSING 1+
      repeat drop exit
   then
   MEAS# @ 0 > if exit then           \ a measurement is generic evidence, covers every touch
   0 begin dup WATCH# @ < while       \ only waivers added: each touch needs its own waiver
      dup WATCH$ WAIVER-COVERS? 0= if dup WATCH$ MISSING then
      1+
   repeat drop ;

: CROSS-CHECK ( -- )   \ an added waiver must own an emitter this change touches
   0 begin dup WADD# @ < while
      dup WADD-E$ KNOWN? 0= if dup WADD-E$ CROSS then
      1+
   repeat drop ;

: WV-STANDING-GE ( ptr u8 n ptr u8 n n -- n )   \ loaded waivers same kernel+emitter, version>=v
   {: ka:ptr ku:n ea:ptr eu:n v:n :}
   0 WV-N !
   0 begin dup PERF:ROW# < while
      dup PERF:WAIVER? if
         dup PERF:KERNEL$ ka ku LINT-STR=
         over PERF:EMITTER$ ea eu LINT-STR= and
         over PERF:WVID@ v 1- > and
         if WV-N @ 1+ WV-N ! then
      then
      1+
   repeat drop WV-N @ ;

: STALE-CHECK ( -- )   \ each added waiver must be strictly newer than every standing sibling
   0 begin dup WADD# @ < while
      dup {: i:n :}
      i WADD-K$ i WADD-E$ i WADD-V@ WV-STANDING-GE 1 > if i WADD-E$ STALE then
      1+
   repeat drop ;

: MISSING-CHECK ( -- )   \ resolve the waiver lifecycle against the diff + standing registry
   COVER-CHECK
   CROSS-CHECK
   WATCH# @ 0= WADD# @ 0= and if exit then
   [: REG-LOAD$ PERF:LOAD ;] catch 0<> if REG-UNPARSED exit then
   PERF:WAIVER-DUP? if DUP-WAIVER then
   STALE-CHECK ;

public

: RESET ( -- )
   DIFF:RESET
   0 BAD ! 0 ROWS+ ! 0 WATCH# ! false CUR-REG !
   0 WADD# ! 0 MEAS# ! 0 REG-LOAD-U ! ;

: REG-LOAD! ( ptr u8 n -- ) {: a:ptr u:n :}   \ point standing-waiver resolution at a file
   u FS-PATH-CAP > if E-PERF-CAP throw then
   a REG-LOAD-BUF u LINT-BMOVE u REG-LOAD-U ! ;

: SOURCE ( ptr u8 n -- )
   SOURCE-LINES ;

: FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SOURCE
   END-ARTIFACT ;

: FINISH ( -- )
   DIFF:FINISH EVENT
   MISSING-CHECK
   BAD @ 0 > if 1 throw then ;

;package
