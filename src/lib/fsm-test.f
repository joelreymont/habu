require src/lib/test.f
require src/lib/fsm.f
require src/lib/fmt.f
require src/core/bytes.f

package FSM-TEST
public

\ GETAFIX's context: the key text, the next byte to read in it and the output.
STRUCTURE line DERIVE addr
   FIELD text ptr u8
   FIELD len n
   FIELD pos n
   FIELD out ptr u8
   FIELD used n
;STRUCTURE

private

TYPED-VARIABLE LINE-REC line
16 BUFFER: OUT-BUF

: TEXT@ ( ptr line -- ptr u8 ) FSM--TEST-LINE:TEXT @ ;
: LEN@ ( ptr line -- n ) FSM--TEST-LINE:LEN @ ;
: POS@ ( ptr line -- n ) FSM--TEST-LINE:POS @ ;
: POS! ( n ptr line -- ) FSM--TEST-LINE:POS ! ;
: OUT@ ( ptr line -- ptr u8 ) FSM--TEST-LINE:OUT @ ;
: USED@ ( ptr line -- n ) FSM--TEST-LINE:USED @ ;
: USED! ( n ptr line -- ) FSM--TEST-LINE:USED ! ;

using FSM
ENUM entry start whole fraction ended ;ENUM
ENUM key other digit minus point enter ;ENUM

: CLASS ( n -- key )
   {: b:n :}
   b [char] 0 >= b [char] 9 <= and if construct key digit exit then
   b [char] - = if construct key minus exit then
   b [char] . = if construct key point exit then
   construct key other ;


: READ-KEY ( ptr line -- key )
   {: l :}
   l POS@ l LEN@ >= if construct key enter exit then
   l TEXT@ l POS@ + c@ CLASS ;

: DROP-KEY ( ptr line -- ) dup POS@ 1+ swap POS! ;


: ECHO ( ptr line -- )
   {: l :}
   l TEXT@ l POS@ + c@ l OUT@ l USED@ + c!
   l USED@ 1+ l USED!
   l DROP-KEY ;

DEFINE GETAFIX ( ptr line ) entry key
   IN start     READ-KEY
      other   DROP-KEY  >start
      digit   ECHO      >whole
      minus   ECHO      >whole
      point   ECHO      >fraction
      enter   -         >ended
   IN whole     READ-KEY
      other   DROP-KEY  >whole
      digit   ECHO      >whole
      minus   DROP-KEY  >whole
      point   ECHO      >fraction
      enter   -         >ended
   IN fraction  READ-KEY
      other   DROP-KEY  >fraction
      digit   ECHO      >fraction
      minus   DROP-KEY  >fraction
      point   DROP-KEY  >fraction
      enter   -         >ended
   FINAL ended  -
;DEFINE
;using


: GETAFIX$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   LINE-REC {: l :}
   a l FSM--TEST-LINE:TEXT !
   u l FSM--TEST-LINE:LEN !
   0 l POS!
   OUT-BUF l FSM--TEST-LINE:OUT !
   0 l USED!
   l construct entry start GETAFIX
   OUT-BUF l USED@ ;


: ENTRIES ( -- )
   s" 12.5" GETAFIX$ s" 12.5" T$=
   s" -3.25" GETAFIX$ s" -3.25" T$=
   s" 1-2" GETAFIX$ s" 12" T$=
   s" a1.2.3" GETAFIX$ s" 1.23" T$=
   s" " GETAFIX$ s" " T$=
   s" x" GETAFIX$ s" " T$= ;

\ A NEXT word.
using FSM
ENUM tally counting counted ;ENUM
ENUM pulse tick ;ENUM
TYPED-VARIABLE COUNT-CELL n
5 constant LIMIT

: PULSE ( ptr n -- pulse ) drop construct pulse tick ;
: BUMP ( ptr n -- ) 1 swap +! ;


: TALLY-NEXT ( ptr n -- tally )
   @ LIMIT < if construct tally counting else construct tally counted then ;

DEFINE COUNT-UP ( ptr n ) tally pulse
   IN counting  PULSE
      tick  BUMP  TALLY-NEXT
   FINAL counted  -
;DEFINE
;using

: COUNTS ( -- )
   0 COUNT-CELL !
   COUNT-CELL construct tally counting COUNT-UP
   COUNT-CELL @ LIMIT T= ;

\ FINAL actions.
using FSM
ENUM door opening opened shut ;ENUM
ENUM push left right ;ENUM
TYPED-VARIABLE DOOR-CELL n

: PUSH-SIDE ( ptr n -- push )
   @ 1 and 0= if construct push left else construct push right then ;

: NOTE ( ptr n -- ) 1000 swap +! ;
: MARK-OPENED ( ptr n -- ) 10 swap +! ;
: MARK-SHUT ( ptr n -- ) 100 swap +! ;

DEFINE SWING ( ptr n ) door push
   IN opening  PUSH-SIDE
      left   NOTE  >opened
      right  NOTE  >shut
   FINAL opened  MARK-OPENED
   FINAL shut    MARK-SHUT
;DEFINE
;using

: SWUNG ( n door -- n )
   swap DOOR-CELL !
   DOOR-CELL swap SWING
   DOOR-CELL @ ;


: FINALS ( -- )
   0 construct door opening SWUNG 1010 T=
   1 construct door opening SWUNG 1101 T=
   0 construct door shut SWUNG 100 T=
   0 construct door opened SWUNG 10 T= ;

\ A throw.
using FSM
ENUM spin turning stopped ;ENUM
ENUM turn step ;ENUM
TYPED-VARIABLE SPIN-CELL n
42 constant THIRD-CODE

: TURN ( ptr n -- turn ) drop construct turn step ;


: TURN-COUNT ( ptr n -- )
   {: c:ptr :}
   1 c +!
   c @ 3 = if THIRD-CODE throw then ;

DEFINE SPIN ( ptr n ) spin turn
   IN turning  TURN
      step   TURN-COUNT  >turning
   FINAL stopped  -
;DEFINE
;using

: SPIN-RUN ( -- ) SPIN-CELL construct spin turning SPIN ;


: THROWS ( -- )
   0 SPIN-CELL !
   [: SPIN-RUN ;] THIRD-CODE TTHROWSQ
   SPIN-CELL @ 3 T= ;

\ A failure end: the FINAL row's action always throws FAIL-CODE, and the
\ machine still loads.
using FSM
ENUM job working failed ;ENUM
ENUM work break ;ENUM
TYPED-VARIABLE JOB-CELL n
43 constant FAIL-CODE

: BREAK ( ptr n -- work ) drop construct work break ;
: FAIL ( ptr n -- ) drop FAIL-CODE throw ;

DEFINE JOB ( ptr n ) job work
   IN working  BREAK
      break  -  >failed
   FINAL failed  FAIL
;DEFINE
;using

: JOB-RUN ( -- ) JOB-CELL construct job working JOB ;


: FAILS ( -- )
   [: JOB-RUN ;] FAIL-CODE TTHROWSQ ;

\ A total table: kick is refused by an ACTION that always throws, and every
\ line of stuck by a NEXT that does.
using FSM
ENUM gate closed stuck opened ;ENUM
ENUM knock tap kick wedge ;ENUM
TYPED-VARIABLE GATE-CELL n
44 constant KICK-CODE
45 constant STUCK-CODE

: KNOCK ( ptr n -- knock )
   @ {: k:n :}
   k 0= if construct knock tap exit then
   k 1 = if construct knock kick exit then
   construct knock wedge ;

: OPEN-UP ( ptr n -- ) 10 swap ! ;
: KICKED ( ptr n -- ) drop KICK-CODE throw ;
: JAMMED ( ptr n -- gate ) drop STUCK-CODE throw ;

DEFINE GATE ( ptr n ) gate knock
   IN closed  KNOCK
      tap    OPEN-UP  >opened
      kick   KICKED   >closed
      wedge  -        >stuck
   IN stuck  KNOCK
      tap    -  JAMMED
      kick   -  JAMMED
      wedge  -  JAMMED
   FINAL opened  -
;DEFINE
;using

: GATE-RUN ( n -- ) GATE-CELL ! GATE-CELL construct gate closed GATE ;

: TOTAL ( -- )
   [: 1 GATE-RUN ;] KICK-CODE TTHROWSQ
   [: 2 GATE-RUN ;] STUCK-CODE TTHROWSQ
   0 GATE-RUN GATE-CELL @ 10 T= ;

\ A row's own inputs: arming senses the header's probe, armed only a latch, and
\ both ENUMs carry ok.
using FSM
ENUM relay arming armed tripped ;ENUM
ENUM probe ok fault ;ENUM
ENUM latch ok ;ENUM
TYPED-VARIABLE RELAY-CELL n

: PROBE ( ptr n -- probe ) drop construct probe ok ;
: LATCH ( ptr n -- latch ) drop construct latch ok ;
: ARM ( ptr n -- ) 1 swap +! ;
: TRIP ( ptr n -- ) 10 swap +! ;

DEFINE RELAY ( ptr n ) relay probe
   IN arming  PROBE
      ok     ARM  >armed
      fault  -    >tripped
   IN armed   LATCH ( latch )
      ok     TRIP  >tripped
   FINAL tripped  -
;DEFINE
;using

: RELAY-RUN ( relay -- n ) 0 RELAY-CELL ! RELAY-CELL swap RELAY RELAY-CELL @ ;

: ROWS ( -- )
   construct relay arming RELAY-RUN 11 T= ;

\ No FINAL row: LAMP's runner never returns, so the test steps it.
using FSM
ENUM lamp dark lit ;ENUM
ENUM flick flip ;ENUM
TYPED-VARIABLE LAMP-CELL n

: FLICK ( ptr n -- flick ) drop construct flick flip ;

DEFINE LAMP ( ptr n ) lamp flick
   IN dark  FLICK
      flip  BUMP  >lit
   IN lit   FLICK
      flip  BUMP  >dark
;DEFINE
;using

\ A throw from SENSE.
using FSM
TYPED-VARIABLE PEEK-CELL n
48 constant SENSE-CODE

: BLIND ( ptr n -- pulse ) @ 0= if SENSE-CODE throw then construct pulse tick ;

DEFINE PEEK ( ptr n ) tally pulse
   IN counting  BLIND
      tick  BUMP  >counted
   FINAL counted  -
;DEFINE
;using

\ The test ENUMs are private, so phases are read by exhaustive MATCH.
: TALLY>N ( tally -- n ) MATCH tally counting OF 0 ENDOF counted OF 1 ENDOF ;MATCH ;
: LAMP>N ( lamp -- n ) MATCH lamp dark OF 0 ENDOF lit OF 1 ENDOF ;MATCH ;


: DOOR>N ( door -- n )
   MATCH door opening OF 0 ENDOF opened OF 1 ENDOF shut OF 2 ENDOF ;MATCH ;


\ A step into a FINAL answers false without its action; a step at one runs it
\ and answers true.
: STEPS ( -- )
   0 COUNT-CELL !
   COUNT-CELL construct tally counting COUNT-UP-STEP TFALSE TALLY>N 0 T=
   COUNT-CELL @ 1 T=
   0 DOOR-CELL !
   DOOR-CELL construct door opening SWING-STEP TFALSE DOOR>N 1 T=
   DOOR-CELL @ 1000 T=
   0 DOOR-CELL !
   DOOR-CELL construct door opened SWING-STEP TTRUE DOOR-CELL @ 10 T=
   DOOR-CELL swap SWING-STEP TTRUE DOOR>N 1 T= DOOR-CELL @ 20 T=
   0 LAMP-CELL !
   LAMP-CELL construct lamp dark LAMP-STEP TFALSE dup LAMP>N 1 T=
   LAMP-CELL @ 1 T=
   LAMP-CELL swap LAMP-STEP TFALSE LAMP>N 0 T= LAMP-CELL @ 2 T=
   0 PEEK-CELL !
   [: PEEK-CELL construct tally counting PEEK-STEP 2drop ;] SENSE-CODE TTHROWSQ ;

DYNAMIC-BUFFER SRC u8
variable SRC-LEN

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   SRC-LEN @ u + SRC-RESERVE
   a SRC-LEN @ SRC u BYTE-COPY
   SRC-LEN @ u + SRC-LEN ! ;

: N+ ( n -- ) FMT:INT$ SRC+ ;
: SRC$ ( -- ptr u8 n ) 0 SRC SRC-LEN @ ;


\ A machine that runs after a rejected table: the next DEFINE starts from an
\ empty buffer, so it answers 2.
: VALID ( n -- n )
   {: k:n :}
   0 SRC-LEN !
   s" package FSM-OK-" SRC+ k N+
   s"  TYPED-VARIABLE C n ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM" SRC+
   s"  : S ( ptr n -- sig ) drop construct sig go ; : BUMP ( ptr n -- ) 1 swap +! ;" SRC+
   s"  using FSM DEFINE M ( ptr n ) ph sig IN idle S go BUMP >busy halt - >idle" SRC+
   s"  FINAL busy BUMP ;DEFINE ;using" SRC+
   s"  public : GO ( -- n ) 0 C ! C construct ph idle M C @ ; ;package FSM-OK-" SRC+
   k N+ s" :GO" SRC+
   SRC$ TEST-EVAL:N ;


\ Each text opens its own package, so a failed one leaves nothing a later one
\ meets.
: SYNTAX ( -- )
   s" package FSM-SYN-END ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-CTX ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM using FSM DEFINE M ( n"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-HEAD ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M n ph sig IN idle S go - >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-FIRST ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig go - >busy IN idle S go - >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-IN ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - IN busy S go - >idle halt - >idle ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-FINAL ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-CLOSE ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-AFTER ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle FINAL busy - go - >idle ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-SLASH ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle \ old line FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-PAREN ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go ( >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-OPEN ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S ( sig ph go - >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-EMPTY ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S ( ) ) go - >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-TWO ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S ( sig go ) - >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-ROWBARE ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S ( sig ) IN busy S go - >idle halt - >idle ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   s" package FSM-SYN-BARE ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S IN busy S go - >idle halt - >idle ;DEFINE ;using ;package"
   TEST-EVAL:RC FSM:E-SYNTAX T=
   5 VALID 2 T= ;


\ The corrected twin loads; the checker rejects each faulty table at load, 70.
: REJECTED ( -- )
   s" package FSM-GOOD-INPUT ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC 0 T=
   s" package FSM-BAD-INPUT ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   1 VALID 2 T=
   s" package FSM-BAD-NEXT ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >gone halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   2 VALID 2 T=
   s" package FSM-BAD-STATE ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   3 VALID 2 T=
   s" package FSM-BAD-ACTION ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; : KEEP ( n -- n ) ; using FSM DEFINE M ( n ) ph sig IN idle S go KEEP >busy halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   4 VALID 2 T=
   s" package FSM-DEAD-NEXT 46 constant E-K ENUM st ready done ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; : KICKED ( n -- ) drop E-K throw ; : PICK ( n -- st ) drop construct st done ; using FSM DEFINE M ( n ) st sig IN ready S go KICKED PICK halt - >done FINAL done - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   6 VALID 2 T=
   s" package FSM-DEAD-RUN 47 constant E-L ENUM st ready only ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; : LOST ( n -- ) drop E-L throw ; using FSM DEFINE M ( n ) st sig IN ready S go LOST >only halt LOST >only FINAL only LOST ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   7 VALID 2 T=
   s" package FSM-GOOD-ROW ENUM ph idle busy done ;ENUM ENUM sig go halt ;ENUM ENUM bell go ring ;ENUM : S ( n -- sig ) drop construct sig go ; : R ( n -- bell ) drop construct bell go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle IN busy R ( bell ) go - >done ring - >idle FINAL done - ;DEFINE ;using ;package"
   TEST-EVAL:RC 0 T=
   s" package FSM-BAD-ROW-MISS ENUM ph idle busy done ;ENUM ENUM sig go halt ;ENUM ENUM bell go ring ;ENUM : S ( n -- sig ) drop construct sig go ; : R ( n -- bell ) drop construct bell go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle IN busy R ( bell ) go - >done FINAL done - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   8 VALID 2 T=
   s" package FSM-BAD-ROW-TAG ENUM ph idle busy done ;ENUM ENUM sig go halt ;ENUM ENUM bell go ring ;ENUM : S ( n -- sig ) drop construct sig go ; : R ( n -- bell ) drop construct bell go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle IN busy R ( bell ) go - >done ring - >idle halt - >idle FINAL done - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   9 VALID 2 T=
   s" package FSM-BAD-ROW-SENSE ENUM ph idle busy done ;ENUM ENUM sig go halt ;ENUM ENUM bell go ring ;ENUM : S ( n -- sig ) drop construct sig go ; : R ( n -- bell ) drop construct bell go ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle IN busy S ( bell ) go - >done ring - >idle FINAL done - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   10 VALID 2 T=
   s" package FSM-BAD-FINAL ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; : RETRY ( bool ph n -- bool ph ) drop ; using FSM DEFINE M ( n ) ph sig IN idle S go - >busy halt - >idle FINAL busy RETRY ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   11 VALID 2 T=
   s" package FSM-BAD-PICK-ACTION ENUM ph idle busy ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; : TWIN ( n n -- n ) drop ; : PICK ( n -- ph ) drop construct ph busy ; using FSM DEFINE M ( n ) ph sig IN idle S go TWIN PICK halt - >idle FINAL busy - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   12 VALID 2 T=
   s" package FSM-BAD-STATE-ACTION ENUM ph idle busy done ;ENUM ENUM sig go halt ;ENUM : S ( n -- sig ) drop construct sig go ; : SNEAK ( ph n -- ph ) drop drop construct ph done ; using FSM DEFINE M ( n ) ph sig IN idle S go SNEAK >busy halt - >idle FINAL busy - FINAL done - ;DEFINE ;using ;package"
   TEST-EVAL:RC 70 T=
   13 VALID 2 T= ;

\ The size table: STATES rows of INPUTS lines, each naming one long action, so
\ each line word outgrows its line and the generated text the 1 MiB table.
200 constant STATES
44 constant INPUTS
$100000 constant MIB

: LONG$ ( -- ptr u8 n )
   s" COUNT-ONE-TURN-OF-THE-LARGE-TABLE-WHOSE-ACTION-NAME-IS-LONG-SO-THAT-ITS-TEXT-PASSES-ONE-MEBIBYTE-UNDER-THE-BODY-CAP" ;


: BIG-HEAD ( -- )
   0 SRC-LEN !
   s" package FSM-BIG ENUM big-state" SRC+
   STATES 0 ?do s"  s" SRC+ i N+ loop
   s"  ;ENUM ENUM big-input" SRC+
   INPUTS 0 ?do s"  i" SRC+ i N+ loop
   s"  ;ENUM TYPED-VARIABLE BIG-CELL n" SRC+
   s"  : BIG-SENSE ( ptr n -- big-input ) drop construct big-input i0 ;" SRC+
   s"  : " SRC+ LONG$ SRC+ s"  ( ptr n -- ) 1 swap +! ;" SRC+
   s"  using FSM DEFINE BIG ( ptr n ) big-state big-input" SRC+ ;


: BIG-ROW ( n -- )
   {: k:n :}
   s"  IN s" SRC+ k N+ s"  BIG-SENSE" SRC+
   INPUTS 0 ?do
      s"  i" SRC+ i N+ s"  " SRC+ LONG$ SRC+ s"  >s" SRC+ k 1+ N+
   loop ;


: BIG$ ( -- ptr u8 n )
   BIG-HEAD
   STATES 1- 0 ?do i BIG-ROW loop
   s"  FINAL s" SRC+ STATES 1- N+ s"  -" SRC+
   s"  ;DEFINE ;using public : GO ( -- n ) 0 BIG-CELL ! BIG-CELL construct big-state s0 BIG BIG-CELL @ ; ;package FSM-BIG:GO" SRC+
   SRC$ ;


: SIZE ( -- )
   BIG$ TEST-EVAL:N STATES 1- T=
   SRC-LEN @ MIB > TTRUE ;

public

\ The texts open packages of their own, so RUN runs outside FSM-TEST.
: RUN ( -- )
   T-RESET
   ENTRIES
   COUNTS
   FINALS
   THROWS
   FAILS
   TOTAL
   ROWS
   STEPS
   SYNTAX
   REJECTED
   SIZE
   SRC-RELEASE
   T-REPORT ;

;package

FSM-TEST:RUN
