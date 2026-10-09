\ src/host/gforth/reader.fs - Habu's reader on the Gforth host: the token loop
\ over a whole-file buffer, the recognizer order, the keyword rows, the
\ definers and the definer -> checker protocol (docs/bootstrap.md stage 1).
\
\ The host reads Habu source itself: INP-CELL and INE-CELL are the cursor
\ (habu1.f EMIT-TOK), a Habu token resolves through Habu's records alone, and
\ a definition compiles through Gforth's compiler into a header-less xt held
\ in its record's code cell. Interpret state (habu2.f EM-COMMENT, EM-INTERPRET
\ 10190): a comment, `:`, the keyword rows (10110-10143), a number, LFIND,
\ LFINDUSED. Compile state (EM-COMPILE-LEGACY 12793): `;`, a local, the keyword
\ rows (11315-11374), a literal, then the call (LFIND, LFINDUSED).

\ ---- record flags and host faults -------------------------------------------
: FLAG! ( mask ix -- ) REC >FLAGS dup @ rot or swap ! ;
: REC-FLAG+ ( mask rec -- ) >FLAGS dup @ rot or swap ! ;
\ RC-DIE: a message line on fd 2, then exit rc with no exit hook.
: RC-DIE ( c-addr u rc -- ) >r ERR ERR-NL r> (bye) ;
\ A host stack the program overran or read below: native's stacks are guarded
\ mappings and src/habu/crash.f reports the fault as `hb: stack bounds exceeded
\ (<stack>)`, exit ENGINE-ERROR:STACK-BOUNDS, with no exit hook.
102 constant STACK-BOUNDS-RC         \ src/core/engine-error.f:24 ENGINE-ERROR:STACK-BOUNDS
: STACK-FAULT ( c-addr u -- )
   s" hb: stack bounds exceeded (" ERR ERR s" )" STACK-BOUNDS-RC RC-DIE ;

\ ---- fd-1 output (habu1.f BDOT BCR BTYPE) ------------------------------------
\ Every byte leaves through write(1) unbuffered, as rt.f G-OUT does; `.`
\ prints signed decimal and a newline (rt.f G-PRINT9).
: HB-TYPE ( c-addr u -- ) 1 -rot write drop ;
: HB-CR ( -- ) s\" \n" HB-TYPE ;
: HB-DOT ( n -- ) DEC$ HB-TYPE HB-CR ;

\ ---- the user return stack and the loop stack ------------------------------
\ Habu's >r and do-loop frames live on host stacks of their own, as on native
\ (src/habu/stack-abi.f:45-49: 64 KiB each; a loop frame is the index, then
\ the limit). Gforth's return stack holds Gforth's frames only. catch saves
\ both depths and a throw restores them (habu2.f:11790-11791).
$10000 constant RSTK-BYTES           \ src/habu/stack-abi.f:45 RETURN-BYTES
$10000 constant LSTK-BYTES           \ src/habu/stack-abi.f:48 LOOP-BYTES
RSTK-BYTES RESERVE constant RSTK
LSTK-BYTES RESERVE constant LSTK
variable RSP  RSTK RSP !
variable LSP  LSTK LSP !
: RT->R ( x -- )
   RSP @ dup RSTK RSTK-BYTES + u>= if s" return" STACK-FAULT then
   ! cell RSP +! ;
: RS-TOP ( -- addr )
   RSP @ dup RSTK u<= if s" return" STACK-FAULT then cell- ;
: RT-R> ( -- x ) RS-TOP dup RSP ! @ ;
: RT-R@ ( -- x ) RS-TOP @ ;
\ A block of w cells keeps its order on the return stack, its deepest cell
\ deepest, as native's LP2RS moves one (habu2.f:10275-10290): one transfer
\ for 2>r's pair or a value of w cells, so any later transfer of its values
\ finds their cells in place.
: RT-N>R ( x*w w -- )
   dup cells RSP @ + dup RSTK RSTK-BYTES + u> if s" return" STACK-FAULT then
   {: w top :}  w 0 ?do  top i 1+ cells - !  loop  top RSP ! ;
: RT-NR@ ( w -- x*w )
   dup cells RSP @ swap - dup RSTK u< if s" return" STACK-FAULT then
   {: w base :}  w 0 ?do  base i cells + @  loop ;
: RT-NR> ( w -- x*w ) dup >r RT-NR@  r> cells negate RSP +! ;
: FRAME ( -- addr ) LSP @ 2 cells - ;
: RT-DO ( limit start -- )
   LSP @ dup LSTK LSTK-BYTES + u>= if s" loop" STACK-FAULT then
   >r r@ ! r@ cell+ ! r> 2 cells + LSP ! ;
\ An unloop with no frame faults here, on the pointer move; native faults only
\ at the next frame access, so a frameless unloop nothing follows exits 0 there.
: RT-UNLOOP ( -- )
   LSP @ LSTK u<= if s" loop" STACK-FAULT then -2 cells LSP +! ;
: RT-I ( -- n ) FRAME @ ;
: RT-J ( -- n ) FRAME 2 cells - @ ;
\ The loop counts (habu2.f:2726-2785): `loop` turns again while index+1 <
\ limit, signed; `+loop` stops when the step carries index-limit across zero
\ in the step's direction; `?do` enters while start < limit before `loop`,
\ while start <> limit before `+loop` (its closer sets the mode cell).
: RT-LOOP ( -- done? ) FRAME dup @ 1+ tuck swap ! FRAME cell+ @ < 0= ;
: RT-+LOOP ( step -- done? ) FRAME {: step f :}
   f @ f cell+ @ - {: old :}
   f @ step + f !
   f @ f cell+ @ - old xor  old step xor and 0< ;
: RT-?DO ( limit start mode-addr -- enter? ) @ >r 2dup RT-DO r> if <> else > then ;
: HB-CATCH ( i*x xt -- j*x 0 | i*x n )
   RSP @ >r LSP @ >r  catch
   r> r> rot ?dup if >r RSP ! LSP ! r> else 2drop 0 then ;
: HB-FINALLY ( i*x xt1 xt2 -- j*x ) >r HB-CATCH r> execute throw ;

\ ---- the input cursor (habu1.f EMIT-TOK, BPARSE-NAME) ------------------------
\ A token is a run of bytes above 32; INP lands on its delimiter. At INE the
\ answer is INE and 0.
: SKIP-BLANK ( p e -- p' )
   begin 2dup u< while over c@ bl > if drop exit then swap 1+ swap repeat drop ;
: SCAN-WORD ( p e -- p' )
   begin 2dup u< while over c@ bl <= if drop exit then swap 1+ swap repeat drop ;
: HB-PARSE-NAME ( -- c-addr u )
   INP-CELL D@ INE-CELL D@ tuck SKIP-BLANK tuck swap SCAN-WORD
   dup INP-CELL D! over - ;
\ The reader's own token also names itself to the capacity diagnostics.
: TOKEN ( -- c-addr u ) HB-PARSE-NAME 2dup TKL-CELL D! TKA-CELL D! ;
: SKIP-LINE ( -- )                   \ `\`: to the line's end
   INP-CELL D@ INE-CELL D@ {: p e :}
   begin p e u< while p c@ 10 = if p INP-CELL D! exit then p 1+ to p repeat
   e INP-CELL D! ;
: SKIP-PAREN ( -- )                  \ `(`: past the next `)` (habu2.f COMEND-MSG$)
   INP-CELL D@ INE-CELL D@ {: p e :}
   begin p e u< while p c@ [char] ) = if p 1+ INP-CELL D! exit then p 1+ to p repeat
   e INP-CELL D!  s" hb: source ended inside a ( comment" ERR 74 COMPILE-DIE ;
: COMMENT ( c-addr u -- flag )       \ EM-COMMENT: a comment is consumed
   2dup s" \" str= if 2drop SKIP-LINE true exit then
   s" (" str= if SKIP-PAREN true exit then false ;

\ ---- the body capture (habu1.f EMIT-BCAP; habu2.f LBCS) ---------------------
\ Each captured spelling is followed by one space. The hook reads the buffer.
: BODY ( -- addr ) BODYBUF-OFF D ;
: BODY$ ( -- c-addr u ) BODY BODYLEN-CELL D@ ;
: DREC-NAME ( -- c-addr u ) BODY$ 2dup bl scan nip - ;   \ C-PUSH-DREC-NAME
: BCS ( c-addr u -- )
   BODYLEN-CELL D@ over + 1+ BODYBUF-CAP > if
      2drop s" hb: definition source longer than the capture buffer" 71 RC-DIE then
   tuck BODY BODYLEN-CELL D@ + swap move
   BODYLEN-CELL D@ + dup BODY + bl swap c!  1+ BODYLEN-CELL D! ;
: BODY-SEED ( c-addr u -- ) 0 BODYLEN-CELL D!  BCS ;
\ The tokens of a span, one capture each: an operand a parsing word read.
: BCS-TOKENS ( c-addr u -- )
   begin
      begin dup while over c@ bl <= while 1 /string repeat then
      dup while
      2dup bl scan 2swap 2 pick - BCS
   repeat 2drop ;

\ ---- the declaration owners (habu2.f DECL-OWNER, DEF-TRUST) ------------------
\ The checker publishes its declaration table into TARGET-DECL-CELL and, at
\ its claim, into DECL-CELL. A definer notifies the source owner's slot, then
\ the target's when it is a different xt.
: OWNER-XT ( cell off -- xt|0 ) swap D@ dup if + @ else nip then ;
: SRC-XT ( off -- xt|0 ) DECL-CELL swap OWNER-XT ;
: TGT-XT ( off -- xt|0 ) TARGET-DECL-CELL swap OWNER-XT ;
: TGT-ONLY ( off -- xt|0 ) dup TGT-XT dup rot SRC-XT = if drop 0 then ;
: SIG-NOTIFY ( a u sa su off -- ) {: a u sa su off :}   \ DEF-TRUST:REGISTER
   off SRC-XT ?dup if >r a u sa su r> execute then
   off TGT-ONLY ?dup if >r a u sa su r> execute then ;
: NAME-NOTIFY ( a u off -- ) {: a u off :}              \ C-CALL-CHECKER-DEFER
   off SRC-XT ?dup if >r a u r> execute then
   off TGT-ONLY ?dup if >r a u r> execute then ;
: TGT-CALL ( i*x off -- j*x ) TGT-XT ?dup if execute then ;  \ DECL-OWNER:TARGET
: TSIG ( -- a u ) TSIG-A-CELL D@ TSIG-U-CELL D@ ;
: NAME-SIG-CALL ( sa su xt -- ) >r DREC-NAME 2swap r> execute ;
: SRC-SIG ( sa su off -- )            \ DECL-OWNER:SIGNATURE: the source owner only
   SRC-XT ?dup if NAME-SIG-CALL else 2drop then ;
: SIG-REGISTER ( sa su off -- )       \ DEF-TRUST:REGISTER, REGISTER-IDENTITY
   >r 2dup r@ SRC-SIG  r> TGT-ONLY ?dup if NAME-SIG-CALL else 2drop then ;
\ LASTC-TRUST:PUBLISH, PUBLISH-PTR-A, PUBLISH-A: trust-raw of the source owner,
\ or of the target while a hook is installed, then the target's when it differs.
: RAW-PUBLISH ( sa su -- )
   RAW-OFF SRC-XT ?dup 0= if
      HOOK-CELL D@ 0= if 2drop exit then
      RAW-OFF TGT-XT ?dup 0= if 2drop s" trust-raw" RC-REJECT RC-DIE then
   then >r 2dup r> NAME-SIG-CALL
   DECL-CELL D@ 0= if 2drop exit then
   RAW-OFF TGT-ONLY ?dup if NAME-SIG-CALL else 2drop then ;

\ ---- wordlists and packages (habu2.f C-PACKAGE ... C-END-USING) ------------
\ A wid is a number (WIDN-CELL counts them); a package is a namespace record
\ whose code cell holds its public wid and whose length cell its private one.
: NEW-WID ( -- wid ) WIDN-CELL D@ dup 1+ WIDN-CELL D! ;
: HB-GET-CURRENT ( -- wid ) CUR-CELL D@ ;
: HB-SET-CURRENT ( wid -- ) CUR-CELL D! ;
: PKG-FIND ( c-addr u -- rec|0 ) WL-NAMESPACE REC-FIND ;
: NAMESPACE ( c-addr u pub pri -- rec )
   {: a u pub pri :} pub a u WL-NAMESPACE REC-PEND  pri over cell+ !  REC-PUBLISH ;
: PKG-DIE ( c-addr u -- ) s" hb: " ERR ERR RC-REJECT COMPILE-DIE ;
: HB-PACKAGE ( "name" -- )
   PKG-PUB-CELL D@ if s" package: already open" PKG-DIE then
   TOKEN dup 0= if 2drop s" package: no name" PKG-DIE then
   PACKAGE-OFF TGT-XT ?dup if >r 2dup r> execute then
   2dup PKG-FIND ?dup if nip nip else NEW-WID NEW-WID NAMESPACE then {: rec :}
   USE-DEPTH-CELL D@ USE-PKG-SAVE-CELL D!
   CUR-CELL D@ PKG-PARENT-CELL D!
   rec @ PKG-PUB-CELL D!  rec cell+ @ PKG-PRI-CELL D!  rec PKG-REC-CELL D!
   rec cell+ @ CUR-CELL D! ;
: PKG-OPEN? ( -- ) PKG-PUB-CELL D@ 0= if s" no package open" PKG-DIE then ;
: HB-PUBLIC ( -- ) PKG-OPEN? PUBLIC-OFF TGT-CALL  PKG-PUB-CELL D@ CUR-CELL D! ;
: HB-PRIVATE ( -- ) PKG-OPEN? PRIVATE-OFF TGT-CALL  PKG-PRI-CELL D@ CUR-CELL D! ;
: HB-END-PACKAGE ( -- )
   PKG-OPEN? END-PACKAGE-OFF TGT-CALL
   PKG-PARENT-CELL D@ CUR-CELL D!
   0 PKG-PUB-CELL D!  0 PKG-PRI-CELL D!  0 PKG-PARENT-CELL D!  0 PKG-REC-CELL D!
   USE-PKG-SAVE-CELL D@ USE-DEPTH-CELL D! ;
: HB-USING ( "name" -- )             \ habu2.f C-USING, C-USING-PUSH
   TOKEN 2dup PKG-FIND dup 0= if drop s" using: unknown package" PKG-DIE then @
   USE-DEPTH-CELL D@ dup USE-MAX >= if s" using: too many" PKG-DIE then
   cells USE-WIDS-OFF + D!
   USING-OFF NAME-NOTIFY
   USE-DEPTH-CELL D@ 1+ USE-DEPTH-CELL D! ;
: HB-END-USING ( -- )
   USE-DEPTH-CELL D@ 0= if s" ;using: none open" PKG-DIE then
   USE-DEPTH-CELL D@ 1- USE-DEPTH-CELL D! ;

\ ---- lookup (habu1.f EMIT-FIND 5364; habu2.f EMIT-FIND-USED 9719) ---------
variable SEED-N                      \ the seeded primitive records, dict[0..SEED-N)
: COLON-AT ( c-addr u -- idx|-1 )
   0 ?do dup i + c@ [char] : = if drop i unloop exit then loop drop -1 ;
: BARE-FIND ( c-addr u -- rec|0 )   \ private, public, then global; newest first
   PKG-PRI-CELL D@ ?dup if >r 2dup r> REC-FIND ?dup if nip nip exit then then
   PKG-PUB-CELL D@ ?dup if >r 2dup r> REC-FIND ?dup if nip nip exit then then
   0 REC-FIND ;
\ NAME:tail, a colon after the first byte and before the last: tail in NAME's
\ public; a second colon in the tail finds nothing (FIND-QBAD). When NAME is
\ the open package, a miss there falls to global (FIND-DONE).
: LFIND ( c-addr u -- rec|0 )
   2dup COLON-AT {: a u p :}
   p 1 < p 1+ u >= or if a u BARE-FIND exit then
   a p + 1+ u p - 1- {: ta tu :}
   ta tu COLON-AT 0 >= if 0 exit then
   a p PKG-FIND ?dup 0= if 0 exit then  @ {: wid :}
   ta tu wid REC-FIND ?dup if exit then
   PKG-PRI-CELL D@ 0= if 0 exit then
   wid PKG-PUB-CELL D@ = if ta tu 0 REC-FIND exit then 0 ;
: USED-FIND ( c-addr u -- rec1 rec2 n )   \ LFINDUSED-CORE: distinct records met
   0 0 0 {: a u r1 r2 n :}
   a u COLON-AT 0 >= if 0 0 0 exit then
   USE-DEPTH-CELL D@ 0 ?do
      a u USE-WIDS-OFF i cells + D@ REC-FIND ?dup if
         dup r1 = if drop else
            n 0= if to r1 else to r2 then  n 1+ to n
            n 2 >= if leave then
         then
      then
   loop r1 r2 n ;
: HB-SCOPE-FIND ( c-addr u -- rec used1 used2 flags )   \ habu2.f BSCOPEFIND
   2dup LFIND >r USED-FIND {: r1 r2 n :} r> {: b :}
   n 2 >= if 0 else n 1 = if r1 else 0 then then {: one :}
   b 0= if one to b then
   b  n 2 >= if r1 r2 else one 0 then
   n 2 >= 2 and  b 0<> b SEED-N @ REC u< and 1 and or ;
\ LFIND, then the used publics; two distinct records there are the
\ ambiguity death (ENGINE-ERROR:USING-AMBIGUOUS, 94).
: LOOKUP-REC ( c-addr u -- rec|0 )
   2dup LFIND ?dup if nip nip exit then
   2dup USED-FIND {: r1 r2 n :}
   n 2 >= if
      s" hb: ambiguous bare word resolves in multiple used packages: " ERR ERR
      94 COMPILE-DIE then
   2drop n if r1 else 0 then ;
\ One-wordlist lookup (habu1.f WLFIND): search-wl answers the record's code
\ cell, never for the owner API's private wid or an internal record.
: HB-SEARCH-WL ( c-addr u wid -- xt|0 )
   dup OWNER-API-PRI-WID = if drop 2drop 0 exit then
   REC-FIND dup 0= if exit then
   dup >FLAGS @ DNAME-INT and if drop 0 exit then  @ ;
: HB-TOK-IMM? ( c-addr u -- n )      \ habu2.f BTOKIMM
   LFIND dup if >FLAGS @ DNAME-IMM and if 2 else 0 then then ;
\ scope-kind? (habu2.f BSCOPE-KIND): the host loads none of the seven C2 scope
\ entries, so no entry it holds is one of them.
: HB-SCOPE-KIND? ( xt -- n ) drop 0 ;
: GLOBAL-XT ( c-addr u -- xt )
   2dup 0 HB-SEARCH-WL ?dup if nip nip exit then RC-REJECT RC-DIE ;

\ ---- the undefined-word diagnostic (habu2.f EM-COMPILE-UNDEF) ---------------
\ LUNDEF: "E-UNDEFINED: " and the token on fd 2, then DIAG-RET.
: UNDEF-DIE ( c-addr u -- ) s" E-UNDEFINED: " ERR ERR ERR-NL DIAG-RET ;

\ ---- num-parse (habu1.f EMIT-NUM at LNUM, BNUMPARSE) ------------------------
\ ( c-addr u -- value float? ok? ): optional '-', optional '$' (base 16),
\ digits; one '.' in base 10 starts a fraction. An overflowed or unreadable
\ spelling, or one ending on its '.', answers 0 0 0.
variable FBITS
$7FFFFFFFFFFFFFFF constant INT64-MAX
: NP-DIGIT ( char base -- d true | false )
   >r dup [char] 0 [char] 9 1+ within if [char] 0 - r> drop true exit then
   r> 16 <> if drop false exit then
   dup [char] a [char] f 1+ within if 87 - true exit then
   dup [char] A [char] F 1+ within if 55 - true exit then
   drop false ;
: NUM-PARSE ( c-addr u -- value float? ok? )
   0 1 0 10 0 0 0 0 0 0 {: a u val sgn ix base ovf frac? fr sc ch d :}
   u 0= if 0 0 0 exit then
   a c@ to ch  ch [char] - = if -1 to sgn 1 to ix then
   ix u >= if 0 0 0 exit then
   a ix + c@ to ch  ch [char] $ = if 16 to base ix 1+ to ix then
   ix u >= if 0 0 0 exit then
   begin ix u < while
      a ix + c@ to ch
      ch [char] . = if
         base 10 <> frac? or if 0 0 0 exit then
         true to frac? 0 to fr 1 to sc
      else
         ch base NP-DIGIT 0= if 0 0 0 exit then to d
         frac? if
            fr INT64-MAX d - 10 0 swap um/mod nip > if 1 to ovf then
            sc $0CCCCCCCCCCCCCCC > if 1 to ovf then
            ovf 0= if fr 10 * d + to fr  sc 10 * to sc then
         else
            val -1 d - 0 base um/mod nip u> if 1 to ovf then
            val base * d + to val
         then
      then
      ix 1+ to ix
   repeat
   frac? if
      ch [char] . = if 0 0 0 exit then
      val INT64-MAX u> ovf or if 0 0 0 exit then
      val s>f fr s>f sc s>f f/ f+  sgn 0< if fnegate then
      FBITS df! FBITS @ true true exit
   then
   base 10 = if val INT64-MAX sgn 0< 1 and + u> ovf or to ovf then
   ovf if 0 0 0 exit then
   val sgn * false true ;

\ ---- qualified definition names (habu2.f EMIT-QUALIFY-DEF) -----------------
\ NAME:tail, its first colon after the first byte and before the last,
\ defines tail in NAME's public wordlist; with no package NAME a namespace
\ record is made first, with one fresh public wid and private 0. A colon in
\ the tail is C-QUALIFY-FAIL $4B. The host serves no SEAL-CAPTURE, so no
\ package is sealed (habu2.f C-QUALIFY-SEAL-GUARD never fires).
: QUALIFY ( c-addr u -- c-addr' u' wid )
   2dup COLON-AT {: a u p :}
   p 1 < p 1+ u >= or if a u CUR-CELL D@ exit then
   a p + 1+ u p - 1- {: ta tu :}
   ta tu COLON-AT 0 >= if a u ERR $4B COMPILE-DIE then
   a p PKG-FIND ?dup if @ else a p NEW-WID 0 NAMESPACE @ then  ta tu rot ;
: NAME-DIE ( kw-a kw-u -- )          \ habu2.f C-DIE-TOKEN $4A
   s" hb: reader keyword needs a name: " ERR ERR $4A COMPILE-DIE ;
: DEF-NAME ( kw-a kw-u -- c-addr u ) TOKEN dup if 2swap 2drop else 2drop NAME-DIE then ;

\ ---- signatures (habu2.f C-SIG-START, C-SIG-END, C-SIG-CAPTURE-TSIG) -------
\ SIG-SCAN answers the text inside the `( ... )` after the cursor and 1, 0 when
\ the next byte is not `(`, or -1 when the input ends first.
: SIG-SCAN ( -- a u 1 | 0 | -1 )
   INP-CELL D@ INE-CELL D@ {: p e :}
   p e SKIP-BLANK to p
   p e u< 0= if -1 exit then
   p c@ [char] ( <> if 0 exit then
   p 1+ dup begin dup e u< while
      dup c@ [char] ) = if over - 1 exit then 1+
   repeat 2drop -1 ;
\ C-SIG-BAD (habu2.f:3294): the last token, then the refusal tail with rc 76.
: SIG-BAD ( -- ) TKA-CELL D@ TKL-CELL D@ ERR REFUSE-RC COMPILE-DIE ;
: SIG-ENDED ( -- )   \ LSRCEND
   s" hb: source ended inside definition: " ERR DREC-NAME ERR 74 COMPILE-DIE ;
\ SIG-PEEK answers a signature its definer requires (C-PARSE-REQUIRED-SIG,
\ C-PARSE-CREATED-SIG: any miss is SIG-BAD) without consuming it; SIG-CAPTURE
\ consumes it, captures its full text and answers its inner text, saved, since
\ a logged row is read at the claim.
: SIG-PEEK ( -- a u ) SIG-SCAN 1 <> if SIG-BAD then ;
: SIG-CAPTURE ( a u -- a' u' ) 2dup + 1+ INP-CELL D!  over 1- over 2 + BCS  save-mem ;
: SIG-REQUIRE ( -- a u ) SIG-PEEK SIG-CAPTURE ;
\ `:` may carry a signature (C-COLON-MAYBE-SIG); TRUSTED: must, and a byte other
\ than `(` is SIG-BAD (C-PARSE-TRUST-SIG). For both, input that ends before the
\ signature closes ends inside the definition.
: SIG-OPEN ( trusted -- a u true | false ) {: t :}
   SIG-SCAN dup 0< if SIG-ENDED then
   0= if t if SIG-BAD then false exit then SIG-CAPTURE true ;

\ ---- the declared-row log (layout.f DECLARED-LOG, C-DECLARED-LOG-APPEND) ----
: DLOG ( -- addr ) DLOG-OFF D ;
: DLOG-APPEND ( -- )
   DLOG @ DLOG-CAP >= if
      s" hb: declared-row log full: " ERR DREC-NAME 72 RC-DIE then
   DLOG @ DLOG-SLOT * DLOG-SLOTS-REL + DLOG +
   PEND-CELL D@ over !  PKG-REC-CELL D@ over DLOG-PKG-OFF + !
   TSIG-A-CELL D@ over DLOG-SIG-A-OFF + !  TSIG-U-CELL D@ swap DLOG-SIG-U-OFF + !
   1 DLOG +!  DLOG DLOG-ADDR-CELL D! ;

\ ---- publication (habu2.f EM-COMPILE-PUBLISH, EM-REC-WIDE-PUBLISH) ---------
\ EM-REC-WIDE-PUBLISH: drain the checker's wide and min-in latches onto the
\ record just published.
: REC-WIDE ( -- )
   HOOK-CELL D@ 0= if exit then
   s" rec-wide-publish" GLOBAL-XT execute
   s" rec-min-in@" GLOBAL-XT execute
   ?dup if $FF and 52 lshift NDICT @ 1- FLAG! then ;
: CLEAR-DEF ( -- )
   0 PEND-CELL D!  0 TRUSTED-CELL D!  0 TSIG-A-CELL D!  0 TSIG-U-CELL D!
   0 DOESB-CELL D!  0 TCSIG-A-CELL D!  0 TCSIG-U-CELL D! ;
: DIE-DOES ( -- ) s" does>" RC-REJECT RC-DIE ;   \ habu2.f C-DIE-DOES
: CHECK-DOES ( -- )                              \ habu2.f C-CALL-CHECK-DOES
   BODY DOESB-CELL D@ +  BODYLEN-CELL D@ DOESB-CELL D@ -
   TCSIG-A-CELL D@ TCSIG-U-CELL D@  s" check-does!" GLOBAL-XT execute
   -1 <> if DIE-DOES then ;
: DEFINER-LEN ( -- u ) DOESB-CELL D@ ?dup if 6 - else BODYLEN-CELL D@ then ;
\ Under the hook only a TRUSTED: body reaches `;` here; every other colon
\ definition is codegen.fs's CG-COLON.
: PUBLISH-HOOKED ( -- )                          \ EM-COMPILE-PUBLISH-TRUSTED
   DOESB-CELL D@ if TCSIG-U-CELL D@ 0= if DIE-DOES then CHECK-DOES then
   TSIG EFFECT-OFF SIG-REGISTER ;
: PUBLISH-NOHOOK ( -- )                          \ EM-COMPILE-PUBLISH-HOOKED nohook arm
   TSIG-U-CELL D@ 0= if exit then
   TRUSTED-CELL D@ if TSIG EFFECT-OFF SRC-SIG exit then
   DECL-CELL D@ 0= if DLOG-APPEND exit then
   TSIG DECLARED-ROW-OFF SRC-SIG ;
\ Gforth's `;` closes a definition and a quotation alike (kernel/cond.fs
\ `;]` is `POSTPONE ; swap execute`); the xt it leaves names which. CLOSE-SYS
\ answers whether it closed the definition DEF-OPEN's `:noname` made. Closing
\ the wrong one is refused with the code Gforth's own check throws.
-22 constant CS-MISMATCH             \ Gforth kernel/cond.fs ?struc: control structure mismatch
variable DEF-XT
: CLOSE-SYS ( colon-sys -- xt def? ) postpone ;  dup DEF-XT @ = ;
\ `;` closes the body first, so Gforth's structure check refuses malformed
\ control flow, and CLOSE-SYS an open quotation, before the checker hears the
\ body. A refusal is a throw that leaves through boot.fs's MAIN with the
\ record uncounted.
: SEMI ( xt colon-sys -- )
   CLOSE-SYS 0= if CS-MISMATCH throw then
   HOOK-CELL D@ if PUBLISH-HOOKED else PUBLISH-NOHOOK then
   PEND-CELL D@ !  REC-PUBLISH REC-WIDE CLEAR-DEF ;

\ ---- create, variable, constant (EMIT-CREATE, C-CREATE, C-CONSTANT) -------
\ A created word's body is the aligned DP in Habu's data space; its Gforth word
\ holds that address and pushes it, and a does> clause fetches it first, so a
\ clause starts from the Habu body as native does. At top level each definer
\ seeds the capture with the name, runs the hook over it and its keyword
\ (C-DEFHOOK), then registers its storage through trust-raw. The seeded create
\ row a compiled body calls is CREATED by itself (habu2.f C-OP-ROW-GATE): it
\ names and publishes the word and runs no hook; its does> clause publishes
\ the created signature.
: CREATED-XT ( addr -- xt ) noname create , latestxt does> @ ;
: CONST-XT ( x -- xt ) noname constant latestxt ;
: STORE-DEF ( xt c-addr u wid kind -- )
   >r REC-PEND dup LASTC-CELL D!  r> swap REC-FLAG+  REC-PUBLISH ;
: SEED-NAME ( kw-a kw-u -- c-addr u wid ) DEF-NAME 2dup BODY-SEED QUALIFY ;
: CREATED ( "name" -- )
   s" create" SEED-NAME {: a u wid :} HB-BODY CREATED-XT a u wid DKIND-ADDR STORE-DEF ;
: DEFHOOK ( c-addr u -- ) BCS  HOOK-CELL D@ ?dup if >r BODY$ r> execute drop then ;
: I-CREATE ( "name" -- ) CREATED  s" create" DEFHOOK  s" -- ptr a" RAW-PUBLISH ;
: I-VARIABLE ( "name" -- ) I-CREATE 0 HB-, ;
: I-CONSTANT ( x "name" -- )
   s" constant" SEED-NAME {: a u wid :} CONST-XT a u wid DKIND-VAL STORE-DEF
   s" constant" DEFHOOK  s" -- a" RAW-PUBLISH ;
\ ndict! ( n -- ) (prims.f 695, habu1.f BNDSET): a count above DICT-CAP exits
\ 74; no seal sets a floor. A slot answers only while its record is counted,
\ so a raise needs no index rebuild. A count at or below the old one clears
\ LASTC when it names a record at or above it.
: HB-NDICT! ( n -- )
   dup DICT-CAP u> if drop s" hb: dictionary count out of range" 74 RC-DIE then
   dup NDICT @ > if NDICT ! exit then
   dup NDICT !  REC LASTC-CELL D@ u<= if 0 LASTC-CELL D! then ;
: HB-IMMEDIATE ( -- ) DNAME-IMM NDICT @ 1- FLAG! ;   \ habu2.f C-IMMEDIATE

\ ---- defer and the pre-trust table (habu2.f C-DEFER, BDRAINPRETRUST) -------
\ A defer's dispatch cell is its body in Habu's data space (C-DEFER-CELL),
\ holding the unset vector's xt until `is` stores one; its Gforth word holds
\ the cell's address. Before the target owner has both slots, a defer's name
\ and signature wait in the pre-trust table.
: DEFER-UNSET ( -- ) s" defer: unset execution vector" REFUSE-RC RC-DIE ;
: DEFER-XT ( cell -- xt ) noname create , latestxt does> @ @ execute ;
0 DEFER-XT >does-code constant DEFER-DOES
: DEFER? ( xt -- flag ) >does-code DEFER-DOES = ;
create PD-TABLE PD-CAP 4 * cells allot   variable PD-N
: PD-ENTRY ( i -- addr ) 4 * cells PD-TABLE + ;
: PD-CAPTURE ( a u sa su -- )
   PD-N @ PD-CAP >= if s" hb: pre-trust defer table full" 72 RC-DIE then
   save-mem 2swap save-mem PD-N @ PD-ENTRY 2!  PD-N @ PD-ENTRY 2 cells + 2!
   1 PD-N +! ;
: PRETRUST-READY? ( -- bool ) EFFECT-OFF TGT-XT 0<> DEFER-OFF TGT-XT 0<> and ;
: DEFER-DECLARED ( a u sa su -- ) {: a u sa su :}
   PRETRUST-READY? if
      a u sa su EFFECT-OFF SIG-NOTIFY  a u DEFER-OFF NAME-NOTIFY exit then
   EFFECT-OFF SRC-XT ?dup if >r a u sa su r> execute then
   DEFER-OFF SRC-XT ?dup if >r a u r> execute then
   a u sa su PD-CAPTURE ;
: HB-DEFER ( "name" "sig" -- )
   s" defer" DEF-NAME {: a u :}
   SIG-PEEK {: sa su :}
   a u QUALIFY {: qa qu wid :}
   HB-BODY ['] DEFER-UNSET HB-,  DEFER-XT qa qu wid REC-PEND drop REC-PUBLISH
   a u sa su DEFER-DECLARED ;
: HB-DRAIN-PRETRUST ( -- )
   begin PD-N @ while
      EFFECT-OFF TGT-XT ?dup 0= if s" hb: trust-decl" RC-REJECT RC-DIE then
      >r PD-N @ 1- PD-ENTRY dup 2@ rot 2 cells + 2@ r> execute
      DEFER-OFF TGT-XT ?dup if >r PD-N @ 1- PD-ENTRY 2@ r> execute then
      -1 PD-N +!
   repeat ;
\ is (habu2.f C-IS) compiles a store of the xt into its target's dispatch
\ cell. The target resolves through LFIND alone, never a used public.
: IS-DIE ( c-addr u msg-a msg-u rc -- ) >r ERR ERR ERR-NL r> throw ;
: IS-TARGET ( "name" -- xt )         \ the defer `is` names, through C-IS's walls
   TOKEN dup 0= if s" is" s" hb: is: missing target word after " $4A IS-DIE then
   2dup LFIND ?dup 0= if
      s" hb: is: no deferred word named " ERR ERR ERR-NL
      s" hb: is: parsing words resolve outside using-imports; qualify the target"
      ERR ERR-NL $46 throw then
   @ dup DEFER? 0= if drop s" hb: is: not a deferred word: " $4C IS-DIE then
   nip nip ;
: HB-IS ( "name" -- ) IS-TARGET >body @ postpone literal postpone ! ;

\ ---- export (habu2.f C-EXPORT 10051) ----------------------------------------
\ Inside a package, export publishes the word LFIND finds under the token's
\ tail into the current wordlist: a second record with the source's code
\ cell, length and IMM, WIDE and MIN-IN bits. With no package it consumes the
\ name. Walls in C-EXPORT's order: no name $4A, not found (70), internal
\ (70), dictionary full $4D, the tail already counted in the current wordlist
\ $4E; then checker-export hears the spelling.
DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or constant EXPORT-BITS
: EXPORT-TAIL ( c-addr u -- c-addr' u' )
   2dup COLON-AT {: a u p :}
   p 1 < p 1+ u >= or if a u exit then  a p + 1+ u p - 1- ;
: HB-EXPORT ( "name" -- )
   TOKEN dup 0= if ERR $4A throw then
   PKG-PUB-CELL D@ 0= if 2drop exit then
   2dup LFIND ?dup 0= if ERR RC-REJECT throw then {: a u src :}
   src >FLAGS @ DNAME-INT and if
      s" hb: internal engine word: " ERR a u ERR ERR-NL RC-REJECT throw then
   a u EXPORT-TAIL {: ta tu :}
   NDICT @ DICT-CAP >= if s" hb: dictionary full at: " ERR a u ERR $4D throw then
   ta tu CUR-CELL D@ REC-FIND if
      s" duplicate definition: " ERR a u ERR $4E throw then
   a u s" checker-export" GLOBAL-XT execute
   src @ ta tu CUR-CELL D@ REC-PEND {: rec :}
   src cell+ @ rec cell+ !
   src >FLAGS @ EXPORT-BITS and rec REC-FLAG+  REC-PUBLISH ;

\ ---- the top-row hook (habu2.f LTOPHOOK; src/habu/outer.f HOOK) ------------
\ With a hook installed (set-top-check), the interpreter passes it
\ ( token class flags ) for each number, string, counted string, char and tick
\ after its push and for each word before it runs. The token is TOKEN$: the
\ number's or word's spelling, a string's keyword, a char's or tick's operand.
\ Flags are 0 for a literal and, for a word or tick, LFIND's flag word as
\ outer.f WORD-FLAGS reads it: bit 0 found, bit 1 immediate, bits 8-15 the
\ certified inputs.
$27F0 constant TOP-HOOK-CELL          \ src/habu/layout.f:1408
1 constant TOP-EV-NUM    2 constant TOP-EV-STR    3 constant TOP-EV-CSTR   \ layout.f:1596-1601
4 constant TOP-EV-CHAR   5 constant TOP-EV-TICK   6 constant TOP-EV-WORD
: TOP-HOOK ( class flags -- )
   TOP-HOOK-CELL D@ ?dup if >r TOKEN$ 2swap r> execute else 2drop then ;
: WORD-FLAGS ( rec -- n )
   >FLAGS @ {: f :}
   f DNAME-IMM and if 3 else 1 then  f DNAME-MIN-IN-MASK and 52 rshift 8 lshift or ;

\ ---- the walls (habu2.f:11933-11952 LWIDE, LINTERNAL, LMININ) --------------
\ A wall's refusal is its message and the token on fd 2, then a throw of
\ RC-REJECT, as native's inside an evaluate frame, where a `--load` program
\ always runs (habu2.f:11908-11921 LDIAGRET to LEVALREC). With no catch to
\ receive it, it leaves through boot.fs's UNCAUGHT, the exit hook and then rc
\ 70, as LEVALREC with no handler falls into LUNCAUGHT (habu2.f:11795-11826).
: WALL ( c-addr u -- ) ERR TOKEN$ ERR ERR-NL  RC-REJECT throw ;
\ A record's walls, raised before its name becomes a call or an address, in
\ native's order: a word whose effect is wider than a cell (DNAME-WIDE) would
\ land a bundle on the untyped interpret stack, and an internal engine word
\ (DNAME-INT) has no effect the checker knows. The interpreter's call and `'`
\ raise both (habu2.f:10162-10163 EM-INTERPRET-FIND, 5772-5773 C-TICK); a
\ body's `[']` raises DNAME-INT alone (habu2.f:5814 C-BTICK).
: REC-GATE ( rec mask -- rec )
   over >FLAGS @ and {: f :}
   f DNAME-WIDE and if s" hb: interpret-mode layout value: " WALL then
   f DNAME-INT and if s" hb: internal engine word: " WALL then ;

\ ---- the stack floor (habu1.f:3411 B-EVAL-CLOSED; habu2.f LMININ, LFLOORREC) --
\ Native reads each text on a stack of its own, so a token that would take a
\ cell below the text's first is refused: a word whose certified inputs
\ (DNAME-MIN-IN) the stack lacks before it runs, any other token at its first
\ read under the base. The refusal is the wall
\ `hb: interpret stack underdepth: <token>`. The host's texts share Gforth's
\ stack, and FLOOR-DEPTH is the depth the text began at: the inputs are checked
\ against it before a word runs, and a token that took cells from under it is
\ refused as it returns.
variable FLOOR-DEPTH
: UNDERDEPTH ( -- ) s" hb: interpret stack underdepth: " WALL ;
: MIN-IN ( rec -- n ) >FLAGS @ DNAME-MIN-IN-MASK and 52 rshift ;

\ ---- strings, char and tick (habu2.f 5372-5727, C-TICK 5761, C-BTICK 5803) --
\ A string's text runs from one past its keyword's delimiter to the next `"`
\ before INE; INP lands after the `"`. The escaped forms take \" \q \\ \a \b
\ \e \l \f \n \r \t \v \z and \x with two hex digits (habu2.f:5422); any other
\ escape, or no closing `"`, is BADSTR (rc 74).
: BAD-STRING ( -- ) s" hb: bad string literal" ERR 74 COMPILE-DIE ;
: STR-TEXT ( -- c-addr u )
   INP-CELL D@ 1+ {: a :}
   a begin dup INE-CELL D@ u< while
      dup c@ [char] " = if dup 1+ INP-CELL D!  a - a swap exit then 1+
   repeat drop BAD-STRING ;
: ESC-BYTE ( c -- c' | -1 )
   case
      [char] " of 34 endof   [char] q of 34 endof   [char] \ of 92 endof
      [char] a of 7 endof    [char] b of 8 endof    [char] e of 27 endof
      [char] l of 10 endof   [char] f of 12 endof   [char] n of 10 endof
      [char] r of 13 endof   [char] t of 9 endof    [char] v of 11 endof
      [char] z of 0 endof
      -1 swap
   endcase ;
: HEX-DIGIT ( c -- n | -1 )
   dup [char] 0 [char] 9 1+ within if [char] 0 - exit then
   dup [char] a [char] f 1+ within if 87 - exit then
   dup [char] A [char] F 1+ within if 55 - exit then
   drop -1 ;
variable ESC-BUF  variable ESC-CAP
256 dup allocate throw ESC-BUF ! ESC-CAP !
: ESC-ROOM ( u -- )
   dup ESC-CAP @ > if ESC-BUF @ over resize throw ESC-BUF ! dup ESC-CAP ! then drop ;
: ESC-TEXT ( -- c-addr u )
   INP-CELL D@ 1+ INE-CELL D@ 0 {: p e n :}
   e p - 0 max ESC-ROOM
   begin
      p e u< 0= if BAD-STRING then
      p c@ dup [char] " <> while
      [char] \ = if
         p 1+ to p  p e u< 0= if BAD-STRING then
         p c@ dup [char] x = swap [char] X = or if
            p 2 + e u< 0= if BAD-STRING then
            p 1+ c@ HEX-DIGIT p 2 + c@ HEX-DIGIT
            2dup or 0< if BAD-STRING then swap 4 lshift or
            p 2 + to p
         else
            p c@ ESC-BYTE dup 0< if BAD-STRING then
         then
      else p c@ then
      ESC-BUF @ n + c!  n 1+ to n  p 1+ to p
   repeat drop
   p 1+ INP-CELL D!  ESC-BUF @ n ;
\ Interpret-state strings land in Habu's data space: s" and s\" copy their
\ bytes to DP and push ( addr u ); c" and c\" lay a count byte, then the
\ bytes, and push the counted address (LCSTR, rc 76 over 255 bytes).
: DP-STR ( c-addr u -- addr u ) HB-HERE {: a u d :} u HB-ALLOT  a d u move  d u ;
: CSTR-CHECK ( u -- u )
   dup 255 > if s" hb: counted string too long (max 255)" ERR REFUSE-RC COMPILE-DIE then ;
: DP-CSTR ( c-addr u -- addr ) CSTR-CHECK dup HB-C, DP-STR drop 1- ;
: HB-ISQ ( "text" -- addr u ) STR-TEXT DP-STR TOP-EV-STR 0 TOP-HOOK ;
: HB-IESQ ( "text" -- addr u ) ESC-TEXT DP-STR TOP-EV-STR 0 TOP-HOOK ;
: HB-ICQ ( "text" -- addr ) STR-TEXT DP-CSTR TOP-EV-CSTR 0 TOP-HOOK ;
: HB-IECQ ( "text" -- addr ) ESC-TEXT DP-CSTR TOP-EV-CSTR 0 TOP-HOOK ;
: HB-IDOTQ ( "text" -- ) STR-TEXT HB-TYPE ;
: HB-IEDOTQ ( "text" -- ) ESC-TEXT HB-TYPE ;
\ Compiled strings live with the code: Gforth's sliteral copies the bytes into
\ the definition; a counted string is its own allocation, never freed, as code.
: CSTR-LIT ( c-addr u -- )
   CSTR-CHECK dup 1+ allocate throw {: a u m :}
   u m c!  a m 1+ u move  m postpone literal ;
: KC-SQ ( "text" -- ) STR-TEXT postpone sliteral ;
: KC-ESQ ( "text" -- ) ESC-TEXT postpone sliteral ;
: KC-CQ ( "text" -- ) STR-TEXT CSTR-LIT ;
: KC-ECQ ( "text" -- ) ESC-TEXT CSTR-LIT ;
: KC-DOTQ ( "text" -- ) STR-TEXT postpone sliteral ['] HB-TYPE compile, ;
: KC-EDOTQ ( "text" -- ) ESC-TEXT postpone sliteral ['] HB-TYPE compile, ;
: CHAR-OF ( kw-a kw-u -- c ) DEF-NAME drop c@ ;
: HB-ICHAR ( "name" -- c ) s" char" CHAR-OF TOP-EV-CHAR 0 TOP-HOOK ;
: KC-BCHAR ( "name" -- ) s" [char]" CHAR-OF postpone literal ;
\ ' and ['] resolve through LFIND then LFINDUSED, never a keyword row, a
\ number or a local.
: TICK-REC ( -- c-addr u rec ) TOKEN 2dup LOOKUP-REC dup 0= if drop UNDEF-DIE then ;
: HB-ITICK ( "name" -- xt )
   TICK-REC nip nip DNAME-WIDE DNAME-INT or REC-GATE
   dup @ swap TOP-EV-TICK swap WORD-FLAGS TOP-HOOK ;
: HB-BTICK ( "name" -- ) TICK-REC nip nip DNAME-INT REC-GATE @ postpone literal ;

\ ---- locals ----------------------------------------------------------------
\ {: name:type ... :} declares Gforth cell locals, the first name deepest:
\ Gforth's (local) gives its first name the top cell, so the names go in
\ reverse. A type is stripped; each local holds one cell.
: STRIP-TYPE ( c-addr u -- c-addr u' ) 2dup s" :" search if nip - else 2drop then ;
\ The locals in scope, counted, as native counts them (LOCN-CELL): every
\ control opener saves the count and its closer restores it (habu2.f LCFPUSH,
\ LCFPOP), so a local declared inside a structure is gone after it. A name is
\ the latest local in scope of exactly its bytes, unlike a word (habu2.f
\ LLOC-FIND), on both paths: the capture (codegen.fs) and this reader's
\ compile (LOCAL-REF?). A record holds the name's offset in the body buffer,
\ which no other local of the definition shares, and its length. A frame
\ holds its kind and the count its opener saved. Every local and opener is a
\ captured token of at least two bytes, so half the body buffer bounds both
\ tables.
create CLOC BODYBUF-CAP 2/ 2* cells allot  variable CLOCN
create CFR BODYBUF-CAP 2/ 2* cells allot  variable CFRN
: LOC ( i -- addr ) 2* cells CLOC + ;
: LOC$ ( i -- c-addr u ) LOC 2@ BODY + swap ;
: LOC-ADD ( c-addr u -- )              \ the name token BCS just captured
   dup BODYLEN-CELL D@ swap - 1-  -rot STRIP-TYPE nip swap  CLOCN @ LOC 2!  1 CLOCN +! ;
: LOCAL-FIND ( c-addr u -- off|-1 )    \ the latest local of the name in scope
   CLOCN @ begin dup while 1-
      >r 2dup r@ LOC$ str= if 2drop r> LOC @ exit then  r>
   repeat drop 2drop -1 ;
: LOCAL? ( c-addr u -- flag ) LOCAL-FIND 0>= ;
1 constant ARM                          \ a match arm's frame (codegen.fs CAP-MODE)
2 constant QUOT                         \ a quotation's
: FR ( i -- addr ) 2* cells CFR + ;
: FR-PUSH ( kind -- ) CLOCN @ swap CFRN @ FR 2!  1 CFRN +! ;
: FR-TOP ( -- kind ) CFRN @ if CFRN @ 1- FR @ else 0 then ;
: FR-POP ( -- ) CFRN @ if -1 CFRN +!  CFRN @ FR cell+ @ CLOCN ! then ;
: IN-QUOT? ( -- flag ) false  CFRN @ 0 ?do i FR @ QUOT = or loop ;
\ The frames each control word closes, then opens, and the opened frame's
\ kind, as native's handlers pop and push its control-flow stack (habu2.f
\ J-IF, J-THEN, J-ELSE 2510-2525; J-CASE, J-OF, J-ENDOF, J-ENDCASE 2527-2604;
\ J-BEGIN, J-AGAIN, J-UNTIL, J-WHILE, J-REPEAT 2609-2627; J-DO, J-?DO, J-LOOP,
\ J-+LOOP 2723-2782; J-QUOT, J-SEMIQUOT 3373-3399). Native's endof opens a
\ frame its endcase pops, saving what its arm's `of` saved, so holding none
\ between arms restores the same count at every token.
: CF-ROW ( closes opens kind "name" -- ) rot c, swap c, c, parse-name string, ;
create CF-ROWS
   0 1 0 CF-ROW if      1 0 0 CF-ROW then    1 1 0 CF-ROW else
   0 1 0 CF-ROW case    0 1 0 CF-ROW of      1 0 0 CF-ROW endof   1 0 0 CF-ROW endcase
   0 1 0 CF-ROW begin   1 0 0 CF-ROW again   1 0 0 CF-ROW until
   0 1 0 CF-ROW while   2 0 0 CF-ROW repeat
   0 1 0 CF-ROW do      0 1 0 CF-ROW ?do     1 0 0 CF-ROW loop    1 0 0 CF-ROW +loop
   0 1 QUOT CF-ROW [:   1 0 0 CF-ROW ;]      0 c, 0 c, 0 c, 0 c,
: CF-SCOPE ( c-addr u -- )             \ a compile keyword's frames
   CF-ROWS begin dup 3 + c@ while
      >r 2dup r@ 3 + count NAME= if
         2drop r>  dup c@ 0 ?do FR-POP loop  dup 2 + c@ swap 1+ c@ 0 ?do dup FR-PUSH loop
         drop exit then
      r> 3 + count +
   repeat drop 2drop ;
\ A quotation names no local and declares none: native refuses either there
\ (habu2.f C-LOCAL-REF, C-LBRACE-GUARDS), as it refuses a local in scope at
\ does> (J-DOES), with the same exit status.
75 constant LOCAL-RC
: QUOT-REF ( c-addr u -- c-addr u ) IN-QUOT? if ERR LOCAL-RC COMPILE-DIE then ;
: LOCALS-READ ( "names :}" -- n )      \ a group's names, captured and in scope
   IN-QUOT? if s" habu: local cannot be inside quotation" ERR LOCAL-RC COMPILE-DIE then
   s" {:" BCS  0 {: n :}
   begin TOKEN dup 0= if SIG-ENDED then  2dup BCS  2dup s" :}" str= 0= while
      n LOC-RECS >= if
         2drop s" hb: more than 64 locals in one definition: " ERR DREC-NAME ERR
         $4D COMPILE-DIE then
      LOC-ADD  n 1+ to n
   repeat 2drop n ;
\ A Gforth local is named %L<id>.<cell>: the checked walk's id is the
\ checker's local sequence number (codegen.fs H-LOCAL-DECL), this reader's the
\ record's body offset, so the name a body spells reaches Gforth only through
\ the records, and Gforth's own scope never decides what it names.
create LN-BUF 48 allot
: LNAME ( id c -- c-addr u )
   swap >r 0 <# #s 2drop [char] . hold r> 0 #s [char] L hold [char] % hold #>
   tuck LN-BUF swap move  LN-BUF swap ;
: HB-LOCALS ( "names :}" -- )
   LOCALS-READ {: n :}
   n 0 ?do CLOCN @ 1- i - LOC @ 0 LNAME (local) loop  0 0 (local) ;
: LOCAL-REF? ( c-addr u -- flag )      \ a local in scope, its reference compiled
   2dup LOCAL-FIND dup 0< if drop 2drop false exit then {: a u off :}
   a u QUOT-REF 2drop
   off 0 LNAME rec-local dup translate-none = if
      drop s" hb: gforth has no local for " ERR a u ERR REFUSE-RC COMPILE-DIE then
   execute true ;

\ ---- control flow and loops ----------------------------------------------
\ Branches are Gforth's, their items on Gforth's control-flow stack. A do
\ level keeps its `?do` mode cell (0 for `do`) and the base of its parked
\ leaves: each `leave` is a Gforth `ahead` whose orig waits off the
\ control-flow stack until its closer resolves it at the frame's pop point.
: ORPHAN ( c-addr u -- )             \ habu2.f LORPHAN
   s" hb: control-flow closer without opener: " ERR ERR RC-REJECT COMPILE-DIE ;
64 constant LEVEL-CAP
create LEVELS LEVEL-CAP 2* cells allot   variable LEVEL-N
256 constant LEAVE-CAP
create LEAVES LEAVE-CAP cells allot   variable LEAVE-SP
: LEVEL ( -- addr ) LEVEL-N @ 1- 2* cells LEVELS + ;
: CF-DEEP ( -- ) s" hb: control-flow nesting too deep: " ERR TOKEN$ ERR RC-REJECT COMPILE-DIE ;
: OPEN-LEVEL ( mode-addr -- )
   LEVEL-N @ LEVEL-CAP >= if CF-DEEP then
   1 LEVEL-N +!  LEVEL !  LEAVE-SP @ LEVEL cell+ ! ;
: PARK ( orig -- )
   LEAVE-SP @ cs-item-size + LEAVE-CAP > if CF-DEEP then
   cs-item-size 0 ?do LEAVES LEAVE-SP @ cells + !  1 LEAVE-SP +! loop ;
: UNPARK ( -- orig ) cs-item-size 0 ?do -1 LEAVE-SP +!  LEAVES LEAVE-SP @ cells + @ loop ;
\ A begin no path reaches assumes on Gforth the locals UNREACHABLE left
\ (kernel/cond.fs backedge-locals, glocals.fs (begin-like)), often none;
\ native's begin keeps the scope it is in, so each assumes the locals visible
\ where it stands, as ASSUME-LIVE assumes an orig's.
: KC-BEGIN ( -- ) locals-list @ backedge-locals !  postpone begin ;
: KC-DO ( -- ) postpone RT-DO  0 OPEN-LEVEL  KC-BEGIN ;
: KC-?DO ( -- )
   1 cells allocate throw dup 0 swap !
   dup postpone literal postpone RT-?DO  OPEN-LEVEL  postpone if PARK  KC-BEGIN ;
: CLOSE-LEVEL ( -- )
   begin LEAVE-SP @ LEVEL cell+ @ > while UNPARK postpone then repeat
   postpone RT-UNLOOP  -1 LEVEL-N +! ;
: KC-LOOP ( -- )
   LEVEL-N @ 0= if s" loop" ORPHAN then
   postpone RT-LOOP postpone until CLOSE-LEVEL ;
: KC-+LOOP ( -- )
   LEVEL-N @ 0= if s" +loop" ORPHAN then
   LEVEL @ ?dup if 1 swap ! then
   postpone RT-+LOOP postpone until CLOSE-LEVEL ;
: KC-LEAVE ( -- ) LEVEL-N @ 0= if s" leave" ORPHAN then postpone ahead PARK ;
: KC-IF postpone if ;          : KC-THEN postpone then ;      : KC-ELSE postpone else ;
: KC-UNTIL postpone until ;    : KC-AGAIN postpone again ;
: KC-WHILE postpone while ;    : KC-REPEAT postpone repeat ;
: KC-CASE postpone case ;      : KC-OF postpone of ;          : KC-ENDOF postpone endof ;
: KC-ENDCASE postpone endcase ;
\ `;]` is Gforth's with the check its `;` lacks: closing the definition
\ itself is a `;]` with no quotation open.
: KC-[: postpone [: ;
: KC-;] ( quotation-sys colon-sys -- ) CLOSE-SYS if CS-MISMATCH throw then swap execute ;
: KC-EXIT postpone exit ;      : KC-RECURSE postpone recurse ;
: KC-I postpone RT-I ;          : KC-J postpone RT-J ;          : KC-UNLOOP postpone RT-UNLOOP ;
: KC->R postpone RT->R ;        : KC-R> postpone RT-R> ;        : KC-R@ postpone RT-R@ ;

\ ---- does> (habu2.f J-DOES, C-PARSE-CREATED-SIG) --------------------------
\ The keyword is captured; the created word's signature stays out of the
\ body, published when the defining word runs (C-EMIT-CRSIG-SET and the
\ does-patch runtime's LASTC-TRUST:PUBLISH). The clause starts with Gforth's
\ does>, then the fetch of the Habu body its created word holds. A local in
\ scope at the keyword is refused there, the token named, before anything else,
\ then a structure left open, as J-DOES orders them (habu2.f:4515-4520 LOCF,
\ C-CF-NONE). A second does> is the host's own refusal: native has no gate for
\ it at the token.
: CRSIG-PUBLISH ( a u -- ) 2dup CRSIG-U-CELL D! CRSIG-A-CELL D!  RAW-PUBLISH  REC-WIDE ;
: DOES-TAKE ( c-addr u -- )          \ the keyword captured, its created signature kept
   CLOCN @ if ERR LOCAL-RC COMPILE-DIE then
   CFRN @ if s" hb: control-flow word does not match the open structure: " ERR ERR RC-REJECT COMPILE-DIE then
   DOESB-CELL D@ if 2drop DIE-DOES then
   BCS
   SIG-PEEK
   2dup + 1+ INP-CELL D!
   save-mem TCSIG-U-CELL D! TCSIG-A-CELL D!
   BODYLEN-CELL D@ DOESB-CELL D! ;
: DOES-COMPILE ( -- )
   TCSIG-A-CELL D@ TCSIG-U-CELL D@ postpone 2literal postpone CRSIG-PUBLISH
   postpone does> postpone @ ;
: DO-DOES ( c-addr u -- ) DOES-TAKE DOES-COMPILE ;

\ ---- keyword rows ----------------------------------------------------------
\ Each row is a spelling folded A-Z and the host word that serves it. KW-I:
\ habu2.f EM-INTERPRET-DEFINE- and -STRING-KEYWORDS (10083-10111). KW-C:
\ EM-COMPILE-CONTROL-, -STRING-, -META- and LOOP-EMIT:EM-COMPILE-LOOP-KEYWORDS
\ (11288-11339). `\`, `(`, `:`, `;`, does> and {: are read before the rows.
\ A row this host does not serve exits 76 naming it, as REFUSE-BODY does.
variable KW-I   variable KW-C
\ KW$ takes the spelling as a string: the string rows below spell their openers
\ as escaped strings, so lib/source-lex.f, which tools/error-code-lint.f runs
\ over .fs sources, reads no string body in them.
: KW$ ( head xt c-addr u -- ) {: head xt a u :}
   align here  head @ , xt ,  u ,  a here u move  u allot  head ! ;
: KW ( head xt "row" -- ) parse-name KW$ ;
: KW-FIND ( c-addr u head -- xt|0 )
   @ begin dup while
      >r 2dup r@ 3 cells + r@ 2 cells + @ NAME= if 2drop r> cell+ @ exit then
      r> @
   repeat nip nip ;
: REFUSAL ( body -- ) s" hb: gforth has no " ERR 2@ ERR ERR-NL REFUSE-RC (bye) ;
: REFUSAL-XT ( c-addr u -- xt ) save-mem noname create 2, latestxt does> REFUSAL ;
: KW-REFUSE ( head "row" -- ) parse-name 2dup REFUSAL-XT -rot KW$ ;

\ ---- the token loop -------------------------------------------------------
\ A definition reads its own tokens up to `;` (BODY-LOOP); each is captured
\ before it runs, and what a parsing word then read on the same line follows
\ it, verbatim after a string word, token by token otherwise.
: STRING-WORD? ( c-addr u -- flag )
   2dup s\" s\"" NAME= >r 2dup s\" c\"" NAME= >r 2dup s\" .\"" NAME= >r
   2dup s\" s\\\"" NAME= >r 2dup s\" c\\\"" NAME= >r s\" .\\\"" NAME=
   r> or r> or r> or r> or r> or ;
: COMPILE-REC ( rec -- )
   dup >FLAGS @ DNAME-IMM and if @ execute exit then
   dup >FLAGS @ DKIND-CAST and DKIND-CAST = if drop exit then
   @ compile, ;
: RUN-COMPILE ( c-addr u -- )
   2dup LOCAL-REF? if 2drop exit then
   2dup KW-C KW-FIND ?dup if >r CF-SCOPE r> execute exit then
   2dup NUM-PARSE if drop nip nip postpone literal exit then 2drop
   2dup LOOKUP-REC ?dup if nip nip COMPILE-REC exit then
   UNDEF-DIE ;
: CAPTURE-TAIL ( tok-end str? -- )
   >r INP-CELL D@ over - 0 max
   2dup 10 scan nip if 2drop r> drop exit then
   r> if 1 /string 0 max BCS else BCS-TOKENS then ;
: COMPILE-TOKEN ( c-addr u -- )
   2dup s" does>" NAME= if DO-DOES exit then
   2dup s" {:" str= if 2drop HB-LOCALS exit then
   2dup BCS  2dup + >r  2dup STRING-WORD? >r  RUN-COMPILE  r> r> swap CAPTURE-TAIL ;
: NEXT-TOKEN ( -- c-addr u ) TOKEN dup 0= if SIG-ENDED then ;
: BODY-LOOP ( -- )
   begin NEXT-TOKEN 2dup s" ;" str= 0= while
      2dup COMMENT if 2drop else COMPILE-TOKEN then
   repeat 2drop ;
: DEF-OPEN ( trusted -- xt colon-sys )   \ C-COLON, C-TRUSTED
   CLEAR-DEF  0 LEVEL-N !  0 LEAVE-SP !  0 CLOCN !  0 CFRN !
   s" :" DEF-NAME  over PENDTKA-CELL D!  2dup BODY-SEED  QUALIFY {: t a u wid :}
   0 a u wid REC-PEND PEND-CELL D!  t TRUSTED-CELL D!
   :noname  latestxt DEF-XT !
   t SIG-OPEN 0= if 0 0 then
   TSIG-U-CELL D! TSIG-A-CELL D! ;
require ./codegen.fs
: HB-COLON ( "name" -- )
   HOOK-CELL D@ if CG-COLON exit then  0 DEF-OPEN BODY-LOOP SEMI ;
: HB-TRUSTED ( "name" -- ) 1 DEF-OPEN BODY-LOOP SEMI ;
: HB-CAST ( "name sig" -- )          \ habu2.f C-IDENTITY, C-CAST
   CLEAR-DEF
   s" cast:" DEF-NAME 2dup BODY-SEED QUALIFY {: a u wid :}
   ['] noop a u wid REC-PEND dup PEND-CELL D!  DKIND-CAST swap REC-FLAG+
   SIG-REQUIRE TSIG-U-CELL D! TSIG-A-CELL D!
   TSIG CAST-OFF SIG-REGISTER  REC-PUBLISH REC-WIDE CLEAR-DEF ;
: RUN-WORD ( rec -- )                \ habu2.f EM-INTERPRET-FIND: the walls, the hook, the call
   DNAME-WIDE DNAME-INT or REC-GATE
   >r  depth FLOOR-DEPTH @ - r@ MIN-IN < if UNDERDEPTH then
   TOP-EV-WORD r@ WORD-FLAGS TOP-HOOK  r> @ execute ;
: INTERPRET-TOKEN ( c-addr u -- )
   2dup s" :" str= if 2drop HB-COLON exit then
   2dup KW-I KW-FIND ?dup if nip nip execute exit then
   2dup NUM-PARSE if drop nip nip TOP-EV-NUM 0 TOP-HOOK exit then 2drop
   2dup LOOKUP-REC ?dup if nip nip RUN-WORD exit then
   UNDEF-DIE ;
\ The state is native's: a definition is open while PEND-CELL holds it.
: EVAL-TOKEN ( c-addr u -- )
   2dup COMMENT if 2drop exit then
   PEND-CELL D@ if COMPILE-TOKEN exit then
   INTERPRET-TOKEN  depth FLOOR-DEPTH @ < if UNDERDEPTH then ;
: EVAL-LOOP ( -- ) begin TOKEN dup while EVAL-TOKEN repeat 2drop ;
\ A whole source file is one buffer, kept for the run: signatures and
\ captures point into it. The source-location cells stay 0, as in native's
\ boot prefix (habu2.f:12021-12023): the program's path is src/core/include.f's
\ (PUBLISH-LOCATION, include.f:1424-1433), so until the Habu loop reads the
\ program, a refusal in it ends with no ` at <path>:<line>` on this host, and
\ check-hook.f's AT-SOURCE, gated on the path length (check-hook.f:87-88),
\ prints none either.
\ A file that does not open names its path and exits 74 with no exit hook
\ (habu2.f:906-915 EMIT-SOURCE-READ).
\ Each text's floor is the depth it began at.
74 constant SRC-OPEN-RC
: LOAD-FILE ( c-addr u -- )
   2dup r/o bin open-file if drop s" hb: cannot open " ERR SRC-OPEN-RC RC-DIE then
   nip nip  dup slurp-fid  rot close-file throw
   over INP-CELL D!  + INE-CELL D!
   depth FLOOR-DEPTH !  EVAL-LOOP ;
\ A text read as LOAD-FILE reads a file, inside whatever read is under way:
\ the cursor and the floor come back after it, and a throw leaves through it
\ unchanged.
: EVAL-TEXT ( c-addr u -- )
   INP-CELL D@ INE-CELL D@ 2>r  FLOOR-DEPTH @ >r
   over + INE-CELL D! INP-CELL D!  depth FLOOR-DEPTH !
   ['] EVAL-LOOP catch
   r> FLOOR-DEPTH !  2r> INE-CELL D! INP-CELL D!  throw ;

\ ---- the rows ---------------------------------------------------------------
KW-I ' HB-PACKAGE KW package           KW-I ' HB-PUBLIC KW public
KW-I ' HB-PRIVATE KW private           KW-I ' HB-END-PACKAGE KW ;package
KW-I ' HB-USING KW using               KW-I ' HB-END-USING KW ;using
KW-I ' HB-EXPORT KW export             KW-I ' HB-TRUSTED KW trusted:
KW-I ' HB-CAST KW cast:                KW-I KW-REFUSE linear:
KW-I ' HB-DEFER KW defer               KW-I ' I-CREATE KW create
KW-I ' I-VARIABLE KW variable          KW-I ' I-CONSTANT KW constant
KW-I ' HB-ITICK KW '                   KW-I ' HB-ICHAR KW char
KW-I ' HB-IMMEDIATE KW immediate       KW-I ' HB-ISQ s\" s\"" KW$
KW-I ' HB-ICQ s\" c\"" KW$             KW-I ' HB-IDOTQ s\" .\"" KW$
KW-I ' HB-IESQ s\" s\\\"" KW$          KW-I ' HB-IECQ s\" c\\\"" KW$
KW-I ' HB-IEDOTQ s\" .\\\"" KW$
KW-C ' KC-IF KW if                     KW-C ' KC-THEN KW then
KW-C ' KC-ELSE KW else                 KW-C ' KC-BEGIN KW begin
KW-C ' KC-UNTIL KW until               KW-C ' KC-AGAIN KW again
KW-C ' KC-WHILE KW while               KW-C ' KC-REPEAT KW repeat
KW-C ' KC-CASE KW case                 KW-C ' KC-OF KW of
KW-C ' KC-ENDOF KW endof               KW-C ' KC-ENDCASE KW endcase
KW-C KW-REFUSE construct               KW-C KW-REFUSE match
KW-C ' KC-SQ s\" s\"" KW$              KW-C ' KC-CQ s\" c\"" KW$
KW-C ' KC-DOTQ s\" .\"" KW$            KW-C ' KC-ESQ s\" s\\\"" KW$
KW-C ' KC-ECQ s\" c\\\"" KW$           KW-C ' KC-EDOTQ s\" .\\\"" KW$
KW-C ' HB-BTICK KW [']                 KW-C ' KC-BCHAR KW [char]
KW-C ' KC-[: KW [:                     KW-C ' HB-IS KW is
KW-C ' KC-;] KW ;]                     KW-C ' KC-DO KW do
KW-C ' KC-LOOP KW loop                 KW-C ' KC-I KW i
KW-C ' KC->R KW >r                     KW-C ' KC-R> KW r>
KW-C ' KC-R@ KW r@                     KW-C ' KC-EXIT KW exit
KW-C ' KC-RECURSE KW recurse           KW-C ' KC-?DO KW ?do
KW-C ' KC-+LOOP KW +loop               KW-C ' KC-J KW j
KW-C ' KC-LEAVE KW leave               KW-C ' KC-UNLOOP KW unloop
