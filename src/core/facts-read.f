\ The link-time facts reader, loaded from source while it is developed; it moves
\ to checker.f's end, where REC-SYM and PIMM-ADD factor out of NOMINAL-REC-SYM
\ and PARSE-IMM.
package CHECK

\ The checker symbol of record rec, interned: a global's, else the package
\ symbol of the namespace whose public or private wordlist holds it, newest
\ first; 0 when none does.
: REC-SYM ( ptr n -- n ) {: rec:ptr :}
   rec CHECKER-RESOLVE:CK-REC-NAME$ {: name:ptr nameu:n :}
   rec CK-REC-WID {: wid:n :}
   nameu 0= wid -2 = or IF 0 EXIT THEN
   wid BOOT:GLOBAL-WL = IF name nameu CHECKER-GLOBAL-SYM EXIT THEN
   ndict@ 0 ?DO
      ndict@ 1- i - CHECKER-RESOLVE:REC-AT {: ns:ptr :}
      ns CK-REC-WID BOOT:NAMESPACE-WL = IF
         wid ns @ = IF
            ns CHECKER-RESOLVE:CK-REC-NAME$ SYM-PUBLIC name nameu CHECKER-PKG-SYM
            UNLOOP EXIT
         THEN
         wid ns CELL + @ = IF
            ns CHECKER-RESOLVE:CK-REC-NAME$ SYM-PRIVATE name nameu CHECKER-PKG-SYM
            UNLOOP EXIT
         THEN
      THEN
   LOOP 0 ;

: PIMM-ADD ( n n -- ) {: sym:n cnt:n :}
   PIMM-N @ PIMM-CAP < 0= IF s" checker: parse-imm table full" 76 die THEN
   sym PIMM-N @ cells PIMM-SYMS + !
   cnt PIMM-N @ cells PIMM-NS + !
   PIMM-N @ 1 + PIMM-N ! ;

variable READ-WAS   \ CHECKER-REC-SYM while a read runs

\ Facts text a u parsed; false when the parse refuses it, names a type no
\ package declares here, carries a primitive's flag, or no record holds it.
: READ-ROWS ( ptr u8 n -- n n n n bool )
   {: a:ptr u:n :}
   NEW SGBAD-CLEAR
   a u PARSE-SIG-RAW {: din:n dout:n rin:n rout:n :}
   SGBAD @ 0 <>  SIG-UNRES @ 0 <> or  SIG-PE @ 0 <> or IF
      din dout rin rout RES-FALSE EXIT
   THEN
   a u din dout rin rout SGBAD-UNFIT!
   din dout rin rout SGBAD @ 0= ;

\ Refuses text a u stored for name na nu, as a bad stored signature.
: READ-REFUSE ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n na:ptr nu:n :}
   SGBAD @ 0= IF a u SGBAD-SYNTAX! THEN
   a u na nu na nu USIG-ADD-BAD ;

\ The created words' effect, offset+1, under symbol 0 as render.f FACTS-BUILD
\ builds one; 0 for none. False when its text is refused.
: READ-MADE ( ptr u8 n -- n bool )
   {: ca:ptr cu:n :}
   cu 0= IF 0 RES-TRUE EXIT THEN
   ca cu READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   ok 0= IF 0 RES-FALSE EXIT THEN
   0 CHECKER-REC-SYM !
   din dout rin rout SGHASR @ E-BUILD-EFFECT 1 + RES-TRUE ;

\ A text's control bits beside the ones storing an effect derives from its rows.
: READ-FLAGS ( n n -- n )
   {: sym:n ctl:n :}
   sym CTL-FLAGS-SYM  CTL-BARRIER EFFECT-EXTERNAL or  and
   ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER invert and  or ;

: READ-BODY ( ptr u8 n n -- )
   {: a:ptr u:n ix:n :}
   ix CHECKER-RESOLVE:REC-AT {: rec:ptr :}
   rec CHECKER-RESOLVE:CK-REC-NAME$ {: name:ptr nameu:n :}
   rec REC-SYM {: sym:n :}
   sym 0= IF a u name nameu READ-REFUSE EXIT THEN
   sym SYM-PKG$ SIGSCOPE!
   RES-TRUE SIG-FACTS!
   a u READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   ok 0= IF a u name nameu READ-REFUSE EXIT THEN
   SIG-CTL @  SIG-PIMM @  SIG-CREATES-A @  SIG-CREATES-U @  SGHASR @
      {: ctl:n pimm:n ca:ptr cu:n hasr:bool :}
   sym CHECKER-REC-SYM !
   din dout rin rout hasr RES-TRUE E-ADD-EFFECT
   \ The latches name the next record the engine publishes; this one is made.
   0 DL-NEW !  0 RECW !  0 RECMI !
   ca cu READ-MADE {: made:n good:bool :}
   good 0= IF a u name nameu READ-REFUSE EXIT THEN
   sym sym ctl READ-FLAGS ctl XFER-DMASK ctl XFER-RMASK made NORET-ADD-CREATES
   ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> IF sym RES-TRUE DFER-ADD-SYM THEN
   pimm 0 >=  sym PIMM-IX 0 <  and IF sym pimm PIMM-ADD THEN
   ix sym USIG-NEWEST sym NORET-NEWEST sym DFER-FIND-SYM RECORD-LINK-SET
   ix RECORD-LINK-CURRENT? IF ix rec RECORD-LINK-FIXED! THEN ;

: READ-CLEAN ( -- )
   RES-FALSE SIG-FACTS!  SIGSCOPE-OFF  READ-WAS @ CHECKER-REC-SYM ! ;

public
\ What a compile of the file that made record ix taught the checker, read back
\ from the record's facts text a u, so a later file's compile can call it.
: READ-FACTS ( ptr u8 n n -- )
   {: a:ptr u:n ix:n :}
   u 0= IF EXIT THEN
   CHECKER-REC-SYM @ READ-WAS !
   a u ix [: READ-BODY ;] [: READ-CLEAN ;] finally ;
;package
