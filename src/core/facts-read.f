\ The link-time facts reader, loaded from source while it is developed; it moves
\ to checker.f's end, where REC-SYM and PIMM-ADD factor out of NOMINAL-REC-SYM
\ and PARSE-IMM.
package CHECK

\ Per wordlist id, one plus the index of the newest namespace record whose public
\ or private wordlist it is, 0 for none, over the records below NS-SEEN. A lookup
\ first indexes the records made since; it indexes from 0 again when the count
\ went down, or when the answer is missing or no longer holds, as after a package
\ gains its private wordlist. The cells rest in a scratch span: SR-RETIRE empties it.
CHECKER-REG:SCRATCH{
PTR-VARIABLE NS-ROWS-P   variable NS-CAP   variable NS-SEEN
CHECKER-REG:}SCRATCH

: NS-ENSURE ( n -- ) {: need:n :}
   need NS-CAP @ <= IF EXIT THEN
   need NS-CAP @ 2 * max 64 max {: nc:n :}
   NS-ROWS-P @ NS-CAP @ cells nc cells SR-GROW NS-ROWS-P !
   nc NS-CAP ! ;

: NS-PUT ( n n -- ) {: wid:n i:n :}
   wid 0 <= IF EXIT THEN
   wid 1 + NS-ENSURE
   i 1 +  NS-ROWS-P @ wid cells + ! ;

: NS-NOTE ( n -- ) {: i:n :}
   i CHECKER-RESOLVE:REC-AT {: ns:ptr :}
   ns CK-REC-WID BOOT:NAMESPACE-WL <> IF EXIT THEN
   ns @ i NS-PUT  ns CELL + @ i NS-PUT ;

: NS-CLEAR ( -- ) NS-ROWS-P @ 0 NS-CAP @ ARENA-CELLS-ZERO  0 NS-SEEN ! ;

: NS-INDEX ( -- )
   ndict@ NS-SEEN @ < IF NS-CLEAR THEN
   ndict@ NS-SEEN @ ?DO i NS-NOTE LOOP
   ndict@ NS-SEEN ! ;

: NS-ROW@ ( n -- n ) {: wid:n :}
   wid 0 >  wid NS-CAP @ <  and IF NS-ROWS-P @ wid cells + @ ELSE 0 THEN ;

: NS-HOLDS? ( n n -- bool ) {: wid:n row:n :}
   row 0= IF RES-FALSE EXIT THEN
   row 1 - CHECKER-RESOLVE:REC-AT {: ns:ptr :}
   ns CK-REC-WID BOOT:NAMESPACE-WL =  wid ns @ =  wid ns CELL + @ =  or  and ;

\ One plus the index of the newest namespace record holding wid, 0 for none.
: NS-FIND ( n -- n ) {: wid:n :}
   NS-INDEX
   wid  wid NS-ROW@  NS-HOLDS? 0= IF NS-CLEAR NS-INDEX THEN
   wid NS-ROW@ {: row:n :}
   wid row NS-HOLDS? IF row ELSE 0 THEN ;

\ The checker symbol of record rec, interned: a global's, else the package
\ symbol of the newest namespace whose public or private wordlist holds it; 0
\ when none does.
: REC-SYM ( ptr n -- n ) {: rec:ptr :}
   rec CHECKER-RESOLVE:CK-REC-NAME$ {: name:ptr nameu:n :}
   rec CK-REC-WID {: wid:n :}
   nameu 0= wid -2 = or IF 0 EXIT THEN
   wid BOOT:GLOBAL-WL = IF name nameu CHECKER-GLOBAL-SYM EXIT THEN
   wid NS-FIND {: row:n :}
   row 0= IF 0 EXIT THEN
   row 1 - CHECKER-RESOLVE:REC-AT {: ns:ptr :}
   ns CHECKER-RESOLVE:CK-REC-NAME$
   wid ns @ = IF SYM-PUBLIC ELSE SYM-PRIVATE THEN
   name nameu CHECKER-PKG-SYM ;

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

\ EXPORT-META-COPY for symbols, which a link has where a compile has a spelling.
: EXPORT-META-SYM ( n n -- ) {: sym:n src:n :}
   src DFER-FIND-SYM IF sym RES-TRUE DFER-ADD-SYM THEN
   src CTL-FLAGS-SYM CTL-IDENTITY invert and {: ctl:n :}
   src CTL-MASKS-SYM {: dm:n rm:n :}
   src NORET-CREATES@ {: creates:n :}
   sym ctl dm rm creates src CHECKER-SYM-SOURCE NORET-APPEND ;

\ An export's facts are its source's, as CHECKER-EXPORT-ROW records them.
: ALIAS-BODY ( n n -- )
   {: ix:n src:n :}
   ix CHECKER-RESOLVE:REC-AT REC-SYM {: sym:n :}
   src CHECKER-RESOLVE:REC-AT REC-SYM {: from:n :}
   sym 0=  from 0= or IF E-EXPORT-UNDEFINED throw THEN
   from USIG-NEWEST {: eff:n :}
   eff 0= IF E-EXPORT-UNDEFINED throw THEN
   NEW
   eff 1 - E-PTR EXPORT-EFF-INST {: din:n dout:n rin:n rout:n hasr:bool :}
   sym CHECKER-REC-SYM !
   din dout rin rout hasr from EFFECT-EXTERNAL-SYM? E-ADD-EFFECT
   0 DL-NEW !  0 RECW !  0 RECMI !
   from CHECKER-ASIG-EXPORT
   sym from EXPORT-META-SYM
   ix sym USIG-NEWEST sym NORET-NEWEST sym DFER-FIND-SYM RECORD-LINK-SET
   ix RECORD-LINK-CURRENT? IF ix ix CHECKER-RESOLVE:REC-AT RECORD-LINK-FIXED! THEN ;

public
\ The index of the newest package record whose public or private wordlist is wid,
\ and true; false when none is.
: WORDLIST-PACKAGE ( n -- n bool )
   NS-FIND {: row:n :}
   row 0= IF 0 RES-FALSE EXIT THEN
   row 1 - RES-TRUE ;

\ What a compile of the file that made record ix taught the checker, read back
\ from the record's facts text a u, so a later file's compile can call it.
: READ-FACTS ( ptr u8 n n -- )
   {: a:ptr u:n ix:n :}
   u 0= IF EXIT THEN
   CHECKER-REC-SYM @ READ-WAS !
   a u ix [: READ-BODY ;] [: READ-CLEAN ;] finally ;

\ Export record ix of record src, as a compile of its file recorded it; src's
\ facts are read first.
: READ-ALIAS ( n n -- )
   CHECKER-REC-SYM @ READ-WAS !
   [: ALIAS-BODY ;] [: READ-WAS @ CHECKER-REC-SYM ! ;] finally ;
;package
