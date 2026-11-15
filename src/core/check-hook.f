\ check-hook.f - the checks the compiler calls: CHECK:DEFINITION on each
\ definition's source, CHECK:IMMEDIATE before an immediate word inside one, and
\ the facts text each record takes.

package CHECK

70 constant CHECK-RC

private

\ ---- a duplicate only the checker sees ----------------------------------------
\ A replay entry (CHECKER-DEFPRODUCT, STRUCTURE-DECL:SD-REPLAY, ...) publishes a
\ checked effect without defining the word, so a later definition passes the
\ engine's duplicate wall. The hook asks
\ CHECKER-CERT-DUP? (which interns nothing) first and rejects it as the
\ wall does (REJECT-DUPLICATE), before its body is checked.

BODY-TEXT:BASE-CELL RESERVED-PTR-U8-CELL BODY-BASE
SRCLOC:INB-CELL RESERVED-PTR-U8-CELL INPUT-START
INE-CELL RESERVED-PTR-U8-CELL INPUT-END

\ Arms the checker's source map (REJECTION:SOURCE!, REJECTION:MAP!) for the
\ check of text a u, from the rows the engine's capture records (layout.f
\ BODY-SOURCE), and answers whether it did. It arms only for the pending
\ definition's text from the body text's base, its name in the buffer being
\ read, that buffer a file's (SRCLOC:FILE-BUFFER?), with no source armed. The
\ source starts at the name, placed in the file by the checker.
: ROWS-ARM ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   REJECTION:SOURCE$ nip 0 <> if RES-FALSE exit then
   SRCLOC:FILE-BUFFER? 0= if RES-FALSE exit then
   data-base PEND-CELL + @ 0= if RES-FALSE exit then
   BODY-BASE @ {: base:ptr :}
   base NULL-PTR = if RES-FALSE exit then
   a base - 0 <> if RES-FALSE exit then
   data-base BODY-TEXT:ROWS-CELL + @ {: rows:n :}
   rows 0 <= if RES-FALSE exit then
   SRC-NAME @ INPUT-START @ INPUT-END @ {: nm:ptr b:ptr e:ptr :}
   nm b - 0 <  e nm - 0 <= or if RES-FALSE exit then
   nm  e nm -  nm FILE-POS  REJECTION:SOURCE!
   a u  base data-base BODY-TEXT:CAP-CELL + @ + CELL-VIEW  rows  REJECTION:MAP!
   nm b - REJECTION:MAP-BIAS!
   RES-TRUE ;

public

\ Gives the checker the token that closes the text a u, which the text does
\ not hold (REJECTION:CLOSER!), so a structure still open there is rejected at
\ it as the load rejects it. Only text of the pending definition's body text is
\ closed: text that ends where the body text ends closes at the `;` the engine
\ read last; text that ends sooner is a definer's head, closed by the `does>`
\ just past it, placed in the armed source by the rows, else in the body text.
: CLOSER-ARM ( ptr u8 n -- )
   {: a:ptr u:n :}
   REJECTION:CLOSER-OFF
   data-base PEND-CELL + @ 0= if exit then
   BODY-BASE @ {: base:ptr :}
   base NULL-PTR = if exit then
   a base - {: x:n :}
   data-base BODYLEN-CELL + @ {: len:n :}
   x 0 <  x u + len > or if exit then
   x u + len = if SRCLOC:TOKEN$ a REJECTION:CLOSER! exit then
   REJECTION:SOURCE$ nip 0= if a u + 5 a REJECTION:CLOSER! exit then
   a u + REJECTION:>SOURCE 0= if drop exit then {: off:n :}
   off 0 <  off 5 + REJECTION:SOURCE$ nip > or if exit then
   REJECTION:SOURCE$ drop off + 5 a REJECTION:CLOSER! ;

\ ROWS-ARM, then CLOSER-ARM whatever ROWS-ARM answered.
: BODY-ARM ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u ROWS-ARM  a u CLOSER-ARM ;

\ The cleanup that takes back what BODY-ARM or ROWS-ARM armed, and the closer.
: DISARMER ( bool -- [ -- ] )
   if [: REJECTION:SOURCE-OFF ;] else [: REJECTION:CLOSER-OFF ;] then ;

\ An immediate word met inside a definition, before it runs: one the checker
\ has no model for is refused unless it parses nothing.
: IMMEDIATE ( ptr u8 n ptr u8 n bool -- )
   drop
   {: ba:ptr bu:n ta:ptr tu:n :}
   ta tu NEUTRAL-PARSE-IMM? if exit then
   ba bu ta tu [: CHECKER-PREFLIGHT:CHECK-TARGET! ;]  ba bu ROWS-ARM DISARMER
   finally 0 <> if
      s" checker: preflight did not reject unmodeled immediate" 76 die
   then
   CHECK-RC throw ;

private

\ The name as written, which is what the wall prints: the first token of the
\ source the engine hands the hook.
: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1 + repeat
   a swap ;


\ Appends sym's tags at REJECTION:TEXT-LEN, after the effect FACTS-SAME? left
\ in REJECTION:TEXT. A kept `parses:` row's tag carries CTL-PARSES, so the
\ flag's own tag is left out.
: FACTS-TAGS ( n -- )
   {: sym:n :}
   sym PROW-FIND {: cnt:n t:ptr tu:n kept:bool :}
   sym CTL-WORD-SYM  sym DFER-FIND-SYM if CHECKER-OWNER-ABI:BINDING-DEFER or then
   kept if CTL-PARSES invert and then
   REND-CTL 2drop
   sym PRIM-FIRST-IDX {: prim:n :}
   prim 0 <> if prim 1 - PE-FLAGS@ else 0 then REND-PE 2drop
   sym PIMM-IX {: ix:n :}
   ix 0 < if -1 else ix PIMM-CNT@ then REND-PIMM 2drop
   kept if cnt else -1 then t tu REND-PARSES 2drop
   sym NORET-CREATES@ REND-CREATES 2drop ;


\ Ends the process with the sentence REJECTION:TEXT holds from byte t on: a
\ facts text the checker cannot keep is an engine defect.
: TEXT-DIE ( n -- ) {: t:n :}
   REJECTION:TEXT t +  REJECTION:TEXT-LEN @ t -  76 die ;

\ Dies naming na nu, a u saying what is wrong with the facts text written for
\ it, which REJECTION:TEXT holds. The sentence goes after the text in TEXT,
\ which may move as it grows, so the text is copied by offset.
: FACTS-DIE ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n a:ptr u:n :}
   REJECTION:TEXT-LEN @ {: t:n :}
   1 REJECTION:DEST !
   s" the facts text written for `" REJECTION:EMIT-STR
   na nu REJECTION:EMIT-STR  s" ` " REJECTION:EMIT-STR
   a u REJECTION:EMIT-STR  s" : " REJECTION:EMIT-STR
   t 0 ?do REJECTION:TEXT i + c@ REJECTION:EMIT1 loop
   t TEXT-DIE ;

\ Dies naming na nu: its amended effect has no record, or no facts text that
\ says it.
: AMENDED-DIE ( ptr u8 n -- )
   {: na:ptr nu:n :}
   0 REJECTION:TEXT-LEN !  1 REJECTION:DEST !
   s" no facts text says the amended effect of `" REJECTION:EMIT-STR
   na nu REJECTION:EMIT-STR  96 REJECTION:EMIT1
   0 TEXT-DIE ;

\ Only a record this check published, at or above MARK and still active, is
\ asked: a duplicate or an unrecorded effect publishes none.
: CHECK-FACTS ( ptr u8 n n -- )
   {: na:ptr nu:n mark:n :}
   na nu CHECKER-RECORD-SYM? {: sym:n :}
   sym 0= if exit then
   sym USIG-NEWEST {: rec1:n :}
   rec1 0= if exit then
   rec1 1 - mark < if exit then
   rec1 1 - E-PTR ER.ACTIVE @ EFF-ACTIVE <> if exit then
   sym FACTS-SAME? if
      CK-LIVE-PEND? 0= if exit then
      DOES-SPLIT @ 0 <> if exit then
      sym FACTS-TAGS
      RQM @ 0= if REJECTION:TEXT REJECTION:TEXT-LEN @ ndict@ FACTS! exit then
   then
   NMB NMU @
   RQM @ 0 <> if s" cannot say its effect" else s" reads back as another effect" then
   FACTS-DIE ;


\ Whether sym's newest effect has a facts text that says it, left in
\ REJECTION:TEXT.
: FACTS-TEXT? ( n -- bool )
   {: sym:n :}
   sym 0= if RES-FALSE exit then
   sym USIG-NEWEST {: rec1:n :}
   rec1 0= if RES-FALSE exit then
   rec1 1 - E-PTR ER.ACTIVE @ EFF-ACTIVE <> if RES-FALSE exit then
   sym FACTS-SAME? 0= if RES-FALSE exit then
   sym FACTS-TAGS
   RQM @ 0= ;


public

\ A does> clause's publication, once the definer's record ndict@ 1 - is
\ appended: it refuses nothing and writes facts only there, as `: PKG:X` hands
\ it the tail X.
: DOES-PUBLISHED ( ptr u8 n bool -- )
   {: a:ptr u:n committed:bool :}
   a u committed CHECKER-NATIVE-DOES-FINISH
   committed 0= if exit then
   a u CHECKER-RECORD-SYM? {: sym:n :}
   sym FACTS-TEXT? 0= if exit then
   sym VIS-SYM-REC CHECKER-RESOLVE:REC>INDEX {: ix:n :}
   ix ndict@ 1 - <> if exit then
   REJECTION:TEXT REJECTION:TEXT-LEN @ ix FACTS!
   sym NORET-CREATES@ {: made:n :}
   made 0= if exit then
   0 REJECTION:TEXT-LEN !  made 1 - REND-RECORD
   RQM @ 0 <> if 2drop exit then
   LAY-FACTS ;

private


\ The checker's registrars the fields held: a checked body cannot name them
\ (UNSAFE-TOK?), so each entry calls its token.
variable RAW-REG
variable DECL-REG
variable DEFCAST-REG
variable ROW-REG
CAST: REG-XT ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: ROW-XT ( n -- [ ptr u8 n ptr u8 n -- bool ] )


\ The raw registrar's field entry: a created word or constant takes its text
\ through FIND-FACTS!, or 0 when none says its effect.
: RAW-DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su RAW-REG @ REG-XT execute
   CHECKER-VERIFY-PKG-DEPTH @ 0 <> CHK-CAND @ 0 <> or if exit then
   data-base LASTC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec NULL-PTR = if exit then
   rec CHECKER-RESOLVE:REC>INDEX {: ix:n :}
   ix 0 < if exit then
   LASTC-LINK-SYM FACTS-TEXT? if
      REJECTION:TEXT REJECTION:TEXT-LEN @
   else NULL-PTR 0 then
   ix FIND-FACTS! ;


\ The pending record's facts once a registrar recorded the live window for this
\ name; an unsaid text stores 0, so no earlier registrar's text stays.
: PENDING-FACTS ( ptr u8 n -- )
   {: a:ptr u:n :}
   CK-LIVE-PEND? 0= if exit then
   CK-CLOSED-OPEN @ 0= if exit then
   DOES-SPLIT @ 0 <> if exit then
   CK-CLOSED-SYM @ CK-CLOSED-IX @ CK-CLOSED-OFF@ CK-PEND-SYM {: sym:n :}
   sym 0= if exit then
   a u CHECKER-RECORD-SYM? sym <> if exit then
   sym FACTS-TEXT? if REJECTION:TEXT REJECTION:TEXT-LEN @ CK-CLOSED-IX @ FACTS!
   else NULL-PTR 0 CK-CLOSED-IX @ FIND-FACTS! then ;

\ The declared-row registrar's field entry: DECLARED's, answering whether the
\ checker rejected the row.
: ROW-DECLARED ( ptr u8 n ptr u8 n -- bool )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su ROW-REG @ ROW-XT execute {: rejected:bool :}
   na nu PENDING-FACTS
   rejected ;

\ The shapes whose child answers or takes `ptr t` for its declared type t.
: STORED-SHAPE? ( n -- bool ) {: shape:n :}
   shape CHILD-ABI:STORED =  shape CHILD-ABI:BUFFER = or
   shape CHILD-ABI:DYNAMIC = or  shape CHILD-ABI:SLOT = or
   shape CHILD-ABI:SLOTS = or  shape CHILD-ABI:WORK = or
   shape CHILD-ABI:WORKS = or  shape CHILD-ABI:HEAP-NEW = or
   shape CHILD-ABI:HEAP-RELEASE = or  shape CHILD-ABI:ARENA-NEW = or
   shape CHILD-ABI:ARENA-DISPOSE = or ;

\ A refused type is reported, naming the child, before the throw. A NODE other
\ than 0 is the type, which TYPE does not spell.
: STORED-ROWS ( ptr u8 n ptr u8 n n n -- n n )
   {: na:ptr nu:n ta:ptr tu:n shape:n node:n :}
   node 0 <> if node NODE-STORAGE-TERM else ta tu CHECKER-STORAGE-TERM then
   0= if
      drop
      node 0 <> if na nu NODE-TYPE-REFUSE else na nu ta tu CHECKER-STORAGE-TYPE-REJECT then
      CHECK-RC throw
   then
   shape swap TFAM:TFAM-STORED-ROWS ;

\ Type t's bytes in storage: whole cells only when WHOLE, else as DYNAMIC
\ admits, which takes `u8` too.
: TYPE-BYTES ( ptr u8 n bool -- n bool )
   {: ta:ptr tu:n whole:bool :}
   whole 0= if ta tu CHECKER-DYNAMIC-INFO exit then
   ta tu CHECKER-STORAGE-INFO {: w:n ok:bool :}
   w CELL * ok ;

\ A storage declaration's name, stored type and that type's bytes. A missing
\ name or a refused type is reported, naming the definer, and answers 0 bytes.
: DECL-LINE ( ptr u8 n bool -- ptr u8 n ptr u8 n n )
   {: da:ptr du:n whole:bool :}
   STORAGE-LINE-TOKEN {: na:ptr nu:n :}
   nu 0= if da du CHECKER-STORAGE-NAME-REFUSE na 0 na 0 0 exit then
   STORAGE-PARSE-TYPE {: ta:ptr tu:n :}
   na nu CHECKER-LBUF-NAME-OK? 0= if na nu ta tu 0 exit then
   ta tu whole TYPE-BYTES 0= if
      drop na nu ta tu CHECKER-STORAGE-TYPE-REJECT na nu ta tu 0 exit
   then {: bytes:n :}
   na nu ta tu bytes ;

\ The declaration registrars' field entries: each runs its registrar, then lays
\ the declared name's facts.
: DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su DECL-REG @ REG-XT execute  na nu PENDING-FACTS ;

public

\ Registers the effect of the pending child word NAME of SHAPE, PARAM and
\ stored TYPE, lays its facts, and answers its body value and effect handle.
\ TYPE is read only for the shapes that answer `ptr t`, and not for a STORED
\ child whose PARAM is its type's node.
: CHILD ( ptr u8 n n n ptr u8 n -- n n )
   {: na:ptr nu:n shape:n param:n ta:ptr tu:n :}
   shape STORED-SHAPE? if
      na nu ta tu shape
      shape CHILD-ABI:STORED = if param else 0 then STORED-ROWS 0
   else shape param TFAM:TFAM-CHILD-ROWS then {: din:n dout:n val:n :}
   na nu din dout CHECKER-CHILD-ROWS {: h:n :}
   na nu PENDING-FACTS
   val h ;

\ The line of a definer whose element may be a byte.
: STORAGE-LINE ( ptr u8 n -- ptr u8 n ptr u8 n n ) RES-FALSE DECL-LINE ;

\ The line of a definer whose element is whole cells.
: CELL-LINE ( ptr u8 n -- ptr u8 n ptr u8 n n ) RES-TRUE DECL-LINE ;

: DEFINER-AT ( ptr u8 n -- )
   REJECTION:AT  s" the definer was reading here" REJECTION:EMIT-STR ;

\ Places the rejection at the token read last, the definer's own or
\ one it parsed.
: DEFINER-PLACE ( -- ) SRCLOC:TOKEN$ DEFINER-AT ;

private

using SCHEMA-REG

\ A type: a live node, never an effect side's row. A node ends with the file
\ that made it.
: TYPE-ARG ( n -- n ) {: node:n :}
   node SCHEMA-NODE-OK? if node SCHEMA-ROW? 0= if node exit then then
   s" E-NOT-TYPE" REJECTION:OPEN
   s" a definer was given a value that is no type" REJECTION:EMIT-STR
   DEFINER-PLACE
   REJECTION:HELP
   s" pass a type EFFECT:N, EFFECT:NAMED, EFFECT:READ-TYPE or EFFECT:PTR made in this file: a type ends with the file that built it"
   REJECTION:EMIT-STR
   CHECK-RC REJECTION:STOP ;

: UNKNOWN-HELP$ ( -- ptr u8 n )
   s" name n, bool, r or a family the package running the definer can name" ;

\ An effect; any other type is rejected.
: EFFECT-ARG ( n -- n ) {: node:n :}
   node TYPE-ARG SCHEMA-QUOT? if node exit then
   s" E-NOT-EFFECT" REJECTION:OPEN
   s" EFFECT:IN or EFFECT:OUT was given a type that is no effect"
   REJECTION:EMIT-STR
   DEFINER-PLACE
   REJECTION:HELP
   s" pass the effect EFFECT:EMPTY made, or one EFFECT:IN or EFFECT:OUT answered"
   REJECTION:EMIT-STR
   CHECK-RC REJECTION:STOP ;

: FAMILY-NODE ( n -- n ) {: fam:n :}
   fam TFAM:TFAM-ARITY@ {: k:n :}
   SCHEMA-ROOT-N@ {: start:n :}
   k 0 ?do FRESH-PARAM SCHEMA-ROOT+ drop loop
   fam start k SCHEMA-APP ;

\ TYPE-OP's operands are cells: a spelling arrives as its address.
CAST: >BYTES ( n -- ptr u8 )

\ A built-in type's con code as a signature reads its spelling, else 0.
: BUILT-IN ( n n -- n ) {: a:n u:n :}
   u 1 = if
      a >BYTES c@ {: c:n :}
      c 110 = if CC-N exit then
      c 102 = if CC-BOOL exit then
      c 114 = if CC-R exit then
   then
   a >BYTES u CON-OF ;

\ EFFECT:NAMED's family tail a u is public in more than one used package.
: NAMED-AMBIG ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-USING-AMBIGUOUS" REJECTION:OPEN
   s" EFFECT:NAMED found type family '" REJECTION:EMIT-STR
   a u REJECTION:EMIT-STR
   s" ' public in more than one used package" REJECTION:EMIT-STR
   DEFINER-PLACE
   a u SGBAD-AMBIG-HELP
   CHECK-RC REJECTION:STOP ;

\ The type a spelling names: a built-in type's con code and 0, else 0 and the
\ family; one that names neither is rejected.
: NAMED-FAMILY ( n n -- n n )
   {: a:n u:n :}
   a u BUILT-IN dup 0 <> if 0 exit then drop
   SGBAD-CLEAR
   TFAM:TFAM-ACTIVE-PKG$ a >BYTES u
   a >BYTES u TFAM:TFQ-SPLIT? if TFAM:TFAM-EXACT-RESOLVE
   else TFAM:TFAM-PARSE-RESOLVE then
   if 0 swap exit then
   drop
   SGBAD-AMBIG? if a >BYTES u NAMED-AMBIG then
   s" E-UNKNOWN-SIGNATURE-TYPE" REJECTION:OPEN
   s" EFFECT:NAMED found no type named '" REJECTION:EMIT-STR
   a >BYTES u REJECTION:EMIT-STR  s" '" REJECTION:EMIT-STR
   DEFINER-PLACE
   REJECTION:HELP  UNKNOWN-HELP$ REJECTION:EMIT-STR
   CHECK-RC REJECTION:STOP ;

\ A rejected READ of the type xa xu, naming the token, or the type when the
\ reason names none.
: TEXT-TYPE-REFUSE ( ptr u8 n ptr u8 n n ptr u8 n -- )
   {: ta:ptr tu:n wa:ptr wu:n code:n xa:ptr xu:n :}
   tu 0= if xa xu else ta tu then {: na:ptr nu:n :}
   code TYPE-DECL:E-TDECL-PAYLOAD = {: unknown:bool :}
   code E-USING-AMBIGUOUS = {: ambig:bool :}
   ambig if s" E-USING-AMBIGUOUS" else
   unknown if s" E-UNKNOWN-SIGNATURE-TYPE" else s" E-BAD-SIGNATURE" then then
   REJECTION:OPEN
   wa wu REJECTION:EMIT-STR  s"  '" REJECTION:EMIT-STR
   na nu REJECTION:EMIT-STR  s" '" REJECTION:EMIT-STR
   tu xu <> if
      s"  in the type '" REJECTION:EMIT-STR  xa xu REJECTION:EMIT-STR
      s" '" REJECTION:EMIT-STR
   then
   DEFINER-PLACE
   ambig if na nu SGBAD-AMBIG-HELP else
      REJECTION:HELP
      unknown if s" declare a type of that name, or correct its spelling"
      else s" write the type as a signature writes it" then
      REJECTION:EMIT-STR
   then
   CHECK-RC REJECTION:STOP ;

\ Rejects a type variable's or region's letter in a type EFFECT:READ-TYPE was
\ given, placed at the token the engine read last, which ran the definer.
: TEXT-LETTER-REFUSE ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n xa:ptr xu:n :}
   s" E-BAD-SIGNATURE" REJECTION:OPEN
   s" the type '" REJECTION:EMIT-STR
   xa xu REJECTION:EMIT-STR  s" ' has the letter '" REJECTION:EMIT-STR
   la lu REJECTION:EMIT-STR  s" ', which names no type" REJECTION:EMIT-STR
   DEFINER-PLACE
   REJECTION:HELP
   s" a built definition's types are concrete: " REJECTION:EMIT-STR
   s" write one such as n or ptr u8 in place of '" REJECTION:EMIT-STR
   la lu REJECTION:EMIT-STR  s" '" REJECTION:EMIT-STR
   CHECK-RC REJECTION:STOP ;

: TEXT-TYPE ( n n -- n ) {: a:n u:n :}
   a >BYTES u [: a >BYTES u TEXT-TYPE-REFUSE ;]
   [: a >BYTES u TEXT-LETTER-REFUSE ;] TYPE-DECL:READ-TYPE ;

: TYPE-NODE ( n n n -- n ) {: code:n a:n b:n :}
   code TYPE-OP-ABI:INT = if CC-N SCHEMA-CON exit then
   code TYPE-OP-ABI:NAMED = if
      a 0 <> if a SCHEMA-CON exit then
      b FAMILY-NODE exit
   then
   code TYPE-OP-ABI:PTR = if a TYPE-ARG SCHEMA-PTR exit then
   code TYPE-OP-ABI:EFFECT = if EMPTY-EFFECT exit then
   code TYPE-OP-ABI:IN = if a EFFECT-ARG b TYPE-ARG EFFECT-IN exit then
   code TYPE-OP-ABI:OUT = if a EFFECT-ARG b TYPE-ARG EFFECT-OUT exit then
   code TYPE-OP-ABI:READ = if a b TEXT-TYPE exit then
   s" E-TYPE-OP" REJECTION:OPEN
   s" the checker has no type operation " REJECTION:EMIT-STR
   code REJECTION:EMIT-N
   DEFINER-PLACE
   REJECTION:HELP
   s" this engine is older than the tree it loads: " REJECTION:EMIT-STR
   s\" build the engine in stages (docs/bootstrap.md \"Staged hosts\")"
   REJECTION:EMIT-STR
   CHECK-RC REJECTION:STOP ;

public

\ The transient schema node a TYPE-OP-ABI code makes of a and b, else rejected.
\ NAMED's lookup, and the type entries READ's lookups would read, come first:
\ the transient region would drop an entry a lookup reads.
: TYPE-OP ( n n n -- n )
   {: code:n a:n b:n :}
   code TYPE-OP-ABI:NAMED = if a b NAMED-FAMILY else a b then {: x:n y:n :}
   code TYPE-OP-ABI:READ = if TFAM:READ-PENDING then
   TRANSIENT-ON
   [: code x y TYPE-NODE ;] [: TRANSIENT-OFF ;] finally ;

\ Whether node is an effect a definition can take: a live quotation node with
\ no return clause.
: EFFECT? ( n -- bool ) {: node:n :}
   node SCHEMA-NODE-OK? 0= if RES-FALSE exit then
   node SCHEMA-ROW? if RES-FALSE exit then
   node SCHEMA-QUOT? 0= if RES-FALSE exit then
   node SCHEMA-QUOT-HASR@ 0= ;

private

\ EFFECT-DENSE's map: the raw index of each parameter it numbered, at that
\ parameter's dense index, and how many it met.
create DENSE-RAW TFAM:TFAM-DECL-PARAM-COUNT cells allot
variable DENSE-N

\ The dense index of raw parameter index RAW, the next one when RAW is new.
\ Past the map's entries a new parameter is counted, not kept.
: DENSE-INDEX ( n -- n ) {: raw:n :}
   DENSE-N @ TFAM:TFAM-DECL-PARAM-COUNT min 0 ?do
      i cells DENSE-RAW + @ raw = if i unloop exit then
   loop
   DENSE-N @ TFAM:TFAM-DECL-PARAM-COUNT < if
      raw DENSE-N @ cells DENSE-RAW + !
   then
   DENSE-N @  1 DENSE-N +! ;

\ The run of schema roots an application's arguments or a row's elements are.
: CHILD-START ( n -- n )
   dup SCHEMA-APP? if SCHEMA-B@ exit then SCHEMA-ROW-START@ ;
: CHILD-COUNT ( n -- n )
   dup SCHEMA-APP? if SCHEMA-C@ exit then SCHEMA-ROW-COUNT@ ;

\ Roots the copies staged on PARAM-SCR from MARK on, contiguously, unstages
\ them and answers the first root.
: DENSE-ROOTS ( n -- n ) {: mark:n :}
   SCHEMA-ROOT-N@ {: start:n :}
   PARAM-SCR-N @ mark ?do i cells PARAM-SCR + @ SCHEMA-ROOT+ drop loop
   mark PARAM-SCR-N !
   start ;

\ NODE with each parameter renumbered by DENSE-INDEX. A node's copied children
\ are staged before its own roots are pushed, so each run of roots is contiguous.
: DENSE-COPY ( n -- n ) {: node:n :}
   node SCHEMA-PARAM? if node SCHEMA-A@ DENSE-INDEX SCHEMA-PARAM exit then
   node SCHEMA-PTR? if node SCHEMA-A@ RECURSE SCHEMA-PTR exit then
   node SCHEMA-QUOT? if
      node SCHEMA-QUOT-DIN@ RECURSE  node SCHEMA-QUOT-DOUT@ RECURSE
      node SCHEMA-QUOT-RIN@ RECURSE  node SCHEMA-QUOT-ROUT@ RECURSE
      node SCHEMA-QUOT-HASR@ SCHEMA-QUOT exit
   then
   node SCHEMA-APP? node SCHEMA-ROW? or 0= if node exit then
   PARAM-SCR-N @
   0 begin dup node CHILD-COUNT < while
      dup node CHILD-START + SCHEMA-ROOT@ RECURSE PARAM-SCR+  1 +
   repeat drop
   DENSE-ROOTS {: first:n :}
   node SCHEMA-APP? if node SCHEMA-A@ first node CHILD-COUNT SCHEMA-APP exit then
   first node CHILD-COUNT SCHEMA-ROW ;

public

\ A transient copy of type node, its parameters numbered on by the map the
\ open definition's EFFECT-DENSE began; false past TFAM-DECL-PARAM-COUNT.
: TYPE-DENSE ( n -- n bool ) {: node:n :}
   TRANSIENT-ON
   [: node DENSE-COPY ;] [: TRANSIENT-OFF ;] finally
   DENSE-N @ TFAM:TFAM-DECL-PARAM-COUNT > if drop 0 RES-FALSE exit then
   RES-TRUE ;

\ A transient copy of effect node, its parameters numbered 0, 1 ... by first
\ occurrence, inputs then outputs, element 0 first; false past TFAM-DECL-PARAM-COUNT.
: EFFECT-DENSE ( n -- n bool ) 0 DENSE-N !  TYPE-DENSE ;

;using

private

\ The span through its first `)`.
: THROUGH-CLOSE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 41 <> else RES-FALSE then while 1 + repeat
   {: k:n :}
   a  k 1 + u min ;

public

\ An effect EFFECT? admits as a signature, `( n -- n )`, in the renderer's
\ buffer past its recorded text, where the next rendering replaces it.
: EFFECT-TEXT ( n -- ptr u8 n ) {: node:n :}
   node EFFECT-SIG 2drop {: din:n dout:n :}
   REJECTION:TEXT-LEN @ {: at:n :}
   din dout 0 0 RES-FALSE REND-FACTS {: buf:ptr len:n :}
   at REJECTION:TEXT-LEN !
   buf at +  len at -  THROUGH-CLOSE ;

private


: DEFERRED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u CHECKER-DEFER  a u PENDING-FACTS ;


: CAST-DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su DEFCAST-REG @ REG-XT execute  na nu PENDING-FACTS ;


: LINEAR-DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su CHECKER-LINEAR  na nu PENDING-FACTS ;


\ CHECK prints no deferred verdict, which rejects a definition the engine
\ compiles, so its record is written here while the body's source is armed.
: REPORT-DEFERRED ( n -- n )
   dup VERDICT-DEFERRED = if dup DVERD !  DIAG-PRINT then ;

public

\ The check of a definition's source: a rejected or deferred body's record is
\ written, then CHECK-RC thrown. In multi-error mode the record is counted and
\ the answer is -1, which publishes the definition (zero unpublishes) so its
\ name resolves for later definitions; the driver exits nonzero via MULTI-ERR-END.
: DEFINITION ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u FIRST-TOKEN {: na:ptr nu:n :}
   na nu CHECKER-CERT-DUP? if na nu  na nu SOURCE-NAME$  REJECT-DUPLICATE then
   UEND @ {: mark:n :}
   a u [: CHECK! REPORT-DEFERRED ;]
   a u BODY-ARM DISARMER  finally
   dup -1 = if na nu mark CHECK-FACTS then
   MULTI-ERR? if drop -1 exit then
   dup -1 <> if CHECK-RC throw then ;

\ At a piece's end (src/habu/linker.f PIECE-END), each record a `TRUST` row or an
\ amender (`INTRINSIC`, `generates:`, `names:`, NOMINAL-PROVIDER, a does>
\ publication) changed, held from entry FROM on, takes its facts text again from
\ its symbol's newest effect, as a definition's does: the record the symbol names
\ takes the text, and a held record it no longer names an empty one. A text that
\ cannot say the effect, or a symbol no record holds, fails the build: a link
\ would read the record's older facts.
: AMENDED ( n -- )
   {: from:n :}
   HELD-FACTS from ?do
      i HELD-FACT@ 2drop HELD-AMENDED = if
         i HELD-AMENDED@ {: ix:n sym:n :}
         sym VIS-SYM-REC CHECKER-RESOLVE:REC>INDEX {: now:n :}
         now ix <> if NULL-PTR 0 ix FACTS! then
         now 0 >=  sym FACTS-TEXT?  and 0= if sym SYM-NAME$ AMENDED-DIE then
         REJECTION:TEXT REJECTION:TEXT-LEN @ now FACTS!
      then
   loop ;

\ The active primitive rows of sym after row k: whether one is, and whether
\ each survives its facts text (FACTS-SAME-AT?).
: ROW-OF? ( n n -- bool ) {: sym:n k:n :} k PE-ACTIVE?  k PE-SYM@ sym =  and ;

: MORE-ROWS? ( n n -- bool )
   {: sym:n k:n :}
   #PE @ k 1 + ?do sym i ROW-OF? if RES-TRUE unloop exit then loop
   RES-FALSE ;

: ROWS-SAY? ( n n -- bool )
   {: sym:n k:n :}
   #PE @ k 1 + ?do
      sym i ROW-OF? if
         sym i PE-EFF@ FACTS-SAME-AT? 0= if RES-FALSE unloop exit then
      then
   loop
   RES-TRUE ;


\ Sym's tags alone in REJECTION:TEXT, the facts text of a primitive the checker
\ types by what it does (`execute`, `?dup`). False when it has none or a text
\ cannot say them.
: TAGS-TEXT? ( n -- bool )
   {: sym:n :}
   0 REJECTION:TEXT-LEN !  0 RQM !
   sym FACTS-TAGS
   REJECTION:TEXT-LEN @ 0 >  RQM @ 0=  and ;


\ Sym's primitive rows as one facts text in REJECTION:TEXT: the first row's
\ effect and tags, then each further row after `overload`; with no row, its tags
\ alone.
\ False when a text cannot say them, or a created effect, whose text ends the
\ facts, would hide the rows.
: ROWS-TEXT? ( n -- bool )
   {: sym:n :}
   sym PRIM-FIRST-IDX {: first:n :}
   first 0= if sym TAGS-TEXT? exit then
   first 1 - {: k:n :}
   sym k ROWS-SAY? 0= if RES-FALSE exit then
   sym k PE-EFF@ FACTS-SAME-AT? 0= if RES-FALSE exit then
   sym FACTS-TAGS
   sym k MORE-ROWS? 0= if RQM @ 0= exit then
   sym NORET-CREATES@ 0 <> if RES-FALSE exit then
   #PE @ k 1 + ?do sym i ROW-OF? if i REND-OVERLOAD 2drop then loop
   RQM @ 0= ;


\ Sym's rows as one facts text (ROWS-TEXT?), empty for sym 0 or when no text
\ can say them. The text lies in REJECTION:TEXT, which the next render reuses.
: ROWS-FACTS ( n -- ptr u8 n )
   {: sym:n :}
   sym 0= if NULL-PTR 0 exit then
   sym ROWS-TEXT? if REJECTION:TEXT REJECTION:TEXT-LEN @ else NULL-PTR 0 then ;

\ A linked engine's facts text for primitive na nu: what the open build's rows
\ say of its global symbol, empty when no text can.
: PRIMITIVE-FACTS ( ptr u8 n -- ptr u8 n )
   CHECKER-GLOBAL-SYM? ROWS-FACTS ;

\ The facts text the open build's rows give the primitive named na nu that
\ package pa pu owns (src/habu/prims.f EPPRIM:), for the record a build lays
\ for it in that package's private wordlist; empty when no text can.
: OWNED-FACTS ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n na:ptr nu:n :}
   pa pu SYM-PRIVATE na nu CHECKER-PKG-SYM? ROWS-FACTS ;

\ Only the engine reads the two cells; inside a build the builder's own checker
\ checks, so a target's never replaces it.
: INSTALL ( -- )
   BUILDING? if exit then
   ['] CHECK:IMMEDIATE set-preflight
   ['] DEFINITION set-check ;

\ Replace the checker's entries in the owner's fields, now the renderer exists.
' DOES-PUBLISHED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:NATIVE-DOES-FINISH-OFF + xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:RAW-OFF + CELL-VIEW @ RAW-REG xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:EFFECT-OFF + CELL-VIEW @ DECL-REG xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + CELL-VIEW @ DEFCAST-REG xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:DECL-ROW-OFF + CELL-VIEW @ ROW-REG xt!
' RAW-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:RAW-OFF + xt!
' DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:EFFECT-OFF + xt!
' ROW-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:DECL-ROW-OFF + xt!
' DEFERRED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:DEFER-OFF + xt!
' CAST-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + xt!
' LINEAR-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:LINEAR-OFF + xt!
INSTALL

;package
