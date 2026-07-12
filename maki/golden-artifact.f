\ maki/golden-artifact.f - external GOLDEN reference artifacts + synthetic inputs.
\
\ CAD-PLAN section 11: GOLDEN may compare a model's device/host output against an
\ EXTERNAL reference artifact - a saved tensor dump with a per-artifact tolerance -
\ instead of only the self-consistency oracle (maki/golden.f). This file owns two
\ concerns of the same "reference material" seam: (1) the deterministic synthetic
\ input binding both golden legs share (GA-BIND-SYNTH, kept here so golden.f can call
\ it without a load-order cycle back through the artifact reader), and (2) the on-disk
\ artifact FORMAT + save/load/check under the CAD store root's `golden/` subdir.
\
\ On-disk format (line-oriented, agent-parseable by splitting each line on ": "):
\
\   artifact.model: NAME
\   artifact.tolerance.atol-exp: <int>        \ atol = 10^atol-exp (f32 -6, f16/bf16 -3)
\   artifact.tolerance.rtol-exp: <int>        \ rtol = 10^rtol-exp (f32 -5, f16/bf16 -2)
\   artifact.inputs: K
\   input.<i>.shape: RxC
\   input.<i>.data:  v0 v1 v2 ...             \ row-major, space-separated decimals
\   output.shape: RxC
\   output.data:  v0 v1 ...
\
\ Tolerance is a base-10 EXPONENT integer (clean, exact, POW10-reconstructed), matching
\ the section 11 per-dtype defaults precisely. Floats render with GA-DECIMALS places
\ (1e-9, well below f32 atol) so a save->load->recompute round-trips inside tolerance.
\ The tolerance test is `|computed-expected| <= atol + rtol*|expected|` (section 11).
\
\ GA-SAVE synthesizes+binds inputs, runs the forward IR, and writes them + the executed
\ output as an artifact; GA-LOAD reads one back, binding its inputs and loading the
\ expected output + tolerance; GA-CHECK executes the forward IR on the artifact inputs
\ and compares -> V-PASS / V-FAIL / V-NOTRUN(no artifact or non-host-executable). This is
\ the LocateAnything-port seam: an external reference dump IS an artifact of this format.
\
\ Fail closed: a save on a non-host-executable model, a malformed artifact (missing key /
\ non-numeric / wrong count), an input/output shape that disagrees with the current IR,
\ and every buffer capacity are named throws. A read of an absent artifact is a legitimate
\ "no artifact" (false / V-NOTRUN), never an error. maki -> habu only; owns -5165..-5168.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require maki/array.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/executor.f
require maki/store.f
require maki/report.f

-5165 constant E-GA-CAP     \ input / expected / text buffer capacity exceeded
-5166 constant E-GA-PARSE   \ malformed artifact: missing key / non-numeric / wrong count
-5167 constant E-GA-UNSUP   \ GA-SAVE on an empty or non-host-executable model
-5168 constant E-GA-SHAPE   \ artifact input/output count or shape disagrees with the IR

package MAKI
private

64    constant GA-SCAP         \ input slots (mirrors model-ir MIR-IN-CAP)
$4000 constant GA-ARENA-CELLS   \ synthetic / loaded input-buffer arena (float cells); sized to the
                                \ launch caps (lower-launch LLA-MAX-IN * LLA-NCAP = 4 x 4096 elems)
$1000 constant GA-EXP-CELLS     \ loaded expected-output buffer (float cells)
$8000 constant GA-TEXT-CAP      \ artifact text build / read buffer (bytes)
96    constant GA-FILE-CAP      \ "<NAME>.artifact" filename buffer
9     constant GA-DECIMALS      \ float render precision (1e-9 << f32 atol 1e-6)

\ per-dtype default tolerance exponents (CAD-PLAN section 11)
-6 constant GA-F32-ATOL-EXP    -5 constant GA-F32-RTOL-EXP
-3 constant GA-LOW-ATOL-EXP    -2 constant GA-LOW-RTOL-EXP

create GA-ARENA  GA-ARENA-CELLS cells allot
create GA-IN-OFF GA-SCAP cells allot
variable GA-BUMP
create GA-EXP    GA-EXP-CELLS cells allot   variable GA-EXP-N
variable GA-ATOL-EXP   variable GA-RTOL-EXP
create GA-TEXT   GA-TEXT-CAP allot          variable GA-TEXT-U
create GA-FILE   GA-FILE-CAP allot          variable GA-FILE-U
create GA-DIR    FS-PATH-CAP allot          variable GA-DIR-U
create GA-PATH   FS-PATH-CAP allot          variable GA-PATH-U
create GA-KEY    96 allot                   variable GA-KEY-U
variable GA-F-OFF   variable GA-F-LEN   variable GA-F-OK     \ line-find result
variable GA-PF-DST  variable GA-PF-CNT  variable GA-PF-IDX   \ float-parse cursor

\ ---- reason buffer (mirrors golden.f GO-RE) --------------------------------
128 constant GA-RE-CAP
create GA-RE GA-RE-CAP allot  variable GA-RE-U
: GA-RE-RESET ( -- )  0 GA-RE-U ! ;
: GA-RE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GA-RE-U @ u + GA-RE-CAP > if exit then
   a GA-RE GA-RE-U @ + u BYTE-COPY  GA-RE-U @ u + GA-RE-U ! ;
public
: GA-RE$ ( -- ptr u8 n )  GA-RE GA-RE-U @ ;
private

\ ---- host-executability membership (single home; golden.f reuses it) --------
public
: GA-OP-BLOCKS? ( opkind -- bool )               \ op has no host oracle / reference?
   dup OPR-COMPLETE? 0=  swap EX-OP-OK? 0= or ;
: GA-SUPPORTED? ( -- bool )
   MIR-N@ 0 ?do
      i MIR-NODE-ID MIR-OP@ GA-OP-BLOCKS? if false unloop exit then
   loop true ;
: GA-FIRST-BAD ( -- n )                           \ first blocking node index, or -1 (op refetched at use)
   MIR-N@ 0 ?do
      i MIR-NODE-ID MIR-OP@ GA-OP-BLOCKS? if i unloop exit then
   loop -1 ;
private

\ ---- synthetic input synthesis (shared by golden self-consistency + GA-SAVE) --
\ Gather's index operand must hold valid indices; those slots get a deterministic
\ in-range REVERSAL of the gather source rows (src_rows-1 - e mod src_rows): exact
\ small integers (unambiguous under the kernel's +0.5/cvt.rzi rounding), never
\ constant and never the identity for src_rows>1, so the golden exercises a real
\ row permutation instead of selecting source row 0 only. A fixed (e*small) mod
\ src_rows multiplier was rejected: it degenerates to constant 0 whenever the
\ multiplier divides src_rows. src_rows comes from the gather node's data operand
\ (min over all gathers indexing via the slot, so every consumer stays in range).
: GA-SLOT-ELEMS ( MIR:input-slot -- n ) {: s:MIR:input-slot :}
   s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ SHAPE-ELEMS DIM-RAW ;
public
: GA-IN-PTR ( MIR:input-slot -- ptr a )
   SLOT>RAW cells GA-IN-OFF + @ {: off:n :}  GA-ARENA off T-AT ;
private

: GA-NODE-IDX? ( CAD-KIND:node-id MIR:input-slot -- bool )
   {: nd:CAD-KIND:node-id s:MIR:input-slot :}
   nd MIR-OP@ MAKI-OPKIND:GATHER MAKI-OPKIND:EQ 0= if false exit then
   nd 1 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? 0= if false exit then
   r MIR-REF-SLOT s MIR-SLOT= ;
: GA-SRC-ROWS ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}   \ gather node's data-operand row count
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r MIR-REF-INPUT?
   if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-REF-NODE MIR-ROWS@ then ROWS-RAW ;
variable GA-IDX-MIN
: GA-IDX-ROWS ( MIR:input-slot -- n ) {: s:MIR:input-slot :}   \ min src rows over gathers indexing slot s (0 = none)
   0 GA-IDX-MIN !
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: nd:CAD-KIND:node-id :}
      nd s GA-NODE-IDX? if
         nd GA-SRC-ROWS {: r:n :}
         GA-IDX-MIN @ 0=  r GA-IDX-MIN @ <  or if r GA-IDX-MIN ! then
      then
   loop
   GA-IDX-MIN @ ;
: GA-IDX-VAL ( n n -- r ) {: rows:n e:n :}       \ reversed in-range source row for index elem e
   rows 1-  e rows mod  -  s>f ;
: GA-FILL-VAL ( MIR:input-slot n -- r ) {: s:MIR:input-slot e:n :}
   s GA-IDX-ROWS {: rows:n :}
   rows 0 > if rows e GA-IDX-VAL exit then
   s SLOT>RAW 5 * e +  13 mod  s>f  0.17 f*  0.4 f+ ;
: GA-FILL-SLOT ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   s GA-IN-PTR {: p:ptr :}
   s GA-SLOT-ELEMS 0 ?do  s i GA-FILL-VAL  p i T-SET  loop ;
: GA-ALLOC-SLOT ( MIR:input-slot -- ) {: s:MIR:input-slot :}   \ carve arena space + record the offset
   s GA-SLOT-ELEMS {: e:n :}
   GA-BUMP @ {: off:n :}
   off e + GA-ARENA-CELLS > if E-GA-CAP throw then
   off s SLOT>RAW cells GA-IN-OFF + !  off e + GA-BUMP ! ;

public
: GA-BIND-SYNTH ( -- )                            \ reset EX; bind+fill synthetic inputs
   EX-RESET  0 GA-BUMP !
   MIR-IN-SLOTS@ 0 ?do
      i MIR-SLOT-ID {: s:MIR:input-slot :}
      s GA-ALLOC-SLOT
      s GA-IN-PTR s EX-BIND
      s GA-FILL-SLOT
   loop ;
private

\ ---- artifact path: <store-root>/golden/<NAME>.artifact ---------------------
: GA-FILE$ ( -- ptr u8 n )                        \ "<NAME>.artifact" (dedicated buffer)
   MIR-NAME$ {: na:ptr nu:n :}
   nu 9 + GA-FILE-CAP > if E-GA-CAP throw then
   na GA-FILE nu BYTE-COPY
   s" .artifact" {: sa:ptr su:n :}
   sa GA-FILE nu + su BYTE-COPY
   nu su + GA-FILE-U !
   GA-FILE GA-FILE-U @ ;
: GA-DIR$ ( -- ptr u8 n )                         \ <store-root>/golden (root ensured)
   STORE-ROOT+ s" golden" GA-DIR JOIN-PATH GA-DIR-U !
   GA-DIR GA-DIR-U @ ;
: GA-PATH$ ( -- ptr u8 n )
   GA-DIR$ {: da:ptr du:n :}
   GA-FILE$ {: fa:ptr fu:n :}
   da du fa fu GA-PATH JOIN-PATH GA-PATH-U !
   GA-PATH GA-PATH-U @ ;
: GA-ENSURE-DIR ( -- )  GA-DIR$ MAKE-DIRS ;

public
: GA-EXISTS? ( -- bool )
   MIR-N@ 0= if false exit then
   GA-PATH$ FILE? ;
private

\ ---- tolerance --------------------------------------------------------------
: GA-OUT-NODE ( -- CAD-KIND:node-id )  MIR-N@ 1- MIR-NODE-ID ;
: GA-DEFAULT-TOL ( dtype -- n n )                 \ dtype -> atol-exp rtol-exp
   MATCH dtype
      df32  OF GA-F32-ATOL-EXP GA-F32-RTOL-EXP ENDOF
      df16  OF GA-LOW-ATOL-EXP GA-LOW-RTOL-EXP ENDOF
      dbf16 OF GA-LOW-ATOL-EXP GA-LOW-RTOL-EXP ENDOF
      du32  OF GA-LOW-ATOL-EXP GA-LOW-RTOL-EXP ENDOF
      di32  OF GA-LOW-ATOL-EXP GA-LOW-RTOL-EXP ENDOF
   ;MATCH ;
: GA-SET-SAVE-TOL ( -- )                          \ tolerance for the model output dtype
   GA-OUT-NODE MIR-DT@ GA-DEFAULT-TOL GA-RTOL-EXP ! GA-ATOL-EXP ! ;

\ ---- artifact text builder --------------------------------------------------
: GA-T-RESET ( -- )  0 GA-TEXT-U ! ;
: GA-T-CK ( n -- ) {: k:n :}  GA-TEXT-U @ k + GA-TEXT-CAP > if E-GA-CAP throw then ;
: GA-T+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u GA-T-CK  a GA-TEXT GA-TEXT-U @ + u BYTE-COPY  GA-TEXT-U @ u + GA-TEXT-U ! ;
: GA-T-C ( n -- ) {: c:n :}
   1 GA-T-CK  c GA-TEXT GA-TEXT-U @ + c!  GA-TEXT-U @ 1+ GA-TEXT-U ! ;
: GA-T-NL ( -- )  $0A GA-T-C ;
: GA-T-INT ( n -- )  SB-RESET SB-INT SB$ GA-T+ ;
: GA-T-FLOAT ( r -- )  SB-RESET GA-DECIMALS SB-FIX SB$ GA-T+ ;
: GA-T-KEY ( ptr u8 n -- )  GA-T+ s" : " GA-T+ ;          \ "key: "
: GA-T-IKEY ( ptr u8 n n -- ) {: idx:n :}  GA-T+ $2E GA-T-C idx GA-T-INT ;   \ "prefix.<idx>"
: GA-WRITE-SHAPE ( CAD-KIND:rows CAD-KIND:cols -- )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   rows ROWS-RAW GA-T-INT $78 GA-T-C cols COLS-RAW GA-T-INT ;
: GA-WRITE-DATA ( ptr a n -- ) {: p:ptr n:n :}
   n 0 ?do  i 0 > if $20 GA-T-C then  p i T-GET GA-T-FLOAT  loop ;

: GA-WRITE-HEADER ( -- )
   s" artifact.model" GA-T-KEY  MIR-NAME$ GA-T+ GA-T-NL
   s" artifact.tolerance.atol-exp" GA-T-KEY  GA-ATOL-EXP @ GA-T-INT GA-T-NL
   s" artifact.tolerance.rtol-exp" GA-T-KEY  GA-RTOL-EXP @ GA-T-INT GA-T-NL
   s" artifact.inputs" GA-T-KEY  MIR-IN-SLOTS@ GA-T-INT GA-T-NL ;
: GA-WRITE-INPUT ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   s SLOT>RAW {: raw:n :}
   s" input" raw GA-T-IKEY  s" .shape" GA-T-KEY
      s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ GA-WRITE-SHAPE GA-T-NL
   s" input" raw GA-T-IKEY  s" .data"  GA-T-KEY
      s GA-IN-PTR s GA-SLOT-ELEMS GA-WRITE-DATA GA-T-NL ;
: GA-WRITE-OUTPUT ( -- )
   GA-OUT-NODE {: nd:CAD-KIND:node-id :}
   s" output.shape" GA-T-KEY  nd MIR-ROWS@ nd MIR-COLS@ GA-WRITE-SHAPE GA-T-NL
   s" output.data"  GA-T-KEY  nd EX-OUT@ nd EX-NODE-ELEMS GA-WRITE-DATA GA-T-NL ;
: GA-BUILD-TEXT ( -- )
   GA-T-RESET  GA-WRITE-HEADER
   MIR-IN-SLOTS@ 0 ?do  i MIR-SLOT-ID GA-WRITE-INPUT  loop
   GA-WRITE-OUTPUT ;

public
: GA-SAVE ( -- )
   MIR-N@ 0= if E-GA-UNSUP throw then
   GA-SUPPORTED? 0= if E-GA-UNSUP throw then
   GA-SET-SAVE-TOL
   GA-BIND-SYNTH
   MIR-N@ EX-RUN-N
   GA-BUILD-TEXT
   GA-ENSURE-DIR
   GA-PATH$ GA-TEXT GA-TEXT-U @ WRITE-ALL ;
private

\ ---- artifact text reader ---------------------------------------------------
: GA-TEXT$ ( -- ptr u8 n )  GA-TEXT GA-TEXT-U @ ;
: GA-NEXT-NL ( ptr u8 n n -- n ) {: ta:ptr tu:n off:n :}    \ first newline at/after off, else tu
   off begin dup tu < while  dup ta + c@ $0A = if exit then  1+  repeat ;
: GA-LINE-MATCH? ( ptr u8 n ptr u8 n -- bool ) {: la:ptr lu:n ka:ptr ku:n :}
   lu ku < if false exit then
   la ku ka ku STR= ;
: GA-LINE-TRY ( ptr u8 n n ptr u8 n -- n ) {: ta:ptr tu:n off:n ka:ptr ku:n :}
   ta tu off GA-NEXT-NL {: ed:n :}
   GA-F-OK @ 0= if
      ta off +  ed off -  ka ku GA-LINE-MATCH? if
         off ku + GA-F-OFF !  ed off - ku - GA-F-LEN !  -1 GA-F-OK !
      then
   then
   ed 1+ ;
: GA-FIND-LINE ( ptr u8 n -- bool ) {: ka:ptr ku:n :}       \ prefix (incl ": ") found?
   0 GA-F-OK !
   GA-TEXT$ {: ta:ptr tu:n :}
   0 begin dup tu < while  >r ta tu r> ka ku GA-LINE-TRY  repeat drop
   GA-F-OK @ 0= 0= ;
: GA-VAL$ ( -- ptr u8 n )  GA-TEXT GA-F-OFF @ +  GA-F-LEN @ ;
: GA-REQ-LINE ( ptr u8 n -- )  GA-FIND-LINE 0= if E-GA-PARSE throw then ;
: GA-PARSE-INT-VAL ( ptr u8 n -- n )
   STR>NUMBER?
   MATCH option none OF E-GA-PARSE throw ENDOF some OF ENDOF ;MATCH ;

\ indexed key builders: "input.<i>.data: " / "input.<i>.shape: "
: GA-KEY-RESET ( -- )  0 GA-KEY-U ! ;
: GA-KEY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a GA-KEY GA-KEY-U @ + u BYTE-COPY  GA-KEY-U @ u + GA-KEY-U ! ;
: GA-KEY-C ( n -- ) {: c:n :}  c GA-KEY GA-KEY-U @ + c!  GA-KEY-U @ 1+ GA-KEY-U ! ;
: GA-KEY-INT ( n -- )  SB-RESET SB-INT SB$ GA-KEY+ ;
: GA-IN-KEY$ ( n ptr u8 n -- ptr u8 n ) {: i:n sa:ptr su:n :}   \ i suffix -> "input.<i><suffix>"
   GA-KEY-RESET  s" input" GA-KEY+  $2E GA-KEY-C
   i GA-KEY-INT  sa su GA-KEY+  GA-KEY GA-KEY-U @ ;

\ parse `cnt` space-separated decimals from a value span into dst[0..cnt)
: GA-PF-STORE ( r -- ) {: v:r :}
   GA-PF-IDX @ GA-PF-CNT @ >= if E-GA-PARSE throw then
   v GA-PF-DST @ GA-PF-IDX @ T-SET
   GA-PF-IDX @ 1+ GA-PF-IDX ! ;
: GA-PF-TOKEN ( ptr u8 n -- ) {: ta:ptr tu:n :}
   tu 0= if exit then
   ta tu STR>FLOAT 0= if E-GA-PARSE throw then
   GA-PF-STORE ;
: GA-PF-STEP ( ptr u8 n n -- n bool ) {: va:ptr vu:n start:n :}   \ value start -- nextstart continue?
   va vu $20 start SPLIT-NEXT {: ta:ptr tu:n nx:n ok:bool :}
   ok if ta tu GA-PF-TOKEN then
   nx ok ;
: GA-PARSE-FLOATS ( ptr u8 n ptr a n -- ) {: va:ptr vu:n dst:ptr cnt:n :}
   dst GA-PF-DST !  cnt GA-PF-CNT !  0 GA-PF-IDX !
   0 begin va vu rot GA-PF-STEP while repeat drop
   GA-PF-IDX @ cnt <> if E-GA-PARSE throw then ;

: GA-PARSE-SHAPE ( ptr u8 n -- n n ) {: va:ptr vu:n :}   \ "RxC" -> rows cols
   va vu $78 INDEX-OF MATCH option
     none OF E-GA-PARSE throw ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: xi:n :}
   va xi GA-PARSE-INT-VAL
   va xi 1+ +  vu xi 1+ -  GA-PARSE-INT-VAL ;

: GA-CHECK-SHAPE ( n n CAD-KIND:rows CAD-KIND:cols -- )   \ parsed extents vs IR extents
   {: ar:n ac:n er:CAD-KIND:rows ec:CAD-KIND:cols :}
   ar er ROWS-RAW <> ac ec COLS-RAW <> or if E-GA-SHAPE throw then ;

: GA-PARSE-TOL ( -- )
   s" artifact.tolerance.atol-exp: " GA-REQ-LINE  GA-VAL$ GA-PARSE-INT-VAL GA-ATOL-EXP !
   s" artifact.tolerance.rtol-exp: " GA-REQ-LINE  GA-VAL$ GA-PARSE-INT-VAL GA-RTOL-EXP ! ;
: GA-PARSE-COUNT ( -- )
   s" artifact.inputs: " GA-REQ-LINE  GA-VAL$ GA-PARSE-INT-VAL
   MIR-IN-SLOTS@ <> if E-GA-SHAPE throw then ;
: GA-PARSE-INPUT ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   s SLOT>RAW {: raw:n :}
   s GA-SLOT-ELEMS {: e:n :}
   raw s" .shape: " GA-IN-KEY$ GA-REQ-LINE  GA-VAL$ GA-PARSE-SHAPE
      s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ GA-CHECK-SHAPE
   s GA-ALLOC-SLOT
   raw s" .data: " GA-IN-KEY$ GA-REQ-LINE  GA-VAL$ s GA-IN-PTR e GA-PARSE-FLOATS
   s GA-IN-PTR s EX-BIND ;
: GA-PARSE-OUTPUT ( -- )
   GA-OUT-NODE {: nd:CAD-KIND:node-id :}
   nd EX-NODE-ELEMS {: e:n :}
   e GA-EXP-CELLS > if E-GA-CAP throw then
   s" output.shape: " GA-REQ-LINE  GA-VAL$ GA-PARSE-SHAPE
      nd MIR-ROWS@ nd MIR-COLS@ GA-CHECK-SHAPE
   s" output.data: " GA-REQ-LINE  GA-VAL$ GA-EXP e GA-PARSE-FLOATS
   e GA-EXP-N ! ;
: GA-PARSE ( -- )
   GA-PARSE-TOL  GA-PARSE-COUNT
   EX-RESET  0 GA-BUMP !
   MIR-IN-SLOTS@ 0 ?do  i MIR-SLOT-ID GA-PARSE-INPUT  loop
   GA-PARSE-OUTPUT ;

public
: GA-LOAD ( -- bool )
   GA-EXISTS? 0= if false exit then
   GA-PATH$ {: pa:ptr pu:n :}
   pa pu FILE-SIZE GA-TEXT-CAP > if E-GA-CAP throw then
   pa pu GA-TEXT GA-TEXT-CAP READ-ALL GA-TEXT-U !
   GA-PARSE
   true ;

\ loaded expected-output accessors (also the corruption seam the tests drive)
: GA-EXP@ ( n -- r ) {: i:n :}
   i 0 < i GA-EXP-N @ >= or if E-GA-CAP throw then
   GA-EXP i T-GET ;
: GA-EXP! ( r n -- ) {: v:r i:n :}
   i 0 < i GA-EXP-N @ >= or if E-GA-CAP throw then
   v GA-EXP i T-SET ;
private

\ ---- tolerance comparison (section 11: |computed-expected| <= atol + rtol*|expected|) --
: GA-WITHIN? ( r r n n -- bool ) {: a:r b:r ae:n re:n :}
   ae POW10  re POW10 b fabs f* f+ {: t:r :}
   a b f- fabs {: d:r :}
   t d f< 0= ;
: GA-COMPARE ( ptr a ptr a n n n -- bool ) {: cp:ptr ep:ptr n:n ae:n re:n :}
   n 0 ?do
      cp i T-GET  ep i T-GET  ae re GA-WITHIN? 0= if false unloop exit then
   loop  true ;

: GA-PASS-REASON ( -- )
   GA-RE-RESET  s" golden: external artifact " GA-RE+ MIR-NAME$ GA-RE+ s"  matched" GA-RE+ ;
: GA-FAIL-REASON ( -- )
   GA-RE-RESET  s" golden: external artifact " GA-RE+ MIR-NAME$ GA-RE+ s"  mismatch beyond tolerance" GA-RE+ ;

public
\ GA-VERDICT runs the forward IR on the currently-bound inputs (GA-LOAD must precede it)
\ and compares its output to the loaded expected under the loaded tolerance.
: GA-VERDICT ( -- n )
   MIR-N@ EX-RUN-N
   GA-OUT-NODE {: nd:CAD-KIND:node-id :}
   nd EX-OUT@  GA-EXP  GA-EXP-N @  GA-ATOL-EXP @  GA-RTOL-EXP @  GA-COMPARE
   if GA-PASS-REASON V-PASS else GA-FAIL-REASON V-FAIL then ;

\ GA-CHECK is the full external-artifact verdict for the current model.
: GA-CHECK ( -- n )
   MIR-N@ 0= if GA-RE-RESET s" golden: empty model" GA-RE+ V-NOTRUN exit then
   GA-SUPPORTED? 0= if GA-RE-RESET s" golden: model not host-executable" GA-RE+ V-NOTRUN exit then
   GA-EXISTS? 0= if GA-RE-RESET s" golden: no external reference artifact" GA-RE+ V-NOTRUN exit then
   GA-LOAD drop
   GA-VERDICT ;

;package
