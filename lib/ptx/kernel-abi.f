\ kernel-abi.f - structured kernel-ABI record (docs/ptx-sketch.md item 3).
\
\ ONE checked record is the single source of truth for a kernel's entry name,
\ block size, grid-derivation token, ordered logical params (span / matrix /
\ uniform), and the derived flat .param layout: per field the byte offset,
\ size, .param PTX type, role (base/len/cols/rows/stride/scalar), and source
\ (param / launch-derived / static / dense-derived), with equal-extent-token
\ dedup (two spans sharing extent token n lower to ONE p_n). The PTX entry
\ line and ld.param loads render FROM this record (lib/ptx/cg.f CG-ENTRY /
\ CG-PARAMS) and launch packing reads the same offsets/total
\ (tools/ptx/cuda-launch.f), so the ABI is stated once, not three times.
\
\ Lowering: pass 1 gives every logical param its primary .param slot in
\ declaration order (span/matrix base .u64, uniform scalar .f32); pass 2 adds
\ the dedup'd .u32 extent values in first-mention order (span extent, matrix
\ cols); pass 3 adds the non-.param derived rows (matrix rows = launch-derived
\ gridDim.x per the launch ABI, matrix stride = dense-derived, equals cols).
\ Offsets are naturally aligned (.u64 at 8, .u32/.f32 at 4). MATRIX+ encodes
\ the row-kernel lowering (GRID: extent-r); a param-rows variant is added when
\ a record-rendered kernel needs one.
\
\ Load after lib/errors.f, lib/string.f, and lib/fmt.f.

package KABI

public

\ logical param kinds
0 constant KIND-SPAN
1 constant KIND-MATRIX
2 constant KIND-UNIFORM

\ flat field roles
0 constant ROLE-BASE
1 constant ROLE-LEN
2 constant ROLE-COLS
3 constant ROLE-ROWS
4 constant ROLE-STRIDE
5 constant ROLE-SCALAR

\ flat field sources
0 constant SRC-PARAM        \ a real .param slot with a byte offset
1 constant SRC-LAUNCH       \ derived from the launch (gridDim.x rows)
2 constant SRC-STATIC       \ compile-time immediate baked into the PTX
3 constant SRC-DENSE        \ dense-derived (row-major stride equals cols)

private

\ --- capacities ---
$10 constant MAX-LOGICAL
$18 constant MAX-FIELDS
$40 constant NAME-CAP
$1000 constant LPOOL-CAP
$1000 constant FPOOL-CAP

\ --- .param register classes ---
0 constant CL-U64           \ .u64 -> %rd
1 constant CL-F32           \ .f32 -> %f
2 constant CL-U32           \ .u32 -> %r

\ --- element types ---
0 constant EL-F32
1 constant EL-U32

\ --- record state ---
create NAME-BUF NAME-CAP allot
create GRID-BUF NAME-CAP allot
create SNAME-BUF NAME-CAP allot
create LPOOL LPOOL-CAP allot
create FPOOL FPOOL-CAP allot

variable NAME-U
variable GRID-U
variable BLOCK-CELL
variable LPOOL-U
variable FPOOL-U
variable NLP
variable NF
variable OFF-CURSOR
variable STALE

create LP-KIND   MAX-LOGICAL cells allot
create LP-ELEM   MAX-LOGICAL cells allot
create LP-NM-OFF MAX-LOGICAL cells allot
create LP-NM-U   MAX-LOGICAL cells allot
create LP-E1-OFF MAX-LOGICAL cells allot    \ span extent / matrix rows token
create LP-E1-U   MAX-LOGICAL cells allot
create LP-E2-OFF MAX-LOGICAL cells allot    \ matrix cols token
create LP-E2-U   MAX-LOGICAL cells allot
create LP-AL     MAX-LOGICAL cells allot

create FD-NM-OFF MAX-FIELDS cells allot
create FD-NM-U   MAX-FIELDS cells allot
create FD-CLASS  MAX-FIELDS cells allot
create FD-ROLE   MAX-FIELDS cells allot
create FD-SRC    MAX-FIELDS cells allot
create FD-OFF    MAX-FIELDS cells allot     \ .param byte offset; -1 when not a .param
create FD-EX-OFF MAX-FIELDS cells allot     \ extent dedup token; empty when none
create FD-EX-U   MAX-FIELDS cells allot

: KB@ ( ptr a n -- n ) {: a:ptr i:n :}
   a i cells + @ ;

: KB! ( n ptr a n -- ) {: v:n a:ptr i:n :}
   v a i cells + ! ;

: TOKEN-CHECK ( n -- ) {: u:n :}
   u 0= if E-KABI-TOKEN throw then
   u NAME-CAP > if E-KABI-TOKEN throw then ;

: LPOOL+ ( ptr u8 n -- n n ) {: a:ptr u:n :}
   LPOOL-U @ u + LPOOL-CAP > if E-KABI-CAP throw then
   a LPOOL LPOOL-U @ + u BYTE-COPY
   LPOOL-U @ u
   LPOOL-U @ u + LPOOL-U ! ;

: FPOOL+ ( ptr u8 n -- n n ) {: a:ptr u:n :}
   FPOOL-U @ u + FPOOL-CAP > if E-KABI-CAP throw then
   a FPOOL FPOOL-U @ + u BYTE-COPY
   FPOOL-U @ u
   FPOOL-U @ u + FPOOL-U ! ;

: LP-STR! ( ptr u8 n ptr a ptr a -- ) {: a:ptr u:n offa:ptr ua:ptr :}
   a u LPOOL+ {: o:n l:n :}
   o offa NLP @ KB!
   l ua NLP @ KB! ;

: LP-IDX-CHECK ( n -- ) {: i:n :}
   i 0 < if E-KABI-FIELD throw then
   i NLP @ >= if E-KABI-FIELD throw then ;

: P-NAME$ ( n -- ptr u8 n ) {: i:n :}
   LPOOL LP-NM-OFF i KB@ +  LP-NM-U i KB@ ;

: P-EXT$ ( n -- ptr u8 n ) {: i:n :}
   LPOOL LP-E1-OFF i KB@ +  LP-E1-U i KB@ ;

: P-EXT2$ ( n -- ptr u8 n ) {: i:n :}
   LPOOL LP-E2-OFF i KB@ +  LP-E2-U i KB@ ;

: LP+ ( ptr u8 n n n ptr u8 n ptr u8 n -- )   \ name kind elem ext1 ext2
   {: nm:ptr nmu:n kind:n elem:n e1:ptr e1u:n e2:ptr e2u:n :}
   NLP @ MAX-LOGICAL >= if E-KABI-CAP throw then
   nmu TOKEN-CHECK
   nm nmu LP-NM-OFF LP-NM-U LP-STR!
   e1 e1u LP-E1-OFF LP-E1-U LP-STR!
   e2 e2u LP-E2-OFF LP-E2-U LP-STR!
   kind LP-KIND NLP @ KB!
   elem LP-ELEM NLP @ KB!
   0 LP-AL NLP @ KB!
   NLP @ 1+ NLP !
   1 STALE ! ;

\ --- flat field derivation ---

: FD-NAME$ ( n -- ptr u8 n ) {: i:n :}
   FPOOL FD-NM-OFF i KB@ +  FD-NM-U i KB@ ;

: FD-EXT$ ( n -- ptr u8 n ) {: i:n :}
   FPOOL FD-EX-OFF i KB@ +  FD-EX-U i KB@ ;

: FIND-FIELD ( ptr u8 n -- n ) {: a:ptr u:n :}   \ field index by name, or -1
   0 begin dup NF @ < while
      dup FD-NAME$ a u STR= if exit then
      1+
   repeat drop -1 ;

: FIND-EXT ( ptr u8 n -- n ) {: a:ptr u:n :}     \ field index by extent token, or -1
   0 begin dup NF @ < while
      dup FD-EXT$ a u STR= if exit then
      1+
   repeat drop -1 ;

: CLASS-SIZE ( n -- n ) {: c:n :}
   c CL-U64 = if 8 exit then 4 ;

: CLASS-PTX$ ( n -- ptr u8 n ) {: c:n :}
   c CL-U64 = if s" .u64" exit then
   c CL-F32 = if s" .f32" exit then
   s" .u32" ;

: CLASS-REG$ ( n -- ptr u8 n ) {: c:n :}          \ " %rd" etc: single leading space
   c CL-U64 = if s"  %rd" exit then
   c CL-F32 = if s"  %f" exit then
   s"  %r" ;

: ALIGN-UP ( n n -- n ) {: off:n sz:n :}
   off sz 1- + sz 1- invert and ;

: FIELD-OFF! ( n n -- )                           \ class src -> offset cell for field NF
   {: cls:n src:n :}
   src SRC-PARAM = if
      OFF-CURSOR @ cls CLASS-SIZE ALIGN-UP {: off:n :}
      off FD-OFF NF @ KB!
      off cls CLASS-SIZE + OFF-CURSOR !
   else
      -1 FD-OFF NF @ KB!
   then ;

: FIELD+ ( ptr u8 n ptr u8 n n n n -- )           \ name ext class role src
   {: nm:ptr nmu:n ex:ptr exu:n cls:n role:n src:n :}
   NF @ MAX-FIELDS >= if E-KABI-CAP throw then
   nmu TOKEN-CHECK
   nm nmu FIND-FIELD -1 <> if E-KABI-DUP throw then
   nm nmu FPOOL+ {: no:n nu:n :}
   ex exu FPOOL+ {: eo:n eu:n :}
   no FD-NM-OFF NF @ KB!  nu FD-NM-U NF @ KB!
   eo FD-EX-OFF NF @ KB!  eu FD-EX-U NF @ KB!
   cls FD-CLASS NF @ KB!
   role FD-ROLE NF @ KB!
   src FD-SRC NF @ KB!
   cls src FIELD-OFF!
   NF @ 1+ NF ! ;

: EXT-FIELD+ ( ptr u8 n n n -- )                  \ ext-token role src: dedup'd u32 extent value
   {: ex:ptr exu:n role:n src:n :}
   exu TOKEN-CHECK
   ex exu FIND-EXT {: k:n :}
   k -1 <> if
      FD-SRC k KB@ src <> if E-KABI-DUP throw then
      exit
   then
   ex exu ex exu CL-U32 role src FIELD+ ;

: STRIDE-NAME$ ( ptr u8 n -- ptr u8 n ) {: nm:ptr nmu:n :}
   nmu 7 + NAME-CAP > if E-KABI-TOKEN throw then
   nm SNAME-BUF nmu BYTE-COPY
   s" -stride" {: sa:ptr su:n :}
   sa SNAME-BUF nmu + su BYTE-COPY
   SNAME-BUF nmu su + ;

: DERIVE-BASE+ ( n -- ) {: i:n :}
   LP-KIND i KB@ KIND-UNIFORM = if
      i P-NAME$ s" " CL-F32 ROLE-SCALAR SRC-PARAM FIELD+
   else
      i P-NAME$ s" " CL-U64 ROLE-BASE SRC-PARAM FIELD+
   then ;

: DERIVE-EXTENT+ ( n -- ) {: i:n :}
   LP-KIND i KB@ KIND-SPAN = if
      i P-EXT$ ROLE-LEN SRC-PARAM EXT-FIELD+
   then
   LP-KIND i KB@ KIND-MATRIX = if
      i P-EXT2$ ROLE-COLS SRC-PARAM EXT-FIELD+
   then ;

: DERIVE-DERIVED+ ( n -- ) {: i:n :}
   LP-KIND i KB@ KIND-MATRIX = if
      i P-EXT$ ROLE-ROWS SRC-LAUNCH EXT-FIELD+
      i P-NAME$ STRIDE-NAME$ i P-EXT2$ CL-U32 ROLE-STRIDE SRC-DENSE FIELD+
   then ;

: DERIVE ( -- )
   0 NF !  0 OFF-CURSOR !  0 FPOOL-U !
   NLP @ 0 ?do i DERIVE-BASE+ loop
   NLP @ 0 ?do i DERIVE-EXTENT+ loop
   NLP @ 0 ?do i DERIVE-DERIVED+ loop ;

: ENSURE ( -- )
   STALE @ 0= if exit then
   DERIVE
   0 STALE ! ;

: FD-IDX-CHECK ( n -- ) {: i:n :}
   i 0 < if E-KABI-FIELD throw then
   i NF @ >= if E-KABI-FIELD throw then ;

: FD-PARAM? ( n -- bool ) {: i:n :}
   FD-SRC i KB@ SRC-PARAM = ;

: FD-IDX-OF ( ptr u8 n -- n )
   FIND-FIELD dup -1 = if E-KABI-FIELD throw then ;

\ --- public record API ---

public

: RESET ( -- )
   0 NAME-U !  0 GRID-U !  0 BLOCK-CELL !
   0 LPOOL-U !  0 NLP !  0 NF !  0 OFF-CURSOR !  0 FPOOL-U !
   1 STALE ! ;

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u TOKEN-CHECK
   a NAME-BUF u BYTE-COPY
   u NAME-U ! ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: GRID! ( ptr u8 n -- ) {: a:ptr u:n :}
   u TOKEN-CHECK
   a GRID-BUF u BYTE-COPY
   u GRID-U ! ;

: GRID$ ( -- ptr u8 n )
   GRID-BUF GRID-U @ ;

: BLOCK! ( n -- )
   BLOCK-CELL ! ;

: BLOCK@ ( -- n )
   BLOCK-CELL @ ;

: SPAN+ ( ptr u8 n ptr u8 n -- )                  \ name extent-token: f32 span
   {: nm:ptr nmu:n ex:ptr exu:n :}
   exu TOKEN-CHECK
   nm nmu KIND-SPAN EL-F32 ex exu s" " LP+ ;

: U32-SPAN+ ( ptr u8 n ptr u8 n -- )              \ name extent-token: u32 index span
   {: nm:ptr nmu:n ex:ptr exu:n :}
   exu TOKEN-CHECK
   nm nmu KIND-SPAN EL-U32 ex exu s" " LP+ ;

: MATRIX+ ( ptr u8 n ptr u8 n ptr u8 n -- )       \ name rows-token cols-token: f32 matrix
   {: nm:ptr nmu:n rt:ptr rtu:n ct:ptr ctu:n :}
   rtu TOKEN-CHECK
   ctu TOKEN-CHECK
   nm nmu KIND-MATRIX EL-F32 rt rtu ct ctu LP+ ;

: UNIFORM+ ( ptr u8 n -- )                        \ name: f32 uniform scalar
   {: nm:ptr nmu:n :}
   nm nmu KIND-UNIFORM EL-F32 s" " s" " LP+ ;

: ALIGN! ( n -- ) {: v:n :}                       \ alignment refinement on the LAST logical param
   NLP @ 0= if E-KABI-FIELD throw then
   v LP-AL NLP @ 1- KB! ;

\ --- logical param accessors ---

: N-PARAMS ( -- n )
   NLP @ ;

: PARAM-KIND ( n -- n )
   dup LP-IDX-CHECK {: i:n :}
   LP-KIND i KB@ ;

: PARAM-NAME$ ( n -- ptr u8 n )
   dup LP-IDX-CHECK P-NAME$ ;

: PARAM-ELEM$ ( n -- ptr u8 n )
   dup LP-IDX-CHECK {: i:n :}
   LP-ELEM i KB@ EL-U32 = if s" u32" exit then
   s" f32" ;

: PARAM-EXT$ ( n -- ptr u8 n )                    \ span extent / matrix rows token
   dup LP-IDX-CHECK P-EXT$ ;

: PARAM-EXT2$ ( n -- ptr u8 n )                   \ matrix cols token
   dup LP-IDX-CHECK P-EXT2$ ;

: PARAM-ALIGN ( n -- n )
   dup LP-IDX-CHECK {: i:n :}
   LP-AL i KB@ ;

\ --- flat field accessors (derive on demand) ---

: N-FIELDS ( -- n )
   ENSURE NF @ ;

: FIELD-NAME$ ( n -- ptr u8 n )
   ENSURE dup FD-IDX-CHECK FD-NAME$ ;

: FIELD-PTX$ ( n -- ptr u8 n )
   ENSURE dup FD-IDX-CHECK {: i:n :}
   FD-CLASS i KB@ CLASS-PTX$ ;

: FIELD-SIZE ( n -- n )
   ENSURE dup FD-IDX-CHECK {: i:n :}
   FD-CLASS i KB@ CLASS-SIZE ;

: FIELD-OFF ( n -- n )                            \ .param byte offset; -1 for derived fields
   ENSURE dup FD-IDX-CHECK {: i:n :}
   FD-OFF i KB@ ;

: FIELD-ROLE ( n -- n )
   ENSURE dup FD-IDX-CHECK {: i:n :}
   FD-ROLE i KB@ ;

: FIELD-SRC ( n -- n )
   ENSURE dup FD-IDX-CHECK {: i:n :}
   FD-SRC i KB@ ;

: FIELD-EXT$ ( n -- ptr u8 n )                    \ extent dedup token; empty when none
   ENSURE dup FD-IDX-CHECK FD-EXT$ ;

: FIELD-PARAM? ( n -- bool )
   ENSURE dup FD-IDX-CHECK FD-PARAM? ;

: FIELD-REG ( n -- n )                            \ 1-based register index within the field's class
   ENSURE dup FD-IDX-CHECK {: k:n :}
   1
   k 0 ?do
      i FD-PARAM? if
         FD-CLASS i KB@ FD-CLASS k KB@ = if 1+ then
      then
   loop ;

: FIELD-INDEX ( ptr u8 n -- n )                   \ field index by name
   ENSURE FD-IDX-OF ;

: OFFSET-OF ( ptr u8 n -- n )                     \ .param byte offset by field name
   ENSURE FD-IDX-OF {: k:n :}
   FD-OFF k KB@ dup -1 = if E-KABI-FIELD throw then ;

: SIZE-OF ( ptr u8 n -- n )                       \ .param byte size by field name
   ENSURE FD-IDX-OF {: k:n :}
   FD-CLASS k KB@ CLASS-SIZE ;

: TOTAL ( -- n )                                  \ total .param bytes (cuParamSetSize)
   ENSURE OFF-CURSOR @ ;

: U64-N ( -- n )                                  \ .u64 param count (%rd registers consumed)
   ENSURE
   0 NF @ 0 ?do
      i FD-PARAM? if FD-CLASS i KB@ CL-U64 = if 1+ then then
   loop ;

: F32-N ( -- n )                                  \ .f32 param count (%f registers consumed)
   ENSURE
   0 NF @ 0 ?do
      i FD-PARAM? if FD-CLASS i KB@ CL-F32 = if 1+ then then
   loop ;

: U32-N ( -- n )                                  \ .u32 param count (%r registers consumed)
   ENSURE
   0 NF @ 0 ?do
      i FD-PARAM? if FD-CLASS i KB@ CL-U32 = if 1+ then then
   loop ;

\ --- PTX renders (byte-compatible with the historical hardcoded lines) ---

private

: ENTRY-ARG+ ( n n -- n ) {: k:n c:n :}           \ field-idx count -> count'
   c 0 > if s" , " SB-APPEND then
   s" .param " SB-APPEND
   FD-CLASS k KB@ CLASS-PTX$ SB-APPEND
   s"  p_" SB-APPEND
   k FD-NAME$ SB-APPEND
   c 1+ ;

public

: ENTRY$ ( -- ptr u8 n )                          \ ".visible .entry NAME(.param ...)"
   ENSURE
   SB-RESET
   s" .visible .entry " SB-APPEND
   NAME$ SB-APPEND
   s" (" SB-APPEND
   0 NF @ 0 ?do
      i FD-PARAM? if i swap ENTRY-ARG+ then
   loop drop
   s" )" SB-APPEND
   SB$ ;

: LD-LINE$ ( n -- ptr u8 n )                      \ "ld.param.u64 %rd1, [p_x];"
   ENSURE dup FD-IDX-CHECK {: k:n :}
   SB-RESET
   s" ld.param" SB-APPEND
   FD-CLASS k KB@ CLASS-PTX$ SB-APPEND
   FD-CLASS k KB@ CLASS-REG$ SB-APPEND
   k FIELD-REG SB-U
   s" , [p_" SB-APPEND
   k FD-NAME$ SB-APPEND
   s" ];" SB-APPEND
   SB$ ;

;package
