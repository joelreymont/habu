\ regalloc-verify.f - decide whether a finished register assignment is true of the
\ module it claims to be about.
\
\ docs/compiler-ir-design.md section 7.9 ("validate the final assignment
\ independently") and section 11.3, which lists the register-allocation validator
\ among the checks the JIT path runs before it makes anything executable. The
\ allocator in src/compiler/native/regalloc.f publishes claims; this file is what
\ turns an accepted claim into an answer a later stage may emit code from, and it
\ is the only place that answers at all.
\
\ WHAT INDEPENDENT MEANS HERE. Every fact this file decides on is re-derived from
\ the frozen module, never read out of the allocator's working tables. It walks
\ the block itself to learn where each value is written and where it is last
\ read, and it compares that against the interval the allocator recorded - so an
\ allocator that mis-measured a live range is caught by the disagreement, not
\ excused by it. The one thing it does take from the allocator is which module
\ and which routine contract the allocation was made for, because those are what
\ it is checking the assignment against; both are checked to be the ones it was
\ handed. Which register fields an instruction form shares is likewise re-derived:
\ the ties come out of the module's own schema table, so this file and the
\ allocator agree because they read one declaration, not because one told the
\ other.
\
\ THE INTERFERENCE RULE, IN FULL. Two different values may share a register
\ exactly when they are never live at the same instant. Order them by where they
\ are written:
\   - two values written at the same position are always live together. Only
\     block arguments can be, and they all arrive at once, so two arguments never
\     share a register - not even when one of them is never read, because the
\     caller still has to have put both somewhere.
\   - otherwise the earlier value is dead by the time the later one is written
\     exactly when its last read is at or before that position. An operation
\     reads its operands and then writes its results, so a value read for the
\     last time by operation i and a value written by operation i do not clash;
\     that is what lets a chain run in one register, and it is what makes the
\     move-wide overwrite's tied register legal rather than a special case.
\ A value that is never read is live only where it is written, which still costs
\ a register there, because the instruction writes one.
\
\ WHAT THIS FILE DOES NOT DECIDE. Whether the module is a well-formed A64IR
\ module at all is IR-VERIFY's and the dialect's, and whether the operations
\ compute what the source said is the selector's. This file assumes a verified
\ module and asks one question about it: is this assignment of registers to its
\ values legal, complete, and consistent with its own definitions and uses.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/regalloc.f

package A64RAV
private

\ Values in one block, the ceiling the allocator carries.
256 constant VMAX

\ The position of a block argument: before every operation of the block.
-1 constant ENTRY

7 constant VIEWS-N
0 constant V-OPP                     \ operation pool
1 constant V-OPR                     \ operation rows
2 constant V-VALR                    \ value rows
3 constant V-FUNR                    \ function rows
4 constant V-BLKR                    \ block rows
5 constant V-SCHP                    \ schema list pool
6 constant V-SCHR                    \ schema rows

0 constant ST-NONE
1 constant ST-ACCEPTED

here CELL 1- and CELL swap - CELL 1- and allot
variable ST
ST-NONE ST !
variable A-GEN
0 A-GEN !
variable N-VALS
0 N-VALS !

1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view

create D-AT VMAX cells allot         \ where the module says each value is written
create L-AT VMAX cells allot         \ where the module says each value is last read
create S-AT VMAX cells allot         \ whether the block defines this value at all

: KEY ( -- IR-ID:ir-module-key )     0 S-KEY @ ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;

: DEF-AT ( n -- n )                  cells D-AT + @ ;
: LAST-AT ( n -- n )                 cells L-AT + @ ;
: SEEN-AT ( n -- n )                 cells S-AT + @ ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells D-AT + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells L-AT + ! ;
: SEEN! ( n n -- )                   {: v:n k:n :} v k cells S-AT + ! ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SEEN!
      ENTRY i DEF!
      ENTRY i LAST!
   loop ;

\ ---- identity ----------------------------------------------------------------
: SAME-TYPE? ( IR-ID:ir-type-id IR-ID:ir-type-id -- bool )
   {: x:IR-ID:ir-type-id y:IR-ID:ir-type-id :}
   x IR-ID:TYPE-LOCAL y IR-ID:TYPE-LOCAL <> if false exit then
   x IR-ID:TYPE-OWNER y IR-ID:TYPE-OWNER IR-ID:MODULE-SAME? ;

\ ---- reading the frozen module -----------------------------------------------
: SLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64RAV-COVER throw then ;

: OP-AT ( IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR VW V-OPR VW KEY bk i IR-FUN:FOP@ ;

: OPCODE-AT ( IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   {: id:IR-ID:ir-op-id :}
   V-OPR VW KEY id IR-OP:FOPCODE@ ;

: OPERAND-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FOPERAND@ ;

: RESULT-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FRESULT@ ;

: VIEWS! ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 S-KEY !
   m IR-BUILD:FOP-POOL    V-OPP  S-VIEW !
   m IR-BUILD:FOP-ROWS    V-OPR  S-VIEW !
   m IR-BUILD:FVALUE-ROWS V-VALR S-VIEW !
   m IR-BUILD:FFUN-ROWS   V-FUNR S-VIEW !
   m IR-BUILD:FBLOCK-ROWS V-BLKR S-VIEW !
   m IR-BUILD:FSCHEMA-POOL V-SCHP S-VIEW !
   m IR-BUILD:FSCHEMA-ROWS V-SCHR S-VIEW ! ;

\ The straight-line subset, re-derived rather than taken on trust.
: BLOCK-OF ( -- IR-ID:ir-block-id )
   V-FUNR VW IR-FUN:FFUNS 1 <> if E-A64RAV-SHAPE throw then
   KEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   V-FUNR VW f IR-FUN:FBLOCK-COUNT 1 <> if E-A64RAV-SHAPE throw then
   V-FUNR VW V-BLKR VW KEY f 0 IR-FUN:FBLOCK@ ;

\ ---- what the module says about each value -----------------------------------
: NOTE-DEF ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SEEN-AT 0<> if E-A64RAV-COVER throw then
   1 k SEEN!
   pos k DEF!
   pos k LAST! ;

: NOTE-USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SEEN-AT 0= if E-A64RAV-COVER throw then
   pos k LAST! ;

: DEFS-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   V-OPR VW id IR-OP:FRESULTS {: n:n :}
   n 0 ?do id i RESULT-AT pos NOTE-DEF loop ;

: USES-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   V-OPR VW id IR-OP:FOPERANDS {: n:n :}
   n 0 ?do id i OPERAND-AT pos NOTE-USE loop ;

: MEASURE-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW bk IR-FUN:FARG-COUNT {: n:n :}
   n 0 ?do
      V-BLKR VW V-VALR VW KEY bk i IR-FUN:FARG@ ENTRY NOTE-DEF
   loop ;

: MEASURE ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   TABLES-CLEAR
   bk MEASURE-ARGS
   V-BLKR VW bk IR-FUN:FOP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id i USES-OF-OP
      id i DEFS-OF-OP
   loop ;

\ ---- the checks --------------------------------------------------------------
\ Every value of the module is a value of this block, the allocation covers
\ exactly those values, and the interval it recorded for each one is the interval
\ the module gives.
: COVER-CK ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n A64RA:VALUES <> if E-A64RAV-COVER throw then
   n VMAX > if E-A64RAV-COVER throw then
   n 0 ?do i SEEN-AT 0= if E-A64RAV-COVER throw then loop
   n N-VALS ! ;

: INTERVAL-CK ( -- )
   N-VALS @ 0 ?do
      i DEF-AT i A64RA:DEF@ <> if E-A64RAV-INTERVAL throw then
      i LAST-AT i A64RA:LAST@ <> if E-A64RAV-INTERVAL throw then
   loop ;

\ One register class: every value of this dialect is a general register, so a
\ value of any other type has been given a register that cannot hold it.
: CLASS-CK ( -- )
   A64RA:GPR-TYPE@ {: want:IR-ID:ir-type-id :}
   N-VALS @ 0 ?do
      V-VALR VW KEY  KEY i IR-ID:PACK-VALUE  IR-OP:FVALUE-TYPE@
      want SAME-TYPE? 0= if E-A64RAV-CLASS throw then
   loop ;

\ Every assigned register is one the routine's contract says it may destroy. The
\ contract cannot name x18, x30 or register 31 at all - A64EFF refuses them in
\ any general-register set - so a reserved register fails this check for the same
\ reason an unrelated callee-saved register does.
: REGISTER-CK ( -- )
   A64RA:POOL A64EFF:GPRS-N {: pool:n :}
   N-VALS @ 0 ?do
      i A64RA:CLAIM@ {: r:n :}
      r 0 < r A64EFF:FILE-SIZE >= or if E-A64RAV-REGISTER throw then
      pool 1 r lshift and 0= if E-A64RAV-REGISTER throw then
   loop ;

\ Are these two values ever live at the same instant? See the header: values
\ written at the same position always are, and otherwise the earlier one has to
\ be read for the last time at or before the later one is written.
: CLASH? ( n n -- bool )
   {: a:n b:n :}
   a DEF-AT b DEF-AT = if true exit then
   a DEF-AT b DEF-AT < if
      a LAST-AT b DEF-AT > exit
   then
   b LAST-AT a DEF-AT > ;

: OVERLAP-CK ( -- )
   N-VALS @ {: n:n :}
   n 0 ?do
      n i 1+ ?do
         j i CLASH? if
            j A64RA:CLAIM@ i A64RA:CLAIM@ = if E-A64RAV-OVERLAP throw then
         then
      loop
   loop ;

\ A form that names one register field twice - the move-wide overwrite keeps the
\ bits of its destination it does not write - declares that tie in its own
\ operation schema. Every declared tie is checked here on its own terms, read out
\ of the module's schema table rather than out of anything the allocator kept: an
\ assignment that gives a tied result and its operand two registers describes an
\ instruction the machine cannot execute.
: OP-TIE-CK ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW  id OPCODE-AT  i IR-SCHEMA:FTIE-RESULT@ {: rs:n :}
   V-SCHP VW V-SCHR VW  id OPCODE-AT  i IR-SCHEMA:FTIE-OPERAND@ {: op:n :}
   id op OPERAND-AT SLOT A64RA:CLAIM@
   id rs RESULT-AT SLOT A64RA:CLAIM@
   <> if E-A64RAV-TIE throw then ;

: TIE-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW bk IR-FUN:FOP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      V-SCHR VW id OPCODE-AT IR-SCHEMA:FTIES 0 ?do
         id i OP-TIE-CK
      loop
   loop ;

\ ---- what the acceptance is bound to -----------------------------------------
: STATE-CK ( -- )
   A64RA:SEALED? 0= if E-A64RAV-STATE throw then ;

: MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64RAV-MODULE throw then ;

\ The allocation depends on one fact of the contract: which general registers the
\ routine may destroy. A contract that names a different set is a different
\ allocation problem, and this one is not an answer to it.
: CONTRACT-CK ( A64EFF:gprs -- )
   A64RA:POOL A64EFF-GPRS:EQ 0= if E-A64RAV-CONTRACT throw then ;

\ An accepted answer is about one sealed walk. A later walk raises the
\ allocator's generation, so the acceptance stops answering rather than answering
\ about a walk nobody checked.
: FRESH-CK ( -- )
   ST @ ST-ACCEPTED <> if E-A64RAV-STATE throw then
   STATE-CK
   A64RA:GEN A-GEN @ <> if E-A64RAV-STATE throw then ;

public

\ ---- the check ---------------------------------------------------------------
\ Accept the sealed allocation as a true assignment for this module under this
\ routine contract, or refuse it by name. Nothing is answered until this returns.
: ACCEPT ( IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF:GPR-CLOBBER@ {: pool:A64EFF:gprs :}
   {: m:IR-BUILD:module :}
   ST-NONE ST !
   STATE-CK
   m MODULE-CK
   pool CONTRACT-CK
   m VIEWS!
   BLOCK-OF {: bk:IR-ID:ir-block-id :}
   bk MEASURE
   COVER-CK
   INTERVAL-CK
   CLASS-CK
   REGISTER-CK
   OVERLAP-CK
   bk TIE-CK
   A64RA:GEN A-GEN !
   ST-ACCEPTED ST ! ;

: ACCEPTED? ( -- bool )
   ST @ ST-ACCEPTED = ;

\ The register that holds this value. This is the only checked answer in the
\ chain: it exists only after ACCEPT has agreed with the module, and it stops
\ existing the moment a later allocation replaces the one that was accepted.
: REG@ ( n -- n )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   A64RA:CLAIM@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
