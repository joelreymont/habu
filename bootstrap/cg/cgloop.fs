\ cgloop.fs — register-resident DO..LOOP mechanism. A straight-line loop body
\ keeps its loop-carried values in registers across the back-edge instead of
\ round-tripping memory every iteration. This file is the pure mechanism:
\   carry-snap   — pin the live VS into fixed register homes at loop entry
\   carry-recon  — parallel-move the carry-out registers back into those homes
\   carry-restore— rebuild the VS from the homes after the loop
\   cg-snapshot/cg-rollback — speculative compile + clean undo when a candidate
\                  loop turns out not to be register-eligible (depth mismatch)
\ walk.fs decides eligibility (straight-line pre-scan) and drives the body.

require regstack.fs

CHECKING-ON? @  CHECKING-ON? off          \ IR mutation / bit math / loops = unchecked

\ Force VS entry i to a GP register (CON -> LIT, FREG -> FMOVDX), rewriting the
\ entry in place; return the register. Idempotent on an already-REG entry.
: vs-force ( i -- r )
   {: i :}  VTAG i cells + @ {: t :}  VVAL i cells + @ {: v :}
   t V-REG = if  v exit  then
   r-alloc {: r :}
   t V-FREG = if  r v FMOVDX,  v d-free  else  r v LIT64,  then
   V-REG i cells VTAG + !  r i cells VVAL + !  r ;

\ --- carry homes: the registers the loop body expects its carry in at ltop ---
create CARRY-R 16 cells allot   variable CARRY-N

: carry-snap ( -- )                       \ all live VS entries -> regs, record homes
   VSP @ dup 16 > if 1 abort" cg: loop carry too deep for register residency" then
   dup CARRY-N !  0 ?do  i vs-force  CARRY-R i cells + !  loop ;

\ --- parallel register move: src[i] -> dst[i], cycle-safe via T0 scratch ---
create PM-SRC 16 cells allot   create PM-DST 16 cells allot
create PM-DONE 16 cells allot  variable PM-N

: pm-noop? ( i -- f )  cells {: o :}  PM-SRC o + @  PM-DST o + @ = ;

: pm-ready? ( i -- f )                    \ pending, non-noop, dst read by no pending move
   dup cells PM-DONE + @ if  drop false exit then
   dup pm-noop? if  drop false exit then
   cells PM-DST + @ {: d :}  true
   PM-N @ 0 ?do  PM-DONE i cells + @ 0= if
      PM-SRC i cells + @ d = if  drop false  leave then  then  loop ;

: pm-find ( -- i|-1 )  PM-N @ 0 ?do  i pm-ready? if  i unloop exit then  loop  -1 ;

: pm-rem  ( -- n )     0 PM-N @ 0 ?do  PM-DONE i cells + @ 0= if 1+ then  loop ;

: pm-emit ( i -- )  >r  PM-DST r@ cells + @  PM-SRC r@ cells + @  MOV,  1 PM-DONE r> cells + ! ;

: pm-break ( -- )                         \ redirect a pending move's src through T0
   PM-N @ 0 ?do  PM-DONE i cells + @ 0= if
      T0 PM-SRC i cells + @ MOV,  T0 PM-SRC i cells + !  unloop exit then  loop ;

: pm-run ( -- )
   PM-N @ 0 ?do  i pm-noop? if 1 else 0 then  PM-DONE i cells + !  loop
   begin  pm-rem 0>  while
      pm-find dup 0>= if  pm-emit  else  drop  pm-break  then
   repeat ;

\ Reconcile carry-out (current VS) into the carry homes. RL-FAIL (set when the body
\ touched memory below the carry) or a net depth change means this was never a
\ register loop; signal it (no parallel move) and walk.fs rolls back to memory.
\ Flag, don't throw: gforth 0.7.9 faults unwinding `throw` across emit-rloop's locals.
: carry-recon ( -- )
   VSP @ CARRY-N @ <> if  RL-FAIL on  then
   RL-FAIL @ if exit then
   CARRY-N @ 0 ?do  i vs-force  PM-SRC i cells + !  CARRY-R i cells + @  PM-DST i cells + !  loop
   CARRY-N @ PM-N !  pm-run ;

\ After the loop the live values are in the carry homes; rebuild the VS to match
\ and fix the pool free-state so only the homes are allocated.
: r-take ( r -- )  #RPOOL 0 ?do  RPOOL i cells + @ over = if  0 RFREE i cells + !  then  loop  drop ;

: carry-restore ( -- )
   rp-reset  dp-reset  0 VSP !
   CARRY-N @ 0 ?do  CARRY-R i cells + @  dup r-take  v-pushr  loop ;

\ --- speculative compile / rollback (undo a register-loop attempt cleanly) ---
create VS-STAG VMAX cells allot   create VS-SVAL VMAX cells allot   variable VS-SSP
create RF-SAVE #RPOOL cells allot  create DF-SAVE #DPOOL cells allot
variable ICSV  variable LBLSV  variable LDSV  variable CFSV

: cg-snapshot ( -- )
   #IC @ ICSV !  #LBL @ LBLSV !  LOOP-DEPTH @ LDSV !  CF-SP @ CFSV !
   VSP @ VS-SSP !
   VSP @ 0 ?do  VTAG i cells + @ VS-STAG i cells + !  VVAL i cells + @ VS-SVAL i cells + !  loop
   #RPOOL 0 ?do  RFREE i cells + @ RF-SAVE i cells + !  loop
   #DPOOL 0 ?do  DFREE i cells + @ DF-SAVE i cells + !  loop ;

: cg-rollback ( -- )
   ICSV @ #IC !  LBLSV @ #LBL !  LDSV @ LOOP-DEPTH !  CFSV @ CF-SP !
   VS-SSP @ VSP !
   VSP @ 0 ?do  VS-STAG i cells + @ VTAG i cells + !  VS-SVAL i cells + @ VVAL i cells + !  loop
   #RPOOL 0 ?do  RF-SAVE i cells + @ RFREE i cells + !  loop
   #DPOOL 0 ?do  DF-SAVE i cells + @ DFREE i cells + !  loop ;

CHECKING-ON? !
