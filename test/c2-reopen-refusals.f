\ c2-reopen-refusals.f - what code written inside C2-MEM still may not reach.
\
\ Run: the WHITEBOX-SUITE row c2-reopen-refusals (test/gate-stdlib-cases.f),
\ which hands it the unsealed engine (test/whitebox-engine.f).
\
\ Every product bakes C2-MEM (src/habu/native-runtime.f requires
\ lib/c2-owner.f), and the native build seals every package it bakes
\ (src/core/internal-mark.f SEAL-PACKAGES). So on a product `package C2-MEM`
\ exits 84 with the package name before the checker sees a body;
\ test/c2-memory-refusals.f and test/c2-owner-producer-refusals.f pin that on
\ the admitted C2 image. The checker rules below still bind code written inside
\ C2-MEM: its own source, and the whitebox image, which keeps engine packages
\ open. So these cases run there, moved from those two files with their labels,
\ programs and expected exit: each reopens C2-MEM in a disposable fork of the
\ whitebox engine (lib/test/subject.f), and the checker refuses the block, exit
\ 70. The image class is asserted first, because on a sealed engine every case
\ exits 84.

require lib/test.f
require lib/test/subject.f
require lib/c2-owner.f
require src/compiler/native/compiler.f

\ Rewriting a foldable loop must also copy the later C2 transfer's kind.
\ This uses the owner's real carrier and primitive; compilation is the probe.
1 set-tier
package C2-MEM
private
TRUSTED: STOW-WITH-LOOP ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] stow-layout | U -- R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U )
   0 10 0 ?do 1 + loop drop
   HEAD-FRAME c2-init-stow
   TASK:DEFER-LEAVE ;
;package
0 set-tier

package C2-REOPEN-REFUSALS

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

: SECTION-CLASS ( -- )
   s" the engine under test keeps engine packages open" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX T= ;

\ From test/c2-memory-refusals.f.
: SECTION-MEMORY ( -- )
   s" a reopened memory package cannot call the raw allocator" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-ALLOC ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U ) ALLOC-RUN ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot tick the raw allocator" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-ALLOC ( -- ) ['] ALLOC-RUN drop ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot export the raw allocator" T-LABEL
   s" package C2-MEM public EXPORT ALLOC-RUN ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot call the raw loan scope" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-LOAN ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U ) LOAN-RUN ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot tick the raw loan scope" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-LOAN ( -- ) ['] LOAN-RUN drop ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot export the raw loan scope" T-LABEL
   s" package C2-MEM public EXPORT LOAN-RUN ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot call runtime frame lookup" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-ROOT ( ptr u8 -- ptr n ) ROOT-FRAME ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot tick runtime frame lookup" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-ROOT ( -- ) ['] ROOT-FRAME drop ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot export runtime frame lookup" T-LABEL
   s" package C2-MEM public EXPORT ROOT-FRAME ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot call runtime append" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-APPEND ( ptr n NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- ptr u8 n ) APPEND ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot tick runtime append" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-APPEND ( -- ) ['] APPEND drop ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot export runtime append" T-LABEL
   s" package C2-MEM public EXPORT APPEND ;package" 70 STATUS? TTRUE ;

\ From test/c2-owner-producer-refusals.f.
: SECTION-PRODUCER ( -- )
   s" the raw publisher cannot be exported" T-LABEL
   s" package C2-MEM public EXPORT PUBLISH-RAW ;package" 70 STATUS? TTRUE
   s" a private unpacker cannot be exported by reopening its package" T-LABEL
   s" package C2-MEM public EXPORT MUT-UNPACK ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot call root frame lookup" T-LABEL
   s" package C2-MEM private : C2OP-ROOT ( ptr u8 -- ptr n ) ROOT-FRAME ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot tick root frame lookup" T-LABEL
   s" package C2-MEM private : C2OP-TICK-ROOT ( -- ) ['] ROOT-FRAME drop ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot export root frame lookup" T-LABEL
   s" package C2-MEM public EXPORT ROOT-FRAME ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot call append registration" T-LABEL
   s" package C2-MEM private : C2OP-APPEND ( ptr n NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- ptr u8 n ) APPEND ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot export append registration" T-LABEL
   s" package C2-MEM public EXPORT APPEND ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot expose a frame storage accessor" T-LABEL
   s" package C2-MEM public EXPORT APPEND-HEAD-CELL ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot expose the task-local owner region" T-LABEL
   s" package C2-MEM public EXPORT OWNER-REGION ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot expose the owner frame lookup" T-LABEL
   s" package C2-MEM public EXPORT OWNER-FRAME ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot expose the old raw allocation helper" T-LABEL
   s" package C2-MEM public EXPORT ALLOC-RAW ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot expose the old append adapter" T-LABEL
   s" package C2-MEM public EXPORT ALLOC-ON ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot expose root registration" T-LABEL
   s" package C2-MEM public EXPORT ACQUIRE-BYTES ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot call the raw scoped allocator" T-LABEL
   s" package C2-MEM private : C2OP-ALLOC-RUN ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U ) ALLOC-RUN ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot tick the raw scoped allocator" T-LABEL
   s" package C2-MEM private : C2OP-TICK-ALLOC-RUN ( -- ) ['] ALLOC-RUN drop ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot export the raw scoped allocator" T-LABEL
   s" package C2-MEM public EXPORT ALLOC-RUN ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot call the raw loan scope" T-LABEL
   s" package C2-MEM private : C2OP-LOAN-RUN ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U ) LOAN-RUN ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot tick the raw loan scope" T-LABEL
   s" package C2-MEM private : C2OP-TICK-LOAN-RUN ( -- ) ['] LOAN-RUN drop ; ;package" 70 STATUS? TTRUE
   s" reopening C2-MEM cannot export the raw loan scope" T-LABEL
   s" package C2-MEM public EXPORT LOAN-RUN ;package" 70 STATUS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   SECTION-CLASS
   SECTION-MEMORY
   SECTION-PRODUCER
   T-REPORT ;

;package

C2-REOPEN-REFUSALS:RUN
