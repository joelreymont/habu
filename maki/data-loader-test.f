\ maki/data-loader-test.f - acceptance for the v0 text corpus loader
\ (maki/data-loader.f) end-to-end: write a small public-domain fixture to a temp
\ file, read it with the Habu-native reader, build+encode the char vocab, then draw
\ a seeded (B,T) batch with maki/batch-loader.f BL-LOAD and check the get_batch
\ contract on the ENCODED text - the RxC = B x T row shape, the target shifted one
\ token (y = x >> 1) within each contiguous window, determinism (same seed same
\ batch, different seed different), and the file rejects (missing -> E-FS-OPEN,
\ empty -> E-DL-EMPTY). DLT-prefixed (the maki suite shares one dictionary).

require lib/test.f
require lib/prelude.f
require lib/fs.f
require maki/data-loader.f
require maki/batch-loader.f

package MAKI

: DLT-TEXT         ( -- ptr u8 n )  s" To be, or not to be, that is the question:" ;
: DLT-PATH         ( -- ptr u8 n )  s" /tmp/habu-shakespeare-fixture.txt" ;
: DLT-EMPTY-PATH   ( -- ptr u8 n )  s" /tmp/habu-shakespeare-empty.txt" ;
: DLT-MISSING-PATH ( -- ptr u8 n )  s" /tmp/habu-shakespeare-absent.txt" ;

256 constant DLT-TCAP                    \ caller text byte buffer
create DLT-TEXTBUF DLT-TCAP allot
create DLT-CORPUS  DLT-TCAP cells allot  \ encoded ids (one cell per byte)
2 constant DLT-B                          \ sequences
4 constant DLT-T                          \ tokens per sequence
4 constant DLT-DIM                        \ embedding width (arena factor)
8 constant DLT-ROWS                       \ B*T
7  constant DLT-SEED
99 constant DLT-SEED2
create DLT-IDS0 DLT-ROWS cells allot      \ determinism snapshot: ids
create DLT-TGT0 DLT-ROWS cells allot      \ determinism snapshot: targets
variable DLT-N                            \ corpus token count

: DLT-WRITE-FIXTURE ( -- )  DLT-PATH DLT-TEXT WRITE-ALL ;
: DLT-WRITE-EMPTY   ( -- )  DLT-EMPTY-PATH DLT-TEXTBUF 0 WRITE-ALL ;
: DLT-LOAD ( -- )
   DLT-PATH DLT-TEXTBUF DLT-TCAP DLT-CORPUS DLT-TCAP DL-LOAD-CORPUS DLT-N ! ;
: DLT-BATCH ( n -- ) {: seed:n :}         \ draw a batch at the given seed
   DLT-CORPUS DLT-N @ DLT-B DLT-T DLT-DIM seed BL-LOAD ;

\ y = x >> 1 within each window: target[b*T+t] is the input one step ahead
\ (BL-IDS[b*T+t+1]) for t in [0,T-1) - proves contiguity + the one-token shift.
: DLT-SHIFT-OK? ( -- bool )
   DLT-B 0 ?do
      DLT-T 1- 0 ?do
         BL-TGT  j DLT-T * i +      T-GET
         BL-IDS  j DLT-T * i + 1+   T-GET  f= 0= if false unloop unloop exit then
      loop
   loop true ;

: DLT-SNAP ( -- )
   DLT-ROWS 0 ?do  BL-IDS i T-GET DLT-IDS0 i T-SET  BL-TGT i T-GET DLT-TGT0 i T-SET  loop ;

: DLT-EQ? ( ptr a ptr a n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET b i T-GET f= 0= if false unloop exit then  loop true ;

\ same seed reproduces the exact batch
: DLT-SAME-SEED? ( -- bool )
   DLT-SEED DLT-BATCH  DLT-SNAP
   DLT-SEED DLT-BATCH
   BL-IDS DLT-IDS0 DLT-ROWS DLT-EQ?
   BL-TGT DLT-TGT0 DLT-ROWS DLT-EQ?  and ;

\ a different seed yields a different batch
: DLT-DIFF-SEED? ( -- bool )
   DLT-SEED DLT-BATCH  DLT-SNAP
   DLT-SEED2 DLT-BATCH
   BL-IDS DLT-IDS0 DLT-ROWS DLT-EQ? 0= ;

: DLT-TRY-MISSING ( -- )
   DLT-MISSING-PATH DLT-TEXTBUF DLT-TCAP DLT-CORPUS DLT-TCAP DL-LOAD-CORPUS drop ;
: DLT-TRY-EMPTY ( -- )
   DLT-WRITE-EMPTY
   DLT-EMPTY-PATH DLT-TEXTBUF DLT-TCAP DLT-CORPUS DLT-TCAP DL-LOAD-CORPUS drop ;

T-RESET

DLT-WRITE-FIXTURE
DLT-LOAD

\ ---- corpus token count == fixture bytes -------------------------------------
DLT-N @  DLT-TEXT nip  T=

\ ---- get_batch shape (RxC = B x T) + one-token target shift ------------------
DLT-SEED DLT-BATCH
BL-ROWS DLT-ROWS T=
BL-T@   DLT-T    T=
DLT-SHIFT-OK? TTRUE

\ ---- determinism -------------------------------------------------------------
DLT-SAME-SEED? TTRUE
DLT-DIFF-SEED? TTRUE

\ ---- file rejects (fail closed) ----------------------------------------------
' DLT-TRY-MISSING E-FS-OPEN  TTHROWS
' DLT-TRY-EMPTY   E-DL-EMPTY TTHROWS

T-REPORT

;package
