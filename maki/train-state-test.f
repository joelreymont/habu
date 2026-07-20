\ maki/train-state-test.f - checked tests for the training-state checkpoint codec.
\ Round-trip (save -> zero -> load restores every cell bit-for-bit), determinism
\ (save -> load -> save is byte-identical), and every named fail-closed throw:
\ missing file, a flipped byte (digest), a truncated file, a wrong-shape schema,
\ an over-capacity segment, plus the ATOMICITY guarantee (a failed load leaves the
\ registered buffers untouched - nothing partially loaded). Synthetic segments (a
\ 5-cell buffer + a scalar) exercise the codec in isolation; the trainer-level
\ resume-equivalence proof lives in maki/adam-train-test.f. Writes only under the
\ store root; STORE-RESET keeps the tree from leaking.

require lib/test.f
require maki/train-state.f

package MAKI

\ ---- synthetic segments: one 5-cell buffer + one scalar cell -----------------
5 constant TSU-AN
create TSU-A TSU-AN cells allot
variable TSU-S

$2000 constant TSU-FCAP
create TSU-F1 TSU-FCAP allot  variable TSU-F1U
create TSU-F2 TSU-FCAP allot  variable TSU-F2U

: TSU-SEGS ( -- )        TSC-BEGIN  TSU-A TSU-AN TSC-SEG  TSU-S 1 TSC-SEG ;
: TSU-SEGS-SHORT ( -- )  TSC-BEGIN  TSU-A TSU-AN TSC-SEG ;        \ total 5, not the saved 6

: TSU-FILL ( -- )        \ distinct raw-cell values (the codec is representation-agnostic)
   11 TSU-A 0 cells + !   22 TSU-A 1 cells + !   33 TSU-A 2 cells + !
   44 TSU-A 3 cells + !   55 TSU-A 4 cells + !   99 TSU-S ! ;
: TSU-ZERO ( -- )
   TSU-AN 0 ?do  0 TSU-A i cells + !  loop  0 TSU-S ! ;

\ ---- file byte helpers (read a checkpoint, mutate it, write it back) ---------
: TSU-READ1 ( ptr u8 n -- ) {: na:ptr nu:n :}  na nu TSC-PATH$ TSU-F1 TSU-FCAP READ-ALL TSU-F1U ! ;
: TSU-READ2 ( ptr u8 n -- ) {: na:ptr nu:n :}  na nu TSC-PATH$ TSU-F2 TSU-FCAP READ-ALL TSU-F2U ! ;
: TSU-WRITE1 ( ptr u8 n n -- ) {: na:ptr nu:n u:n :}  na nu TSC-PATH$ TSU-F1 u WRITE-ALL ;
: TSU-FLIP24 ( -- )      \ flip one payload byte (byte 24 = first payload cell, after the 3-cell header)
   TSU-F1 24 + c@ 1 xor  TSU-F1 24 + c! ;
: TSU-BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   au bu <> if false exit then
   au 0 ?do  a i + c@ b i + c@ <> if false unloop exit then  loop true ;

\ ---- throw-path wrappers -----------------------------------------------------
: TSU-LOAD-MISSING ( -- )  TSU-SEGS s" nope"  TSC-LOAD ;
: TSU-LOAD-FLIP    ( -- )  TSU-SEGS s" flip"  TSC-LOAD ;
: TSU-LOAD-TRUNC   ( -- )  TSU-SEGS s" trunc" TSC-LOAD ;
: TSU-LOAD-MAGIC   ( -- )  TSU-SEGS s" magic" TSC-LOAD ;
: TSU-LOAD-SHAPE   ( -- )  TSU-SEGS-SHORT s" shape" TSC-LOAD ;
: TSU-CAP-SEG      ( -- )  TSC-BEGIN  TSU-A 5000 TSC-SEG ;        \ 5000 > TSC-PAY-MAX (4096)
: TSU-LOAD-ATOM    ( -- )  TSU-SEGS s" atom"  TSC-LOAD ;

T-RESET
STORE-RESET

\ ---- round-trip: save -> zero -> load restores every cell bit-for-bit --------
TSU-FILL  TSU-SEGS s" rt" TSC-SAVE
TSU-ZERO  TSU-SEGS s" rt" TSC-LOAD
TSU-A 0 cells + @ 11 T=
TSU-A 1 cells + @ 22 T=
TSU-A 2 cells + @ 33 T=
TSU-A 3 cells + @ 44 T=
TSU-A 4 cells + @ 55 T=
TSU-S @ 99 T=

\ ---- determinism: save -> load -> save is byte-identical ---------------------
TSU-FILL  TSU-SEGS s" det" TSC-SAVE
s" det" TSU-READ1                       \ F1 = first serialization
TSU-SEGS s" det" TSC-LOAD
TSU-SEGS s" det" TSC-SAVE
s" det" TSU-READ2                       \ F2 = re-serialization after a load
TSU-F1 TSU-F1U @ TSU-F2 TSU-F2U @ TSU-BYTES= TTRUE

\ ---- fail closed: missing file (never saved) ---------------------------------
' TSU-LOAD-MISSING E-TSC-MISSING TTHROWS

\ ---- fail closed: a flipped payload byte is a digest mismatch ----------------
TSU-FILL  TSU-SEGS s" flip" TSC-SAVE
s" flip" TSU-READ1  TSU-FLIP24  s" flip" TSU-F1U @ TSU-WRITE1
' TSU-LOAD-FLIP E-TSC-DIGEST TTHROWS

\ ---- fail closed: a corrupted magic cell is not a checkpoint -----------------
TSU-FILL  TSU-SEGS s" magic" TSC-SAVE
s" magic" TSU-READ1  TSU-F1 c@ 1 xor TSU-F1 c!  s" magic" TSU-F1U @ TSU-WRITE1
' TSU-LOAD-MAGIC E-TSC-MAGIC TTHROWS

\ ---- fail closed: a truncated file (digest cell dropped) ---------------------
TSU-FILL  TSU-SEGS s" trunc" TSC-SAVE
s" trunc" TSU-READ1  s" trunc" TSU-F1U @ 8 - TSU-WRITE1
' TSU-LOAD-TRUNC E-TSC-TRUNC TTHROWS

\ ---- fail closed: a wrong-shape schema (intact file, fewer registered cells) -
TSU-FILL  TSU-SEGS s" shape" TSC-SAVE
' TSU-LOAD-SHAPE E-TSC-SHAPE TTHROWS

\ ---- fail closed: an over-capacity segment ----------------------------------
' TSU-CAP-SEG E-TSC-CAP TTHROWS

\ ---- atomicity: a failed load leaves the registered buffers untouched --------
TSU-FILL  TSU-SEGS s" atom" TSC-SAVE
s" atom" TSU-READ1  TSU-FLIP24  s" atom" TSU-F1U @ TSU-WRITE1
TSU-ZERO                                \ live state = zeros; a good load would overwrite with 11..55,99
' TSU-LOAD-ATOM E-TSC-DIGEST TTHROWS
TSU-A 0 cells + @ 0 T=                  \ still zero: nothing was partially loaded
TSU-A 4 cells + @ 0 T=
TSU-S @ 0 T=

STORE-RESET
T-REPORT

;package
