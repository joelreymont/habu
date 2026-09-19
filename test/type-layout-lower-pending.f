\ type-layout-lower-pending.f — TFAM 12 width-aware lowering suite
\ (habu-tfam-12-layout, docs/type-families.md §17-18). Run BY THE ENGINE over
\ stdin, standalone or through the native registry:
\     bin/hb < test/type-layout-lower-pending.f
\ Three sections:
\   1. width-fact contracts — every TLP-* subject is a REAL checked definition;
\      the asserts after each pin the per-op fact the emitter consumes
\      (operand position 0=top, family-id, registry logical width; absent row
\      = one-cell operand).
\   2. emitted-lowering goldens — exact u32 instruction sequences of six
\      representative subjects, captured FROM the implemented pass-2 emitter
\      (EM-COMPILE-P2WIDE / LP2COPY / LP2ROT / LP2RS / EM-P2-CARVE, habu2.f).
\      The fetch validator's ASLR-dependent absolute call target is opcode-mask
\      checked; every other word is exact and moves only when lowering does.
\   3. execution rows — generated constructors (TLP--RES:ERR / TLP--MIX:BIG)
\      seed the bundles in checked code, whole-bundle transports run at
\      RUNTIME, and TRUSTED unpackers surface the cells for value asserts:
\      dup/swap/over/nip/tuck/rot/-rot/2dup/2drop/2swap/2over, the
\      return-stack transfers, and wide locals.
\
\ Constant contract (TFAM 12 verdict 2026-07-09): `constant` keeps the one-cell
\ `-- a` model permanently — checked-body layout pops reject (TD12-CONST), the
\ top-level pop never sees a wide value (DNAME-WIDE dispatch gate), and the
\ staged shape-carry fixtures were deleted as unsound (a one-cell bake cannot
\ carry a multi-cell shape). Parity: check-all-errors-test const-layout-narrow.

using TFAM

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

\ ---------------------------------------------------------------------------
\ layout families under test: width 2 (1 payload slot + tag), width 4
\ (3 payload slots + tag), and a zero-payload enum-shaped sum (width 1).
\ ---------------------------------------------------------------------------
SUMTYPE tlp-res 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
SUMTYPE tlp-mix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
SUMTYPE tlp-en 0
  VARIANT lit  ;VARIANT
  VARIANT dark ;VARIANT
;SUMTYPE
\ width 2 with NO family parameters: the shape a local annotation can name.
STRUCTURE tlp-pair 0
  FIELD left n
  FIELD right n
;STRUCTURE

variable TLOK   variable TLF   variable TLX   variable TLPP
\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): the internal registry
\ probe goes through a named trusted shim.
TRUSTED: TWX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
s" " s" tlp-res" TWX-TFAM-FIND-IN TLOK ! TLF !
TLOK @ -1 T=
TLF @ TFAM-WIDTH@ 2 T=
s" " s" tlp-mix" TWX-TFAM-FIND-IN TLOK ! TLX !
TLOK @ -1 T=
TLX @ TFAM-WIDTH@ 4 T=
s" " s" tlp-pair" TWX-TFAM-FIND-IN TLOK ! TLPP !
TLOK @ -1 T=
TLPP @ TFAM-WIDTH@ 2 T=

\ ---------------------------------------------------------------------------
\ transport matrix on the width-2 family. Each subject is a real checked
\ definition; reaching the next line proves certification (a reject kills the
\ load), and the width facts of ITS check are read immediately after. Facts
\ are per-CHECK scratch, so each assert block runs before the next definition.
\ ---------------------------------------------------------------------------
: TLP-DUP ( tlp-res<n,n> -- tlp-res<n,n> tlp-res<n,n> ) dup ;
WF-N@ 1 T=  0 WF-POS@ 0 T=  0 WF-FAM@ TLF @ T=  0 WF-WIDTH@ 2 T=
: TLP-DROP ( tlp-res<n,n> -- ) drop ;
WF-N@ 1 T=  0 WF-POS@ 0 T=
: TLP-SWAP ( tlp-res<n,n> n -- n tlp-res<n,n> ) swap ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-WIDTH@ 2 T=
: TLP-OVER ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> ) over ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-NIP ( tlp-res<n,n> n -- n ) nip ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-TUCK ( n tlp-res<n,n> -- tlp-res<n,n> n tlp-res<n,n> ) tuck ;
WF-N@ 1 T=  0 WF-POS@ 0 T=
: TLP-ROT ( tlp-res<n,n> n n -- n n tlp-res<n,n> ) rot ;
WF-N@ 1 T=  0 WF-POS@ 2 T=
: TLP-MROT ( n n tlp-res<n,n> -- tlp-res<n,n> n n ) -rot ;
WF-N@ 1 T=  0 WF-POS@ 0 T=
: TLP-2DUP ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> n ) 2dup ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-2DROP ( tlp-res<n,n> n -- ) 2drop ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-2SWAP ( tlp-res<n,n> n n n -- n n tlp-res<n,n> n ) 2swap ;
WF-N@ 1 T=  0 WF-POS@ 3 T=
: TLP-2OVER ( tlp-res<n,n> n n n -- tlp-res<n,n> n n n tlp-res<n,n> n ) 2over ;
WF-N@ 1 T=  0 WF-POS@ 3 T=

\ return-stack transfers: one fact per op, from the row the op consumes.
: TLP-TOR ( tlp-res<n,n> -- tlp-res<n,n> ) >r r> ;
WF-N@ 2 T=  0 WF-OFF@ 41 T=  1 WF-OFF@ 44 T=  1 WF-POS@ 0 T=
: TLP-RAT ( tlp-res<n,n> -- tlp-res<n,n> tlp-res<n,n> ) >r r@ r> ;
WF-N@ 3 T=  1 WF-OFF@ 57 T=  2 WF-OFF@ 60 T=
: TLP-2TOR ( tlp-res<n,n> n -- tlp-res<n,n> n ) 2>r 2r> ;
WF-N@ 2 T=  0 WF-POS@ 1 T=  1 WF-POS@ 1 T=
: TLP-2RAT ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> n ) 2>r 2r@ 2r> ;
WF-N@ 3 T=  0 WF-POS@ 1 T=  1 WF-POS@ 1 T=  2 WF-POS@ 1 T=

\ locals capture: the whole group records at the :} token. `x` binds the layout
\ value untyped, so the entry effect carries the detailed type.
: TLP-LOCAL ( tlp-res<n,n> n -- n ) {: x y:n :} y ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-FAM@ TLF @ T=  0 WF-WIDTH@ 2 T=

\ the TYPED twin: a PARAMETRIC spelling is an annotation the local parser reads
\ (dot habu-parse-local-annotations — it shares SIG-TYPE's grammar), and it
\ records the same width fact as the untyped bind above. Naming the family does
\ not change the capture: one fact, operand position 1, family tlp-res, width 2.
: TLP-TYPED-RES-LOCAL ( tlp-res<n,n> n -- n ) {: x:tlp-res<n,n> y:n :} y ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-FAM@ TLF @ T=  0 WF-WIDTH@ 2 T=

\ An arity-0 W=2 family IS nameable: the annotation records the layout's top
\ hidden term and the bind records width 2. The capture at :} is the only
\ width fact of the definition — the reference's reload reads the local's own
\ recorded width (LOCW / LOCW-HW@), not a per-op fact, exactly as the untyped
\ TLP-LOCAL above does.
: TLP-TYPED-WIDE-LOCAL ( tlp-pair -- tlp-pair ) {: pair:tlp-pair :} pair ;
WF-N@ 1 T=  0 WF-POS@ 0 T=  0 WF-FAM@ TLPP @ T=  0 WF-WIDTH@ 2 T=

\ ---------------------------------------------------------------------------
\ width-4 spot checks and multi-fact ordering: facts scan top position first.
\ ---------------------------------------------------------------------------
: TLP-MIX-DUP ( tlp-mix<n,n> -- tlp-mix<n,n> tlp-mix<n,n> ) dup ;
WF-N@ 1 T=  0 WF-FAM@ TLX @ T=  0 WF-WIDTH@ 4 T=
: TLP-MIX-SWAP ( tlp-mix<n,n> n -- n tlp-mix<n,n> ) swap ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-WIDTH@ 4 T=
: TLP-DUAL-2SWAP ( tlp-res<n,n> n tlp-mix<n,n> n -- tlp-mix<n,n> n tlp-res<n,n> n ) 2swap ;
WF-N@ 2 T=
0 WF-POS@ 1 T=  0 WF-FAM@ TLX @ T=  0 WF-WIDTH@ 4 T=
1 WF-POS@ 3 T=  1 WF-FAM@ TLF @ T=  1 WF-WIDTH@ 2 T=

\ a zero-payload enum-shaped sum is width 1: emitters keep one-cell lowering.
: TLP-EN-DUP ( tlp-en -- tlp-en tlp-en ) dup ;
WF-N@ 1 T=  0 WF-WIDTH@ 1 T=

\ Layout memory operations record one operation-width fact at position 0.
\ W=2 therefore triggers pass 2; the scalar address itself remains one cell.
1 LAYOUT-BUFFER TLP-MEM2-BUF tlp-res<n,n>
: TLP-MEM2-P ( -- ptr tlp-res<n,n> ) 0 TLP-MEM2-BUF ;
: TLP-STORE2 ( tlp-res<n,n> -- ) TLP-MEM2-P ! ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 2 T=
: TLP-FETCH2 ( -- tlp-res<n,n> ) TLP-MEM2-P @ ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 2 T=
1 LAYOUT-BUFFER TLP-MEM4-BUF tlp-mix<n,n>
: TLP-MEM4-P ( -- ptr tlp-mix<n,n> ) 0 TLP-MEM4-BUF ;
: TLP-STORE4 ( tlp-mix<n,n> -- ) TLP-MEM4-P ! ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 4 T=
: TLP-FETCH4 ( -- tlp-mix<n,n> ) TLP-MEM4-P @ ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 4 T=

\ Address-input subjects keep the memory lowering golden free of embedded
\ CREATE addresses. Their only body token is the wide memory operation.
: TLP-STORE2-G ( tlp-res<n,n> ptr tlp-res<n,n> -- ) ! ;
WF-N@ 1 T=  0 WF-OFF@ 50 T=  0 WF-WIDTH@ 2 T=
: TLP-FETCH2-G ( ptr tlp-res<n,n> -- tlp-res<n,n> ) @ ;
WF-N@ 1 T=  0 WF-OFF@ 50 T=  0 WF-WIDTH@ 2 T=

\ ---------------------------------------------------------------------------
\ emitted-lowering goldens: exact u32 sequences of five subjects, captured
\ from the implemented pass-2 emitter. Layout invariants pinned here:
\   TLP-DUP     w2 dup     = spill-free copy loop (movz #2; src = top-2 cells)
\   TLP-SWAP    w2-under-scalar swap = triple in-place reversal of the top 3
\               cells (rotate the bottom 2-cell group to the top)
\   TLP-MIX-DUP w4 dup     = the same copy loop shape with #4/top-4
\   TLP-TOR     w2 >r r>   = 2-cell block moves through the return stack
\   TLP-LOCAL   w2 bundle local = 3-cell frame (align16 -> 32 bytes), capture
\               pops tag-first into slots 3,2 (bundle) and 1 (scalar), the
\               reference reloads slot 1
\ TLP-TOR pins the GUARD-PAGE return stack as of 89f2ff3b ("Map every VM stack
\ with guard pages"): the band left the DATA header for its own mapping, so its
\ base is read from STACK-ABI:RETURN-BASE-CELL and a slot is base + depth*8,
\ where the block address used to be one `add x11,x20,x10,lsl#3` and the
\ transfer carried an RSTK-OFF displacement.
\ ---------------------------------------------------------------------------
\ TLP-W32: read one emitted instruction word out of a compiled subject.
\ Tested boundary (TRUSTED): reinterprets an xt as the byte base for the four
\ c@ loads of one u32 — test-only code introspection, the same class as the
\ imgdump/jitdump readers; every use sits directly under the golden asserts.
\ Retirement owner: habu-interpret-wide-gate-1d70acf7.
TRUSTED: TLP-W32 ( n n -- n )
   + dup c@ over 1 + c@ 8 lshift or over 2 + c@ 16 lshift or swap 3 + c@ 24 lshift or ;

\ Tested boundary: the golden subjects carry wide effects, so their
\ dict records are DNAME-WIDE (habu-tfam-12-interpret) and interpret `'`
\ correctly fails closed on them. The goldens only READ code bytes, so the xt
\ comes from the raw-xt introspection boundary (search-wl, wordlist 0) — the
\ same test-only class as TLP-W32, checked because `search-wl` carries a row.
\ Retirement owner: habu-interpret-wide-gate-1d70acf7.
: TLP-XT ( ptr u8 n -- n ) 0 search-wl ;

variable GXT
: GG ( n n -- ) {: ix:n want:n :}   \ golden: instruction ix of subject GXT
   GXT @ ix 4 * TLP-W32  want T= ;

: GM ( n n n -- ) {: ix:n mask:n want:n :}   \ masked golden instruction
   GXT @ ix 4 * TLP-W32 mask and  want T= ;

\ The engine helper call is one direct `BL imm26` when the JIT region maps within BL
\ range of __text (native bin/hb, dot habu-aot-repl-bl), and the absolute movz/movk/movk
\ x16 + blr x16 chain when the region is far (the Gforth stage0 seed). Verify whichever
\ form is present and leave GN = the instruction index right after the call.
variable GN
: GCALL ( n -- ) {: ix:n :}
   GXT @ ix 4 * TLP-W32 $FC000000 and $94000000 = if
      ix $FC000000 $94000000 GM  ix 1 + GN !  exit
   then
   ix     $FFE0001F $D2800010 GM
   ix 1 + $FFE0001F $F2A00010 GM
   ix 2 + $FFE0001F $F2C00010 GM
   ix 3 + $FFFFFFFF $D63F0200 GM
   ix 4 + GN ! ;

: TLP-EXIT-W32 ( -- n )
   HB-TARGET-LINUX? if $D2800BC8 exit then
   HB-TARGET-MACOS? if $D2800030 exit then
   s" type-layout-lower: unknown target" 76 die ;
: TLP-SVC-W32 ( -- n )
   HB-TARGET-LINUX? if $D4000001 exit then
   HB-TARGET-MACOS? if $D4001001 exit then
   s" type-layout-lower: unknown target" 76 die ;

s" TLP-DUP" TLP-XT GXT !
0 $D503201F GG                                          \ entry slot: a nop, the copy loop calls nothing
1 $D2800049 GG  2 $D100426A GG                          \ movz x9,#2 ; sub x10,x19,#16
3 $F940014B GG  4 $9100214A GG                          \ copy loop: ldr x11,[x10] ; add x10,x10,#8
5 $F800866B GG                                          \ str x11,[x19],#8   the push, one instruction
6 $F1000529 GG  7 $54FFFF81 GG                          \ subs x9,#1 ; b.ne -4 (the loop is a word shorter)
8 $D65F03C0 GG                                          \ ret, with no frame to give back

s" TLP-SWAP" TLP-XT GXT !
0 $D503201F GG                                          \ entry slot: a nop, the reversals call nothing
1 $D100626A GG  2 $D100426B GG                          \ rev1: [top-24, top-16]
3 $EB0B015F GG  4 $54000102 GG  5 $F940014C GG  6 $F940016D GG
7 $F900014D GG  8 $F900016C GG  9 $9100214A GG  10 $D100216B GG  11 $17FFFFF8 GG
12 $D100226A GG  13 $D100226B GG                        \ rev2: [top-8, top-8]
14 $EB0B015F GG  15 $54000102 GG  16 $F940014C GG  17 $F940016D GG
18 $F900014D GG  19 $F900016C GG  20 $9100214A GG  21 $D100216B GG  22 $17FFFFF8 GG
23 $D100626A GG  24 $D100226B GG                        \ rev3: whole 3-cell span
25 $EB0B015F GG  26 $54000102 GG  27 $F940014C GG  28 $F940016D GG
29 $F900014D GG  30 $F900016C GG  31 $9100214A GG  32 $D100216B GG  33 $17FFFFF8 GG
34 $D65F03C0 GG                                         \ ret. The reversal loops are untouched: they
                                                        \ address x10/x11, never the data-stack pointer.

s" TLP-MIX-DUP" TLP-XT GXT !
0 $D503201F GG
1 $D2800089 GG  2 $D100826A GG                          \ movz x9,#4 ; sub x10,x19,#32
3 $F940014B GG  4 $9100214A GG  5 $F800866B GG
6 $F1000529 GG  7 $54FFFF81 GG
8 $D65F03C0 GG

s" TLP-TOR" TLP-XT GXT !
0 $D503201F GG                                          \ entry slot: a nop, the block moves call nothing
1 $F942B68A GG                                          \ >r: ldr x10,[x20,#$568]  depth
2 $F964028B GG  3 $8B0A0D6B GG                          \ ldr x11,[x20,#$4800] base ; add x11,x11,x10,lsl#3
4 $D100426C GG  5 $D2800049 GG                          \ sub x12,x19,#16 (src = top-2 cells) ; movz x9,#2
6 $F940018D GG  7 $9100218C GG  8 $F900016D GG          \ data->rstk loop: ldr x13,[x12] ; add x12,x12,#8 ; str x13,[x11]
9 $9100216B GG  10 $F1000529 GG  11 $54FFFF61 GG        \ add x11,x11,#8 ; subs x9,x9,#1 ; b.ne -5
                                                        \ ...still -5: that loop stores through x11, so it
                                                        \ keeps its own separate pointer bump.
12 $D1004273 GG  13 $9100094A GG  14 $F902B68A GG       \ sub x19,x19,#16 ; add x10,x10,#2 ; str x10,[x20,#$568]
15 $F942B68A GG  16 $D100094A GG                        \ r>: ldr x10,[x20,#$568] ; sub x10,x10,#2
17 $F964028B GG  18 $8B0A0D6B GG                        \ ldr x11,[x20,#$4800] base ; add x11,x11,x10,lsl#3
19 $D2800049 GG                                         \ movz x9,#2
20 $F940016D GG  21 $9100216B GG                        \ rstk->data loop: ldr x13,[x11] ; add x11,x11,#8
22 $F800866D GG                                         \ str x13,[x19],#8   the push, one instruction
23 $F1000529 GG  24 $54FFFF81 GG                        \ subs x9,x9,#1 ; b.ne -4 (this loop IS a word shorter)
25 $F902B68A GG                                         \ commit rsp
26 $D65F03C0 GG                                         \ ret

s" TLP-LOCAL" TLP-XT GXT !
0 $D503201F GG                                          \ entry slot: a nop, the body calls nothing
1 $D10083FF GG                                          \ sub sp,sp,#32 (3 cells + pad) -- the LOCALS frame,
                                                        \ which is not the link frame and does not move
2 $F85F8E69 GG  3 $F90007E9 GG                          \ pop y -> slot 1: ldr x9,[x19,#-8]! ; str x9,[sp,#8]
4 $F85F8E69 GG  5 $F9000FE9 GG                          \ pop x tag -> slot 3
6 $F85F8E69 GG  7 $F9000BE9 GG                          \ pop x slot0 -> slot 2
8 $F94007E9 GG  9 $F8008669 GG                          \ ref y: ldr x9,[sp,#8] ; str x9,[x19],#8
10 $910083FF GG                                         \ drop-locals: add sp,#32
11 $D65F03C0 GG                                         \ ret

\ TLP-STORE2-G: pop the typed address, call the whole-span LPROTSPAN ABI before
\ mutation, copy slot0 then tag, and pop both source cells. This one KEEPS a
\ link frame, because the LPROTSPAN call destroys x30.
s" TLP-STORE2-G" TLP-XT GXT !
0 $F81F0FFE GG                                          \ str x30,[sp,#-16]!  frame + save, one instruction
1 $F85F8E6A GG                                          \ ldr x10,[x19,#-8]!  the destination, one instruction
2 $D100426E GG  3 $D2800049 GG
4 $D280020B GG
5 GCALL                                                \ whole-span LPROTSPAN call (BL or chain)
GN @    $F94001CF GG  GN @ 1 + $F900014F GG  GN @ 2 + $910021CE GG  GN @ 3 + $9100214A GG
GN @ 4 + $F1000529 GG  GN @ 5 + $54FFFF61 GG  GN @ 6 + $D1004273 GG
                                                       \ that loop stays -5: it stores through x10.
GN @ 7 + $F84107FE GG                                  \ ldr x30,[sp],#16   restore + release, one instruction
GN @ 8 + $D65F03C0 GG

\ TLP-FETCH2-G: validate the inline descriptor before the typed address is
\ popped, then read slot0 and tag in canonical bundle order. The absolute call
\ target changes under ASLR, so its four-instruction opcode shape is masked.
s" TLP-FETCH2-G" TLP-XT GXT !
0 $F81F0FFE GG                                          \ str x30,[sp,#-16]!
1 GCALL                                                 \ inline-descriptor LP2VEXEC call (BL or chain)
GN @ $14000009 GG                                       \ branch over 8 descriptor u32s
GN @ 1 + 1 GG  GN @ 2 + 0 GG                             \ one check (u64 cell)
GN @ 3 + 1 GG  GN @ 4 + 0 GG                             \ tag at cell offset 1
GN @ 5 + 2 GG  GN @ 6 + 0 GG                             \ two declaration-order tags
GN @ 7 + 0 GG  GN @ 8 + 0 GG                             \ no ancestor guards
GN @ 9 + $F85F8E6A GG                                    \ ldr x10,[x19,#-8]!  the source, one instruction
GN @ 10 + $D2800049 GG
GN @ 11 + $F940014B GG  GN @ 12 + $9100214A GG  GN @ 13 + $F800866B GG
GN @ 14 + $F1000529 GG  GN @ 15 + $54FFFF81 GG           \ b.ne -4: this loop pushes, so it IS a word shorter
GN @ 16 + $F84107FE GG  GN @ 17 + $D65F03C0 GG

\ ---------------------------------------------------------------------------
\ execution rows: whole-bundle transports at RUNTIME. The seeds are the REAL
\ generated constructors (item 8/11: `tlp-res` derives package TLP--RES,
\ `tlp-mix` derives TLP--MIX — tail hyphens escape as `--`), so the physical
\ cells (payload, zero pads, tag) come from checked constructor bodies, not
\ trusted raw pushes. TLP-MK2 = 7 TLP--RES:ERR -> (7, tag 1); TLP-MK4 =
\ 91 92 93 TLP--MIX:BIG -> (91, 92, 93, tag 1). Only the UNPACKERS remain a
\ tested TRUSTED boundary: surfacing bundle cells for value asserts needs a
\ destructor, which is item 9's MATCH (dot habu-retire-tlp-mk2-ac7760d2).
\ Both raw unpackers retire with habu-retire-tlp-mk2-ac7760d2 when checked
\ MATCH/destructuring can expose their payload cells.
\ ---------------------------------------------------------------------------
: TLP-MK2 ( -- tlp-res<n,n> ) 7 TLP--RES:ERR ;
: TLP-MK2B ( -- tlp-res<n,n> ) 8 TLP--RES:OK ;
\ Tested boundary (TRUSTED): the matching 2-cell unpack (payload, tag).
TRUSTED: TLP-UN2 ( tlp-res<n,n> -- n n ) ;
: TLP-MK4 ( -- tlp-mix<n,n> ) 91 92 93 TLP--MIX:BIG ;
\ Tested boundary (TRUSTED): the matching 4-cell unpack.
TRUSTED: TLP-UN4 ( tlp-mix<n,n> -- n n n n ) ;

\ Executed memory lowering: constructor-produced bundles cross typed addresses
\ and return with payload, padding, and tag order intact.
: TLPX-STORE2 ( -- ) TLP-MK2 TLP-STORE2 ;
: TLPX-FETCH2 ( -- n n ) TLP-FETCH2 TLP-UN2 ;
TLPX-STORE2
TLPX-FETCH2 1 T= 7 T=
: TLPX-STORE4 ( -- ) TLP-MK4 TLP-STORE4 ;
: TLPX-FETCH4 ( -- n n n n ) TLP-FETCH4 TLP-UN4 ;
TLPX-STORE4
TLPX-FETCH4 1 T= 93 T= 92 T= 91 T=

: TLPX-DUP ( -- n n n n ) TLP-MK2 dup {: a b :} a TLP-UN2 b TLP-UN2 ;
TLPX-DUP 1 T= 7 T= 1 T= 7 T=
: TLPX-DROP ( -- n ) 5 TLP-MK2 drop ;
TLPX-DROP 5 T=
: TLPX-SWAP ( -- n n n ) TLP-MK2 5 swap {: s:n r :} s r TLP-UN2 ;
TLPX-SWAP 1 T= 7 T= 5 T=
: TLPX-OVER ( -- n n n n n ) TLP-MK2 5 over {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-OVER 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-NIP ( -- n n ) 5 TLP-MK2 nip TLP-UN2 ;
TLPX-NIP 1 T= 7 T=
: TLPX-TUCK ( -- n n n n n ) 5 TLP-MK2 tuck {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-TUCK 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-ROT ( -- n n n n ) TLP-MK2 5 6 rot {: s1:n s2:n r :} s1 s2 r TLP-UN2 ;
TLPX-ROT 1 T= 7 T= 6 T= 5 T=
: TLPX-MROT ( -- n n n n ) 5 6 TLP-MK2 -rot {: r s1:n s2:n :} r TLP-UN2 s1 s2 ;
TLPX-MROT 6 T= 5 T= 1 T= 7 T=
: TLPX-2DUP ( -- n n n n n n ) TLP-MK2 5 2dup {: r1 s1:n r2 s2:n :} r1 TLP-UN2 s1 r2 TLP-UN2 s2 ;
TLPX-2DUP 5 T= 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-2DROP ( -- n ) 6 TLP-MK2 5 2drop ;
TLPX-2DROP 6 T=
: TLPX-2SWAP ( -- n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2swap {: m s2:n r s1:n :} m TLP-UN4 s2 r TLP-UN2 s1 ;
TLPX-2SWAP 5 T= 1 T= 7 T= 6 T= 1 T= 93 T= 92 T= 91 T=
: TLPX-2OVER ( -- n n n n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2over {: r1 s1:n m1 s2:n r2 s3:n :} r1 TLP-UN2 s1 m1 TLP-UN4 s2 r2 TLP-UN2 s3 ;
TLPX-2OVER 5 T= 1 T= 7 T= 6 T= 1 T= 93 T= 92 T= 91 T= 5 T= 1 T= 7 T=
: TLPX-TOR ( -- n n n ) TLP-MK2 >r 5 r> TLP-UN2 ;
TLPX-TOR 1 T= 7 T= 5 T=
: TLPX-RAT ( -- n n n n ) TLP-MK2 >r r@ {: c :} r> TLP-UN2 c TLP-UN2 ;
TLPX-RAT 1 T= 7 T= 1 T= 7 T=
: TLPX-2TOR ( -- n n n ) TLP-MK2 5 2>r 2r> {: r s:n :} r TLP-UN2 s ;
TLPX-2TOR 5 T= 1 T= 7 T=
: TLPX-2RAT ( -- n n n n n n ) TLP-MK2 5 2>r 2r@ {: c cs:n :}
   2r> {: r s:n :} c TLP-UN2 cs r TLP-UN2 s ;
TLPX-2RAT 5 T= 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-MIX-DUP ( -- n n n n n n n n ) TLP-MK4 dup {: a b :} a TLP-UN4 b TLP-UN4 ;
TLPX-MIX-DUP 1 T= 93 T= 92 T= 91 T= 1 T= 93 T= 92 T= 91 T=
: TLPX-MIX-SWAP ( -- n n n n n ) TLP-MK4 5 swap {: s:n m :} s m TLP-UN4 ;
TLPX-MIX-SWAP 1 T= 93 T= 92 T= 91 T= 5 T=
: TLPX-LOCAL ( -- n n n n n n n n n ) 5 TLP-MK4 {: y:n z :} z TLP-UN4 y z TLP-UN4 ;
TLPX-LOCAL 1 T= 93 T= 92 T= 91 T= 5 T= 1 T= 93 T= 92 T= 91 T=
\ two distinct wide locals in one carve group pin declaration-order bind replay
\ against operand-position-sorted width evidence.
package TLP-LOCAL-TEST
public
: DUAL ( -- n n n n n n )
   TLP-MK2 TLP-MK4 {: r m :} r TLP-UN2 m TLP-UN4 ;
: DEEP ( -- n n n n n n )
   TLP-MK2 1 2 3 4 {: r a:n b:n c:n d:n :}
   a b c d r TLP-UN2 ;
;package
TLP-LOCAL-TEST:DUAL 1 T= 93 T= 92 T= 91 T= 1 T= 7 T=
TLP-LOCAL-TEST:DEEP 1 T= 7 T= 4 T= 3 T= 2 T= 1 T=

\ a wide local bound at TOP LEVEL and REFERENCED inside both arms of a branch.
: TLPX-REF-BRANCH ( n -- n n ) TLP-MK2 {: a :} 0 > if a TLP-UN2 else a TLP-UN2 then ;
5 TLPX-REF-BRANCH 1 T= 7 T=
-3 TLPX-REF-BRANCH 1 T= 7 T=

\ ---------------------------------------------------------------------------
\ branch-scoped bundle locals (habu-tfam-12-pass): a bundle local BOUND inside
\ a branch arm lowers by bind sequence (checker LOCW-HW + the P2-CARVE-W live
\ replay), so sibling arms may reuse the same frame slot — at different widths.
\ Each subject runs BOTH arms; values prove whole-bundle capture + reference.
\ ---------------------------------------------------------------------------
: TLPX-BRIF ( n -- n n ) 0 > if TLP-MK2 {: r :} r TLP-UN2
   else TLP-MK2B {: r :} r TLP-UN2 then ;
5 TLPX-BRIF 1 T= 7 T=
-3 TLPX-BRIF 0 T= 8 T=
: TLPX-BRCASE ( n -- n n ) case
     1 of TLP-MK2 {: r :} r TLP-UN2 endof
     2 of TLP-MK2B {: r :} r TLP-UN2 endof
     0 0 rot
   endcase ;
1 TLPX-BRCASE 1 T= 7 T=
2 TLPX-BRCASE 0 T= 8 T=
9 TLPX-BRCASE 0 T= 0 T=
\ sibling arms reuse frame slot 0 at width 2 vs width 4.
: TLPX-BRW ( n -- n n n n ) 0 > if TLP-MK2 {: r :} r TLP-UN2 0 0
   else TLP-MK4 {: m :} m TLP-UN4 then ;
5 TLPX-BRW 0 T= 0 T= 1 T= 7 T=
-3 TLPX-BRW 1 T= 93 T= 92 T= 91 T=
\ a mixed scalar+wide group inside a branch arm.
: TLPX-BRMIX ( n -- n n n ) 0 > if TLP-MK2 5 {: r s:n :} s r TLP-UN2 else 6 7 8 then ;
1 TLPX-BRMIX 1 T= 7 T= 5 T=
0 TLPX-BRMIX 8 T= 7 T= 6 T=
\ an OUTER wide local below a branch-scoped wide local: the branch carve's
\ cumulative spans the live width-4 entry, and the outer local survives the join.
: TLPX-BROUTER ( n -- n n n n ) TLP-MK4 {: m :}
   0 > if TLP-MK2 {: r :} r TLP-UN2 drop drop then m TLP-UN4 ;
1 TLPX-BROUTER 1 T= 93 T= 92 T= 91 T=
0 TLPX-BROUTER 1 T= 93 T= 92 T= 91 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-layout-lower-pending: failures" 1 die ;
REPORT

;using
