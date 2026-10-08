\ src/host/gforth/layout.fs - the Gforth host's engine layout, its libc calls
\ and Habu's two spaces (docs/bootstrap.md stage 1, "Habu source sees only Habu
\ words").
\
\ Gforth compiles every word under src/host/gforth/, and Habu source reaches
\ none of them by name: reader.fs resolves a Habu token through Habu's records
\ alone, and a record's code cell holds the Gforth xt of its body, a word with
\ no Gforth header. The constants restate the current tree, each beside the line
\ it copies; src/habu/layout.f is not among the 19 kernel files, so the host
\ needs its own copy of what it reads before that file exists.

decimal

\ ---- the host OS ------------------------------------------------------------
s" os-type" environment? 0= [if] s" (none)" [then]
2dup s" darwin" string-prefix? constant HOST-MACOS?
2dup s" linux" string-prefix? constant HOST-LINUX?
HOST-MACOS? HOST-LINUX? or 0= [if]
   2 s" hb: gforth host: unsupported os-type " write drop 2 -rot write drop
   2 s\" \n" write drop 70 (bye)
[then] 2drop

\ ---- the data region (src/habu/layout.f) -------------------------------------
\ DATA-SIZE is the OS's reservation; the DP ceiling leaves the profile
\ counters at its top (habu1.f DP-CHECK, habu2.f LDPBAD).
HOST-MACOS? [if] $10000000000 [else] $2000000 [then]
   constant DATA-SIZE                \ src/os/macos/layout.f:7, src/os/linux/layout-constants.f:15
65536 constant DICT-CAP              \ src/habu/layout.f:350
64 constant PROF-STATE-BYTES         \ src/habu/layout.f:393
DICT-CAP cells PROF-STATE-BYTES + constant PROF-CNT-BYTES   \ src/habu/layout.f:394
DATA-SIZE PROF-CNT-BYTES - constant DP-CEILING              \ habu2.f:12106

\ The band cells the host reads or writes, as offsets from data-base.
0 constant DP-CELL                   \ src/habu/layout.f:402
64 constant LOC-RECS                 \ src/habu/layout.f:412
$28 constant CUR-CELL                \ src/habu/layout.f:456
$30 constant WIDN-CELL               \ src/habu/layout.f:457
$38 constant HOOK-CELL               \ src/habu/layout.f:458
$48 constant TSIG-A-CELL             \ src/habu/layout.f:460
$50 constant TSIG-U-CELL             \ src/habu/layout.f:461
$58 constant TCSIG-A-CELL            \ src/habu/layout.f:462
$60 constant TCSIG-U-CELL            \ src/habu/layout.f:463
$68 constant CRSIG-A-CELL            \ src/habu/layout.f:464
$70 constant CRSIG-U-CELL            \ src/habu/layout.f:465
$78 constant PKG-PUB-CELL            \ src/habu/layout.f:466
$80 constant PKG-PRI-CELL            \ src/habu/layout.f:467
$88 constant PKG-PARENT-CELL         \ src/habu/layout.f:468
$90 constant PKG-REC-CELL            \ src/habu/layout.f:469
$1B8 constant BODYLEN-CELL           \ src/habu/layout.f:512
$3688 constant PEND-CELL             \ src/habu/layout.f:525
$3690 constant TKA-CELL              \ src/habu/layout.f:526
$3698 constant TKL-CELL              \ src/habu/layout.f:527
$36A0 constant INP-CELL              \ src/habu/layout.f:528
$36A8 constant INE-CELL              \ src/habu/layout.f:529
$560 constant LASTC-CELL             \ src/habu/layout.f:1349
$27B0 constant DOESB-CELL            \ src/habu/layout.f:635
$27B8 constant TRUSTED-CELL          \ src/habu/layout.f:636
$2818 constant EXIT-HOOK-CELL        \ src/habu/layout.f:1451
$27E8 constant COMPILE-PREFLIGHT-CELL   \ src/habu/layout.f:1392
$27F8 constant REFUSAL-CODE-CELL     \ src/habu/layout.f:1425 REFUSAL-ABI:CODE-CELL
$2850 constant PENDTKA-CELL          \ src/habu/layout.f:1485
$800 constant BODYBUF-OFF            \ src/habu/layout.f:1332
8000 constant BODYBUF-CAP            \ src/habu/layout.f:1333
$360 constant DECL-CELL              \ src/habu/layout.f:1181 NCOMP-DISPATCH:DECL-CELL
$368 constant TARGET-DECL-CELL       \ src/habu/layout.f:1182 NCOMP-DISPATCH:TARGET-DECL-CELL
$3CC0 constant PROT-BITS-OFF         \ src/habu/layout.f:1002
8192 constant PROT-WID-MAX           \ src/habu/layout.f:1003
$9C08 constant USE-BAND-OFF          \ src/habu/layout.f:1887
USE-BAND-OFF constant USE-DEPTH-CELL           \ src/habu/layout.f:1888
USE-BAND-OFF 8 + constant USE-PKG-SAVE-CELL    \ src/habu/layout.f:1889
USE-BAND-OFF 24 + constant USE-WIDS-OFF        \ src/habu/layout.f:1891
16 constant USE-MAX                  \ src/habu/layout.f:1884
\ The pre-trust defer table (src/habu/layout.f:1841-1849, 2228-2229) and the
\ declared-row log (2263-2274) close the band; DATA-START follows them.
128 constant PD-CAP                  \ src/habu/layout.f:1841
48 constant PD-NAME-CAP              \ src/habu/layout.f:1842
64 constant PD-SIG-CAP               \ src/habu/layout.f:1843
16 constant PD-NAME-OFF              \ src/habu/layout.f:1846
PD-NAME-OFF PD-NAME-CAP + constant PD-SIG-OFF   \ src/habu/layout.f:1847
PD-SIG-OFF PD-SIG-CAP + constant PD-SLOT        \ src/habu/layout.f:1848
8 constant PD-SLOTS-REL              \ src/habu/layout.f:1849
2859072 constant PD-TABLE-OFF        \ src/habu/layout.f:2228 REPLAY-SCOPE:END
PD-TABLE-OFF PD-SLOTS-REL + PD-CAP PD-SLOT * + constant PD-TABLE-END   \ src/habu/layout.f:2229
$4398 constant DLOG-ADDR-CELL        \ src/habu/layout.f:2263 DECLARED-LOG:ADDR-CELL
4096 constant DLOG-CAP               \ src/habu/layout.f:2264
8 constant DLOG-PKG-OFF              \ src/habu/layout.f:2266
16 constant DLOG-SIG-A-OFF           \ src/habu/layout.f:2267
24 constant DLOG-SIG-U-OFF           \ src/habu/layout.f:2268
32 constant DLOG-SLOT                \ src/habu/layout.f:2269
8 constant DLOG-SLOTS-REL            \ src/habu/layout.f:2270
PD-TABLE-END constant DLOG-OFF       \ src/habu/layout.f:2271
DLOG-OFF DLOG-SLOTS-REL + DLOG-CAP DLOG-SLOT * + constant DATA-START   \ src/habu/layout.f:2272-2274

\ ---- the dictionary (src/habu/layout.f) ---------------------------------------
$301000 constant DICT-SIZE           \ src/habu/layout.f:182
48 constant DREC                     \ src/habu/layout.f:200
16 constant DNAME-INL                \ src/habu/layout.f:201
2 constant OWNER-API-PRI-WID         \ src/habu/layout.f:203
3 constant FIRST-DYNAMIC-WID         \ src/habu/layout.f:204
-1 constant WL-NAMESPACE             \ src/habu/layout.f:237 DICT-WL:NAMESPACE
$0004000000000000 constant DKIND-VAL    \ src/habu/layout.f:287 DKIND:VAL
$0008000000000000 constant DKIND-ADDR   \ src/habu/layout.f:288 DKIND:ADDR
DKIND-VAL DKIND-ADDR or constant DKIND-CAST   \ src/habu/layout.f:289
15 constant DNAME-FLAG-BITS          \ src/habu/layout.f:295
-1 DNAME-FLAG-BITS rshift constant DNAME-LEN-MASK   \ src/habu/layout.f:296
$0FF0000000000000 constant DNAME-MIN-IN-MASK        \ src/habu/layout.f:309
$1000000000000000 constant DNAME-IMM    \ src/habu/layout.f:310
$2000000000000000 constant DNAME-EXT    \ src/habu/layout.f:311
$4000000000000000 constant DNAME-WIDE   \ src/habu/layout.f:320
$8000000000000000 constant DNAME-INT    \ src/habu/layout.f:332
$20000 constant HIDX-SLOTS           \ src/habu/layout.f:1067
$80000 constant HIDX-BYTES           \ src/habu/layout.f:1068
HIDX-SLOTS 3 * 4 / constant HIDX-LOAD-MAX   \ src/habu/layout.f:1069-1076 HIDX:LOAD-MAX

\ ---- the checker owner ABI (src/core/checker-owner-abi.f) ----------------------
$0 constant RAW-OFF                  \ src/core/checker-owner-abi.f:8
$8 constant EFFECT-OFF               \ src/core/checker-owner-abi.f:9
$10 constant DEFER-OFF               \ src/core/checker-owner-abi.f:10
$18 constant CAST-OFF                \ src/core/checker-owner-abi.f:11
$20 constant USING-OFF               \ src/core/checker-owner-abi.f:12
$28 constant PACKAGE-OFF             \ src/core/checker-owner-abi.f:13
$30 constant PUBLIC-OFF              \ src/core/checker-owner-abi.f:14
$38 constant PRIVATE-OFF             \ src/core/checker-owner-abi.f:15
$40 constant END-PACKAGE-OFF         \ src/core/checker-owner-abi.f:16
$390 constant DECLARED-ROW-OFF       \ src/core/checker-owner-abi.f:193

\ ---- engine exit codes -----------------------------------------------------
67 constant UNCAUGHT-RC              \ src/habu/layout.f:451
70 constant RC-REJECT                \ src/habu/habu2.f:11636
76 constant REFUSE-RC                \ src/habu/kernel-x64.f:63

\ ---- libc (Gforth libcc; docs/bootstrap.md Requirements) ---------------------
c-library habu_gforth_host
\c #include <sys/mman.h>
c-function HOST-MMAP mmap a n n n n n -- a
c-function HOST-MUNMAP munmap a n -- n
c-value MMAP-PROT PROT_READ|PROT_WRITE -- n
c-value MMAP-FLAGS MAP_ANON|MAP_PRIVATE|MAP_NORESERVE -- n
end-c-library

\ ---- decimal text and fd-2 text ---------------------------------------------
\ Decimal text in Gforth's pictured-output buffer, valid until the next <#:
\ unsigned as LDIAGU writes it (habu2.f:202), signed as G-PRINT9 prints it
\ (habu2.f:11806 mirrors it). Every diagnostic leaves through write(2)
\ unbuffered, as the engine's do.
: UDEC$ ( u -- c-addr u ) base @ >r decimal 0 <# #s #> r> base ! ;
: DEC$ ( n -- c-addr u ) base @ >r decimal dup abs 0 <# #s rot sign #> r> base ! ;
: ERR ( c-addr u -- ) 2 -rot write drop ;
: ERR-NL ( -- ) s\" \n" ERR ;
: ERR-U ( u -- ) UDEC$ ERR ;
: ERR-N ( n -- ) DEC$ ERR ;

\ ---- the reservations (docs/bootstrap.md stage 1, the Memory rule) -----------
\ Each space is one anonymous reservation whose pages fill as it is used.
: RESERVE ( u -- addr )
   0 swap MMAP-PROT MMAP-FLAGS -1 0 HOST-MMAP dup -1 = if
      s" hb: gforth host: address space reservation failed" ERR ERR-NL
      REFUSE-RC (bye) then ;

\ ---- the data space ----------------------------------------------------------
\ The band cells at its foot, the DP heap from DATA-START to DP-CEILING.
DATA-SIZE RESERVE constant HB-DATA
: D ( off -- addr ) HB-DATA + ;
: D@ ( off -- x ) D @ ;
: D! ( x off -- ) D ! ;
$47E0 constant HEAP-START-OFF        \ src/habu/habu2.f:7497 EM-LAYOUT:HEAP-START-OFF
DATA-START HEAP-START-OFF D!         \ habu2.f:7510 EM-DATA-INIT
DATA-START D DP-CELL D!              \ habu2.f:7511
FIRST-DYNAMIC-WID WIDN-CELL D!       \ habu2.f:8855

\ ---- the exit paths ----------------------------------------------------------
\ HB-EXIT is every deliberate exit's tail (habu1.f:1935-1955 BDIE, habu2.f:11823
\ LUNCAUGHT): the process-exit hook, cleared before it runs (habu2.f:5597-5627),
\ then rc when the kernel can represent it, else UNCAUGHT-RC.
: RUN-EXIT-HOOK ( -- ) EXIT-HOOK-CELL D@ ?dup if 0 EXIT-HOOK-CELL D! execute then ;
: HB-EXIT ( rc -- )
   RUN-EXIT-HOOK dup 0 256 within 0= if drop UNCAUGHT-RC then (bye) ;
\ COMPILE-DIE (habu2.f:12022-12072 LCOMPILEDIE): the die site wrote its
\ diagnostic; this ends the line and exits rc with no exit hook. Native throws
\ rc instead inside an evaluate frame, and its `--load` program runs inside one
\ (habu2.f:1935, src/core/include.f:1218); this host reads the program at top
\ level until the Habu loop does, and refuses `evaluate`. No source path is
\ published on this host (reader.fs LOAD-FILE), so the line carries no
\ ` at <path>:<line>`.
: COMPILE-DIE ( rc -- ) ERR-NL (bye) ;
\ DIAG-RET (habu2.f:11908 LDIAGRET): the token's diagnostic is out; exit
\ RC-REJECT (habu2.f:11931 LRDIE).
: DIAG-RET ( -- ) RC-REJECT (bye) ;

\ ---- the DP heap (habu1.f:1888 DP-CHECK, habu2.f:12102 LDPBAD) ---------------
\ A DP outside [DATA-START, DP-CEILING] is refused before it lands: both numbers
\ are offsets from data-base, unsigned (LDIAGU), and the rc is REFUSE-RC.
: DP-CHECK ( addr -- addr )
   dup HB-DATA - DATA-START DP-CEILING 1+ within ?exit
   s" hb: data space out of range: DP " ERR  dup HB-DATA - ERR-U
   s"  of " ERR  DP-CEILING ERR-U  s"  bytes" ERR  REFUSE-RC COMPILE-DIE ;
: HB-HERE ( -- addr ) DP-CELL D@ ;
: DP! ( addr -- ) DP-CHECK DP-CELL D! ;
: HB-ALLOT ( n -- ) HB-HERE + DP! ;
: HB-ALIGN ( -- ) HB-HERE aligned DP! ;
: HB-, ( x -- ) HB-HERE dup cell+ DP! ! ;
: HB-C, ( c -- ) HB-HERE dup 1+ DP! c! ;
\ A definer's body starts at the aligned DP (habu2.f EMIT-CREATE, C-DEFER-CELL).
: HB-BODY ( -- addr ) HB-ALIGN HB-HERE ;

\ ---- the dictionary ----------------------------------------------------------
\ Records at dbase@, DREC bytes each: [0] the code cell, here the body's Gforth
\ xt, [8] the body length, [16] the flags and name length, [24] the name inline
\ or [24] its address with DNAME-EXT, [40] the wid. dict[NDICT] is the pending
\ record a definer writes; publication counts it (habu2.f:3614-3640 NAME-COPY).
DICT-SIZE RESERVE constant HB-DBASE
variable NDICT
: REC ( ix -- rec ) DREC * HB-DBASE + ;
: >FLAGS ( rec -- addr ) 2 cells + ;
: >WID ( rec -- addr ) 5 cells + ;
: REC-NAME$ ( rec -- c-addr u )
   dup 3 cells +  over >FLAGS @ DNAME-EXT and if @ then
   swap >FLAGS @ DNAME-LEN-MASK and ;
\ A long name's copy lives in host memory: native puts it at CP, never in DP.
: REC-NAME! ( c-addr u rec -- ) {: a u rec :}
   u rec >FLAGS !
   u DNAME-INL > if
      DNAME-EXT rec >FLAGS +!  a u save-mem drop rec 3 cells + !
   else a rec 3 cells + u move then ;
\ The token a definer is given, for the capacity refusals (habu2.f C-DIE-TOKEN).
: TOKEN$ ( -- c-addr u ) TKA-CELL D@ TKL-CELL D@ ;
\ REC-PEND writes dict[NDICT], uncounted; a full dictionary is habu2.f:3570-3572
\ C-DIE-DICT-FULL.
: REC-PEND ( xt c-addr u wid -- rec ) {: xt a u wid :}
   NDICT @ DICT-CAP >= if
      s" hb: dictionary full at: " ERR TOKEN$ ERR $4D COMPILE-DIE then
   NDICT @ REC dup DREC erase
   xt over !  wid over >WID !  a u rot dup >r REC-NAME! r> ;

\ ---- the record index (habu1.f EMIT-FIND, C-HIDX-INS; layout.f:1067-1076) ----
\ Open addressing over u32 slots of index+1, keyed on the name folded A-Z XOR
\ the wid. A slot answers while its record is counted and still holds that
\ name and wid, so a rolled-back or retired record is skipped, never removed;
\ the newest answering record wins (FIND-LINEAR's rule). Crossing LOAD-MAX
\ rebuilds the table from the counted records.
HIDX-BYTES RESERVE constant HB-HIDX
variable HIDX-USED
: FOLD ( c -- c ) dup [char] A [char] Z 1+ within $20 and or ;
: NAME= ( c-addr u c-addr2 u2 -- flag )
   rot over <> if drop 2drop false exit then
   0 ?do over i + c@ FOLD over i + c@ FOLD <> if 2drop false unloop exit then loop
   2drop true ;
: NAME-SLOT ( c-addr u wid -- slot )
   >r $CBF29CE484222325 -rot bounds ?do i c@ FOLD xor $100000001B3 * loop
   r> xor HIDX-SLOTS 1- and ;
: SLOT ( slot -- addr ) 4 * HB-HIDX + ;
: NEXT-SLOT ( slot -- slot' ) 1+ HIDX-SLOTS 1- and ;
: HIDX-PUT ( ix -- )
   dup REC dup REC-NAME$ rot >WID @ NAME-SLOT
   begin dup SLOT l@ while NEXT-SLOT repeat
   SLOT swap 1+ swap l!  1 HIDX-USED +! ;
: HIDX-ADD ( ix -- )
   HIDX-USED @ HIDX-LOAD-MAX >= if
      HB-HIDX HIDX-BYTES erase  0 HIDX-USED !
      NDICT @ 0 ?do i HIDX-PUT loop then
   HIDX-PUT ;
\ Count the pending record and index it.
: REC-PUBLISH ( -- ) NDICT @ HIDX-ADD  1 NDICT +! ;
: REC-HIT? ( ix c-addr u wid -- flag ) {: ix a u wid :}
   ix NDICT @ u< 0= if false exit then
   ix REC >WID @ wid <> if false exit then
   a u ix REC REC-NAME$ NAME= ;
: REC-FIND ( c-addr u wid -- rec|0 ) {: a u wid :}
   -1  a u wid NAME-SLOT
   begin dup SLOT l@ ?dup while
      1- dup a u wid REC-HIT? if rot max swap else drop then
      NEXT-SLOT
   repeat drop  dup 0< if drop 0 else REC then ;
