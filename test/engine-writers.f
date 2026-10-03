\ engine-writers.f - the definition writers an interpreter written in Habu
\ publishes through: namespace-record, namespace-private, alias-record,
\ package-scope!, def-open, body-append, trust-sig!, created-sig! and
\ def-close (src/habu/prims.f, "the definition writers"), and the replay
\ writers the checker's overlay replays a package through: replay-open,
\ replay-close, replay-widn!, replay-record and record-wid! (src/habu/prims.f,
\ "the replay writers").
\
\ Each case forks a child that hands its source to `evaluate`, the engine's own
\ interpret loop, and judges the child by its status and its fd 1. A row is
\ observed through a consumer that already exists: `using`, a `package` reopen,
\ a qualified name, the scope lookup and `;package`, `tok-imm?`, the tier-1 `;`
\ and `code-origin`.
\
\ A refusal exits and never throws: 79 while a task is live, 84 for a protected
\ wid after the seal and 83 otherwise. The child's memory ends with it, so a
\ refusal case judges what leaves the process: the exact status, and EW-AT's
\ marker as the whole of fd 1 with fd 2 empty. The marker proves the setup ran
\ (a setup store the seal refused would also exit 83) and that the row was
\ reached; the empty rest proves the row wrote nothing on either stream and
\ nothing after it ran. Each row checks everything before its first store
\ (src/habu/habu2.f, package DEFWRITE).
\
\ Run: bin/hb --load test/engine-writers.f
require lib/string.f
require lib/test.f
require lib/test/subject.f
require src/habu/xref.f

package ENGINE-WRITERS-TEST

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

79 constant TASK-LIVE                  \ habu1.f B-TASK-LIVE-GUARD's $4F
70 constant REJECT-RC                  \ an internal engine word at the prompt
76 constant TRAP-RC                    \ an executed replay record

: EW-MARK$ ( -- ptr u8 n ) s" @" ;

\ Run the source in a child: its fd 1 and fd 2 lengths and its status.
: CHILD ( ptr u8 n -- len len n )
   OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN PROC-OUTCOME>RC RC>N ;

\ Run the source in a child: expect its status and exactly its fd 1. The label
\ names the case.
: RUNS-AS ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: la:ptr lu:n src:ptr size:n want:ptr wantu:n rc:n :}
   src size CHILD {: outu:len erru:len got:n :}
   got rc <> if ERR erru LEN>N type then
   la lu T-LABEL  got rc T=
   la lu T-LABEL  OUT outu LEN>N want wantu T$= ;

: RUNS ( ptr u8 n ptr u8 n n -- ) {: src:ptr size:n want:ptr wantu:n rc:n :}
   src size src size want wantu rc RUNS-AS ;

\ Run a refusal: the source prints EW-AT's marker just before the refused call.
: REFUSES-AS ( ptr u8 n ptr u8 n n -- ) {: la:ptr lu:n src:ptr size:n rc:n :}
   src size CHILD {: outu:len erru:len got:n :}
   la lu T-LABEL  got rc T=
   la lu T-LABEL  OUT outu LEN>N EW-MARK$ T$=
   la lu T-LABEL  ERR erru LEN>N s" " T$= ;

: REFUSES ( ptr u8 n n -- ) {: src:ptr size:n rc:n :}
   src size src size rc REFUSES-AS ;

\ Run a source the engine ends: expect its status and the text on its fd 2.
: DIES-AS ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: la:ptr lu:n src:ptr size:n want:ptr wantu:n rc:n :}
   src size CHILD {: outu:len erru:len got:n :}
   ERR erru LEN>N {: ea:ptr eu:n :}
   ea eu want wantu CONTAINS? 0= if ea eu type then
   la lu T-LABEL  got rc T=
   la lu T-LABEL  ea eu want wantu CONTAINS? TTRUE ;

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;

\ The record is hidden from ordinary source, and the checker refuses a checked
\ caller outside the owner with the verdict given. A nine definition writer's
\ global trusted-only row refuses it by name (0) and types a TRUSTED: body's
\ call. No global row types a replay writer, so there the checker reports an
\ unknown name (1), which the load path refuses as E-UNDEFINED.
: HIDDEN ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n cand:ptr candu:n verdict:n :}
   a u 0 RECORD {: rec:ptr :}
   a u T-LABEL  rec XREF-FOUND? TTRUE
   rec XREF-FOUND? 0= if exit then
   a u T-LABEL  rec XREF-FLAGS DNAME-INT and 0<> TTRUE
   a u T-LABEL  a u 0 search-wl 0 T=
   a u T-LABEL  cand candu CHECK-CANDIDATE! verdict T= ;

: INTERNAL ( ptr u8 n ptr u8 n -- ) 0 HIDDEN ;
: OWNER-ONLY ( ptr u8 n ptr u8 n -- ) 1 HIDDEN ;

public

\ ---- the fixtures' words, each a TRUSTED: boundary ----------------------------
TRUSTED: EW-NS ( ptr u8 n bool -- n ) namespace-record ;
TRUSTED: EW-PRIVATE ( n -- ) namespace-private ;
TRUSTED: EW-ALIAS ( ptr u8 n n n -- ) alias-record ;
TRUSTED: EW-SCOPE ( n n -- ) package-scope! ;
TRUSTED: EW-OPEN ( ptr u8 n n n -- ) def-open ;
TRUSTED: EW-APPEND ( ptr u8 n -- ) body-append ;
TRUSTED: EW-SIG ( ptr u8 n -- ) trust-sig! ;
TRUSTED: EW-CSIG ( ptr u8 n -- ) created-sig! ;
TRUSTED: EW-CLOSE ( -- ) def-close ;

\ The marker a refusal case prints just before the refused call.
: EW-AT ( -- ) EW-MARK$ type ;

TRUSTED: EW-LIVE ( -- ) 1 data-base TASKS-LIVE-CELL + ! ;
TRUSTED: EW-BODYLEN! ( n -- ) data-base BODYLEN-CELL + ! ;

\ NDICT at DICT-CAP. The raise would rebuild the name index over every zeroed
\ slot below the cap, so the index goes first and lookup scans, as it does with
\ no index at all.
TRUSTED: EW-DICT-FULL ( -- ) 0 data-base HIDXP-CELL + !  DICT-CAP ndict! ;

\ The code ceiling a definition and a spilled name stay below.
: EW-CEILING ( -- n ) dbase@ REGION + $4000 - ;

\ The code-origin of record n's out-of-line name bytes.
: EW-NAME-ORIGIN ( n -- n ) XREF-REC XREF-NAME-SLOT XREF-CELL@ dup 4 + code-origin ;

\ The index of an internal (DNAME-INT) record: namespace-record's own.
TRUSTED: EW-INT ( -- n ) s" namespace-record" 0 xref-search-wl dbase@ - DREC / ;

\ The index of a seeded primitive's record, below the count the boot seeds:
\ dup's.
: EW-DUP ( -- n ) s" dup" 0 XREF-FIND-WL-INDEX ;

: EW-SIG$ ( -- ptr u8 n ) s" ( -- n )" ;

\ Open `name ( -- n )` as the engine's `:` leaves a definition: the name and the
\ signature captured, the signature's inner span in TSIG. The engine loop
\ compiles the body tokens that follow and publishes at `;`.
TRUSTED: EW-COLON ( ptr u8 n -- ) {: a:ptr u:n :}
   0 data-base BODYLEN-CELL + !                        \ the caller starts the capture, as `:` does
   a u get-current 0 def-open
   a u body-append
   EW-SIG$ body-append
   EW-SIG$ swap 1+ swap 2 - trust-sig! ;

\ A second writer while a definition is pending: its record is slot NDICT.
TRUSTED: EW-OPEN-TWICE ( -- )
   s" EW-FIRST" get-current 0 def-open  EW-AT  s" EW-SECOND" get-current 0 def-open ;
TRUSTED: EW-OPEN-NS ( -- )
   s" EW-FIRST" get-current 0 def-open  EW-AT  s" EW-NSP" 0 0= namespace-record drop ;
TRUSTED: EW-OPEN-ALIAS ( -- )
   ndict@ 1-  s" EW-FIRST" get-current 0 def-open
   EW-AT  s" EW-ALP" rot get-current alias-record ;

\ def-close on the definition just opened, at the tier the caller set.
TRUSTED: EW-OPEN-CLOSE ( -- )
   s" EW-FIRST" get-current 0 def-open  EW-AT  def-close ;

\ ---- the scope replay-open saves ----------------------------------------------
\ Cell k of the scope: 0 NDICT, 1 CP, then EW-OFFS's DATA cells, then the used
\ wids. A case marks it before replay-open and compares it after replay-close.
8 constant EW-OFFS-N
2 EW-OFFS-N + constant EW-WIDS-AT
EW-WIDS-AT USE-MAX + constant EW-SCOPE-N
create EW-OFFS
   WIDN-CELL , CUR-CELL , PKG-PUB-CELL , PKG-PRI-CELL ,
   PKG-PARENT-CELL , PKG-REC-CELL , USE-DEPTH-CELL , USE-PKG-SAVE-CELL ,
create EW-SAVED EW-SCOPE-N cells allot

: EW-SCOPE@ ( n -- n ) {: k:n :}
   k 0= if ndict@ exit then
   k 1 = if cp@ exit then
   k EW-WIDS-AT < if data-base EW-OFFS k 2 - cells + @ + @ exit then
   data-base USE-WIDS-OFF + k EW-WIDS-AT - cells + @ ;

: EW-MARK ( -- )
   EW-SCOPE-N 0 ?do i EW-SCOPE@ EW-SAVED i cells + ! loop ;

: EW-MARKED-NDICT ( -- n ) EW-SAVED @ ;

\ Print the index of every scope cell that differs from EW-MARK's.
: EW-SAME ( -- )
   EW-SCOPE-N 0 ?do
      i EW-SCOPE@ EW-SAVED i cells + @ <> if i . then
   loop ;

\ The open package's private wid.
: EW-PRI ( -- n ) data-base PKG-PRI-CELL + @ ;

\ The next wordlist id the engine hands out.
: EW-WIDN ( -- n ) data-base WIDN-CELL + @ ;

\ Raw stores into the using band, which the checker and the runtime admit:
\ every used wid, the package floor, and the depth last, so that no lookup
\ reads the others.
: EW-SCRIBBLE ( -- )
   USE-MAX 0 ?do i 100 + data-base USE-WIDS-OFF + i cells + ! loop
   7 data-base USE-PKG-SAVE-CELL + !
   0 data-base USE-DEPTH-CELL + ! ;

\ Print the DATA offset of every nonzero cell in [first, end).
: EW-ZERO-BAND ( n n -- ) {: end:n first:n :}
   first begin dup end < while
      dup data-base + @ 0<> if dup . then
      1 cells +
   repeat drop ;

\ Print the record and cell of every nonzero cell in count records from k.
: EW-ZERO-RECS ( n n -- ) {: k:n count:n :}
   count 0 ?do
      DREC 1 cells / 0 ?do
         k j + XREF-REC i XREF-CELL@ 0<> if k j + . i . then
      loop
   loop ;

private

: BOUNDARY ( -- )
   s" namespace-record" s" EWX ( ptr u8 n bool -- n ) namespace-record" INTERNAL
   s" namespace-private" s" EWX ( n -- ) namespace-private" INTERNAL
   s" alias-record" s" EWX ( ptr u8 n n n -- ) alias-record" INTERNAL
   s" package-scope!" s" EWX ( n n -- ) package-scope!" INTERNAL
   s" def-open" s" EWX ( ptr u8 n n n -- ) def-open" INTERNAL
   s" body-append" s" EWX ( ptr u8 n -- ) body-append" INTERNAL
   s" trust-sig!" s" EWX ( ptr u8 n -- ) trust-sig!" INTERNAL
   s" created-sig!" s" EWX ( ptr u8 n -- ) created-sig!" INTERNAL
   s" def-close" s" EWX ( -- ) def-close" INTERNAL
   s" replay-open" s" EWX ( -- ) replay-open" OWNER-ONLY
   s" replay-close" s" EWX ( -- ) replay-close" OWNER-ONLY
   s" replay-widn!" s" EWX ( n -- ) replay-widn!" OWNER-ONLY
   s" replay-record" s" EWX ( ptr u8 n n -- ) replay-record" OWNER-ONLY
   s" record-wid!" s" EWX ( n n -- ) record-wid!" OWNER-ONLY ;

\ A flagged row answers `using`, a `package` reopen and a qualified name. The
\ reopen must find this row: a second row would hold X, and `using NSA` and
\ NSA:X, which resolve this one, would miss it.
: NAMESPACES ( -- )
   s" parse-name NSA true EW-NS drop package NSA public : X ( -- n ) 7 ; ;package NSA:X . using NSA X . ;using"
   s\" 7\n7\n" 0 RUNS
   s" parse-name NSB false EW-NS EW-PRIVATE package NSB public : Y ( -- n ) 8 ; ;package NSB:Y . using NSB Y . ;using"
   s\" 8\n8\n" 0 RUNS
   s" parse-name A-LONG-NAMESPACE-ROW true EW-NS EW-NAME-ORIGIN . package A-LONG-NAMESPACE-ROW public : Z ( -- n ) 9 ; ;package A-LONG-NAMESPACE-ROW:Z ."
   s\" 1\n9\n" 0 RUNS ;

\ The scope opens on the private wid namespace-private gave the row, which
\ package-scope! refuses a row without; `;package` closes it again.
: SCOPES ( -- )
   s" parse-name NSC false EW-NS dup EW-PRIVATE get-current EW-SCOPE private : H ( -- n ) 5 ; H . ;package H"
   s\" 5\n" 70 RUNS ;

\ An alias runs its source's code and keeps the source's immediate bit.
: ALIASES ( -- )
   s" : SRC ( -- ) 42 . ; immediate parse-name AL ndict@ 1- get-current EW-ALIAS AL parse-name AL tok-imm? ."
   s\" 42\n2\n" 0 RUNS
   s" : SRC ( -- n ) 6 ; parse-name AN-ALIAS-WITH-A-LONG-NAME ndict@ 1- get-current EW-ALIAS AN-ALIAS-WITH-A-LONG-NAME . ndict@ 1- EW-NAME-ORIGIN ."
   s\" 6\n1\n" 0 RUNS ;

\ An opened definition is finished by the engine loop at tier 1: `42 ;`
\ publishes it, and its code, like a spilled name, is native.
: DEFINITIONS ( -- )
   s" 1 set-tier parse-name DW EW-COLON 42 ; DW . ' DW dup 4 + code-origin ."
   s\" 42\n1\n" 0 RUNS
   s" 1 set-tier parse-name DEFINED-THROUGH-DEF-OPEN EW-COLON 42 ; DEFINED-THROUGH-DEF-OPEN . ndict@ 1- EW-NAME-ORIGIN ."
   s\" 42\n1\n" 0 RUNS ;

\ The capture takes the bytes and one space up to the buffer's last byte.
: BODIES ( -- )
   s" BODYBUF-CAP 4 - EW-BODYLEN! parse-name abc EW-APPEND data-base BODYLEN-CELL + @ ."
   s\" 8000\n" 0 RUNS ;

: LIVE-REFUSALS ( -- )
   s" EW-LIVE parse-name NSL true EW-AT EW-NS" TASK-LIVE REFUSES
   s" parse-name NSL false EW-NS EW-LIVE EW-AT EW-PRIVATE" TASK-LIVE REFUSES
   s" : S ( -- ) ; EW-LIVE parse-name AL ndict@ 1- get-current EW-AT EW-ALIAS" TASK-LIVE REFUSES
   s" EW-LIVE parse-name DW get-current 0 EW-AT EW-OPEN" TASK-LIVE REFUSES ;

\ Refusals every record writer shares: an empty name, a length no region holds,
\ a spill past the code ceiling, a live (folded name, wid) pair, NDICT at
\ DICT-CAP and a pending definition, whose record is the slot it would write.
: RECORD-REFUSALS ( -- )
   s" parse-name NSE drop 0 true EW-AT EW-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name NSE drop -1 true EW-AT EW-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-CEILING 8 - cp! parse-name A-LONG-NAMESPACE-ROW true EW-AT EW-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name NSD true EW-NS drop parse-name nsd false EW-AT EW-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-DICT-FULL parse-name NSF true EW-AT EW-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-OPEN-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name AL drop 0 ndict@ 1- get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name AL drop -1 ndict@ 1- get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; ndict@ 1- EW-CEILING 8 - cp! parse-name AN-ALIAS-WITH-A-LONG-NAME rot get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name s ndict@ 1- get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; ndict@ 1- EW-DICT-FULL parse-name ALF rot get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-OPEN-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW drop 0 get-current 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW drop -1 get-current 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-CEILING 8 - cp! parse-name DEFINED-THROUGH-DEF-OPEN get-current 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name s get-current 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-DICT-FULL parse-name DWF get-current 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-OPEN-TWICE" ENGINE-ERROR:SEAL-VIOLATION REFUSES ;

\ The refusals particular to one row. An alias of an internal word would carry
\ an engine body with no checker-known effect past its DNAME-INT gate.
: ROW-REFUSALS ( -- )
   s" parse-name NS:X true EW-AT EW-NS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" ndict@ EW-AT EW-PRIVATE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" -1 EW-AT EW-PRIVATE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; ndict@ 1- EW-AT EW-PRIVATE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name NSP true EW-NS EW-AT EW-PRIVATE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name AW ndict@ 1- -1 EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name AW ndict@ 1- -2 EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; parse-name AP ndict@ 1- OWNER-API-PUB-WID EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-PACKAGE REFUSES
   s" parse-name AR ndict@ get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name AR -1 get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name NSS true EW-NS parse-name AN rot get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : R ( -- ) ; ndict@ 1- undefine R parse-name AR rot get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name AI EW-INT get-current EW-AT EW-ALIAS" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" ndict@ 0 EW-AT EW-SCOPE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" -1 5 EW-AT EW-SCOPE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" : S ( -- ) ; ndict@ 1- 0 EW-AT EW-SCOPE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name NSQ false EW-NS 0 EW-AT EW-SCOPE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW get-current 1 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW -1 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW -2 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW OWNER-API-PUB-WID 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-PACKAGE REFUSES
   s" EW-CEILING cp! parse-name DW get-current 0 EW-AT EW-OPEN" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" BODYBUF-CAP 3 - EW-BODYLEN! parse-name abc EW-AT EW-APPEND" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name abc drop -1 EW-AT EW-APPEND" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" BODYBUF-CAP 1+ EW-BODYLEN! parse-name a EW-AT EW-APPEND" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" -5 EW-BODYLEN! parse-name abcdef EW-AT EW-APPEND" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name abc EW-AT EW-SIG" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name abc EW-AT EW-CSIG" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" EW-AT EW-CLOSE" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" 0 set-tier EW-OPEN-CLOSE" ENGINE-ERROR:SEAL-VIOLATION REFUSES ;

\ ---- the replay writers -------------------------------------------------------
\ No global row types them: outside their owner a TRUSTED: body binds one only
\ at tier 0, as unchecked code does. Each case defines its callers in the
\ child, unchecked, where a call to an owned primitive compiles at tier 0 with
\ no checker to decide (docs/forth.md, "Checked code and primitive
\ boundaries"), then runs its row. A case is labelled by its row alone.
: CALLERS ( -- )
   SB-RESET
   s" 0 set-check : EWR-OPEN ( -- ) replay-open ; : EWR-CLOSE ( -- ) replay-close ; " SB-APPEND
   s" : EWR-WIDN ( n -- ) replay-widn! ; " SB-APPEND
   s" : EWR-REC ( ptr u8 n n -- ) replay-record ; : EWR-WID ( n n -- ) record-wid! ; " SB-APPEND ;

: REPLAY$ ( ptr u8 n -- ptr u8 n )
   CALLERS SB-APPEND SB$ ;

: REPLAY-RUNS ( ptr u8 n ptr u8 n n -- ) {: row:ptr rowu:n want:ptr wantu:n rc:n :}
   row rowu  row rowu REPLAY$  want wantu rc RUNS-AS ;

: REPLAY-REFUSES ( ptr u8 n n -- ) {: row:ptr rowu:n rc:n :}
   row rowu  row rowu REPLAY$  rc REFUSES-AS ;

\ The prompt refuses a replay writer by name, and a replay record is codeless:
\ executing one traps.
: REPLAY-TRAPS ( -- )
   s" replay-record" 2dup s" hb: internal engine word: replay-record" REJECT-RC DIES-AS
   s" EWR-OPEN parse-name EWX get-current EWR-REC EWX" 2dup REPLAY$
   s" hb: replay record executed" TRAP-RC DIES-AS ;

\ replay-close puts back everything replay-open saved and zeroes the records
\ published since, then its own band. The first case enters a package the
\ overlay made, moves CURRENT, scribbles the using band and replays a long name
\ into the package; the second ends on a namespace row, which namespace-record
\ counts into the records replay-close zeroes. A case that left any of it would
\ print a number before the marker.
: REPLAY-RESTORES ( -- )
   CALLERS
   s" EW-MARK EWR-OPEN parse-name EWNS true EW-NS get-current EW-SCOPE EW-PRI set-current EW-SCRIBBLE " SB-APPEND
   s" parse-name A-LONG-REPLAY-RECORD-NAME get-current EWR-REC EWR-CLOSE EW-SAME EW-MARKED-NDICT 2 EW-ZERO-RECS " SB-APPEND
   s" REPLAY-SCOPE:END REPLAY-SCOPE:LATCH EW-ZERO-BAND EWR-OPEN EWR-CLOSE EW-AT" SB-APPEND
   s" replay-close restores a package scope, CURRENT, the using band and a long name"
   SB$ EW-MARK$ 0 RUNS-AS
   s" EW-MARK EWR-OPEN parse-name EWNS true EW-NS drop EWR-CLOSE EW-SAME EW-MARKED-NDICT 1 EW-ZERO-RECS EW-AT"
   EW-MARK$ 0 REPLAY-RUNS ;

\ record-wid! retires a record and gives it back its wid: a replay record takes
\ the name meanwhile, and the restored word answers once replay-close removes
\ the replay record. In the second case overlay cycles bring the claimed-slot
\ count to one under HIDX:LOAD-MAX, so the next claim, made while EWT is
\ retired, compacts the hash index and keys EWT on RETIRED's chain; the first
\ number shows the compaction ran, the next two that the restored EWT is found
\ and runs. The third retires a seeded primitive's record, as the live
\ `undefine dup` retires it: dup misses while it is retired and runs after the
\ close.
: REPLAY-RETIRES ( -- )
   s" : EWT ( -- n ) 5 ; ndict@ 1- EWR-OPEN -2 over EWR-WID parse-name EWT get-current search-wl . parse-name EWT get-current EWR-REC get-current swap EWR-WID EWR-CLOSE EWT ."
   s\" 0\n5\n" 0 REPLAY-RUNS
   CALLERS
   s" create EWN 6 allot parse-name EWN-AA EWN swap BYTE-COPY variable EWI variable EWC " SB-APPEND
   s" : EWN! ( n -- ) {: i:n :} i 26 mod [char] A + EWN 4 + c! i 26 / 26 mod [char] A + EWN 5 + c! ; " SB-APPEND
   s" : EWCLAIMS ( -- n ) data-base HIDX:CLAIMS + @ ; " SB-APPEND
   s" : EWCHURN ( -- ) begin EWCLAIMS HIDX:LOAD-MAX 1- < while EWI @ EWN! 1 EWI +! EWR-OPEN EWN 6 get-current EWR-REC EWR-CLOSE repeat ; " SB-APPEND
   s" : EWT ( -- n ) 99 ; ndict@ 1- EWCHURN EWCLAIMS EWC ! EWR-OPEN -2 over EWR-WID parse-name RX get-current EWR-REC " SB-APPEND
   s" get-current swap EWR-WID EWR-CLOSE EWCLAIMS EWC @ < . parse-name EWT get-current search-wl 0<> . EWT ." SB-APPEND
   s" record-wid! restores a record that a compaction re-keyed while it was retired"
   SB$ s\" -1\n-1\n99\n" 0 RUNS-AS
   s" EW-DUP EWR-OPEN -2 over EWR-WID parse-name dup 0 search-wl . parse-name dup 0 EWR-REC 0 swap EWR-WID EWR-CLOSE 3 dup + ."
   s\" 0\n6\n" 0 REPLAY-RUNS ;

\ replay-widn! puts WIDN back to a mark inside the overlay: a namespace row
\ takes two wids from the mark, ndict! drops the row, and once WIDN is back at
\ the mark the next row takes the first of them again; replay-close then finds
\ the scope as it was. It refuses no overlay, a mark below the saved WIDN and
\ one above WIDN.
: REPLAY-REWINDS ( -- )
   s" EW-MARK EWR-OPEN EW-WIDN parse-name EWW true EW-NS drop ndict@ 1- ndict! dup EWR-WIDN parse-name EWV true EW-NS XREF-REC XREF-PKG-PUBLIC = . EWR-CLOSE EW-SAME EW-AT"
   s\" -1\n@" 0 REPLAY-RUNS
   s" EW-WIDN EW-AT EWR-WIDN" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN EW-WIDN 1- EW-AT EWR-WIDN" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN EW-WIDN 1+ EW-AT EWR-WIDN" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES ;

: REPLAY-LIVE ( -- )
   s" EW-LIVE EW-AT EWR-OPEN" TASK-LIVE REPLAY-REFUSES
   s" EWR-OPEN EW-LIVE parse-name RX get-current EW-AT EWR-REC" TASK-LIVE REPLAY-REFUSES
   s" EWR-OPEN EW-LIVE EW-WIDN EW-AT EWR-WIDN" TASK-LIVE REPLAY-REFUSES
   s" : S ( -- ) ; ndict@ 1- EWR-OPEN EW-LIVE -2 swap EW-AT EWR-WID" TASK-LIVE REPLAY-REFUSES
   s" EWR-OPEN EW-LIVE EW-AT EWR-CLOSE" TASK-LIVE REPLAY-REFUSES ;

\ replay-open refuses an open overlay and a pending definition; replay-record
\ refuses no overlay, a namespace or retired wid and what every record writer
\ refuses (RECORD-REFUSALS).
: REPLAY-OPEN-RECORD ( -- )
   s" EWR-OPEN EW-AT EWR-OPEN" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" : EWP ( -- ) parse-name get-current 0 EW-OPEN EW-AT EWR-OPEN ; EWP DW" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" parse-name RX get-current EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN parse-name RX -1 EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN parse-name RX -2 EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN parse-name RX drop 0 get-current EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN parse-name RX drop -1 get-current EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN EW-CEILING 8 - cp! parse-name A-LONG-REPLAY-RECORD-NAME get-current EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN parse-name RX get-current EWR-REC parse-name rx get-current EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN EW-DICT-FULL parse-name RX get-current EW-AT EWR-REC" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" : EWP ( -- ) EWR-OPEN parse-name get-current 0 EW-OPEN EW-AT parse-name get-current EWR-REC ; EWP DW RX" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES ;

\ record-wid! refuses no overlay, an index at or past NDICT (unsigned), a
\ namespace row and the namespace wid; replay-close refuses no overlay, a
\ pending definition, a record published by another writer, NDICT below the
\ mark and CP below it.
: REPLAY-WID-CLOSE ( -- )
   s" : S ( -- ) ; -2 ndict@ 1- EW-AT EWR-WID" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN -2 ndict@ EW-AT EWR-WID" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN -2 -1 EW-AT EWR-WID" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN parse-name NSW true EW-NS -2 swap EW-AT EWR-WID" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" : S ( -- ) ; ndict@ 1- EWR-OPEN -1 swap EW-AT EWR-WID" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EW-AT EWR-CLOSE" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" : EWP ( -- ) EWR-OPEN parse-name get-current 0 EW-OPEN EW-AT EWR-CLOSE ; EWP DW" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN : S ( -- ) ; EW-AT EWR-CLOSE" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" : S ( -- ) ; EWR-OPEN ndict@ 1- ndict! EW-AT EWR-CLOSE" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES
   s" EWR-OPEN cp@ 4 - cp! EW-AT EWR-CLOSE" ENGINE-ERROR:SEAL-VIOLATION REPLAY-REFUSES ;

public

: EW-RUN ( -- )
   T-RESET
   s" each writer is internal and refused to a checked caller" T-LABEL BOUNDARY T-NEXT
   s" namespace rows answer using, reopen and qualified names" T-LABEL NAMESPACES T-NEXT
   s" package-scope! opens a private scope ;package closes" T-LABEL SCOPES T-NEXT
   s" an alias runs its source and stays immediate" T-LABEL ALIASES T-NEXT
   s" def-open body-append trust-sig! then 42 ; publish natively" T-LABEL DEFINITIONS T-NEXT
   s" body-append fills BODYBUF to its last byte" T-LABEL BODIES T-NEXT
   s" the four dictionary rows refuse a live task" T-LABEL LIVE-REFUSALS T-NEXT
   s" record writers refuse what would corrupt the dictionary" T-LABEL RECORD-REFUSALS T-NEXT
   s" each row refuses its own corrupting inputs" T-LABEL ROW-REFUSALS T-NEXT
   s" the prompt refuses a replay writer and a replay record traps" T-LABEL REPLAY-TRAPS T-NEXT
   s" replay-close restores what replay-open saved" T-LABEL REPLAY-RESTORES T-NEXT
   s" record-wid! retires a record and restores it" T-LABEL REPLAY-RETIRES T-NEXT
   s" replay-widn! puts WIDN back to a mark and refuses the rest" T-LABEL REPLAY-REWINDS T-NEXT
   s" the replay writers refuse a live task" T-LABEL REPLAY-LIVE T-NEXT
   s" replay-open and replay-record refuse their corrupting inputs" T-LABEL REPLAY-OPEN-RECORD T-NEXT
   s" record-wid! and replay-close refuse their corrupting inputs" T-LABEL REPLAY-WID-CLOSE T-NEXT
   T-REPORT ;

;package

\ The fixtures run with no package open, since several open one, and with this
\ package's words imported, which the children inherit.
using ENGINE-WRITERS-TEST
EW-RUN
;using
