\ engine-writers.f - the definition writers an interpreter written in Habu
\ publishes through: namespace-record, namespace-private, alias-record,
\ package-scope!, def-open, body-append, trust-sig!, created-sig! and
\ def-close (src/habu/prims.f, "the definition writers").
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
require lib/test.f
require lib/test/subject.f
require src/habu/xref.f

package ENGINE-WRITERS-TEST

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

79 constant TASK-LIVE                  \ habu1.f B-TASK-LIVE-GUARD's $4F

: EW-MARK$ ( -- ptr u8 n ) s" @" ;

\ Run the source in a child: its fd 1 and fd 2 lengths and its status.
: CHILD ( ptr u8 n -- len len n )
   OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN PROC-OUTCOME>RC RC>N ;

\ Run the source in a child: expect its status and exactly its fd 1.
: RUNS ( ptr u8 n ptr u8 n n -- ) {: src:ptr size:n want:ptr wantu:n rc:n :}
   src size CHILD {: outu:len erru:len got:n :}
   got rc <> if ERR erru LEN>N type then
   src size T-LABEL  got rc T=
   src size T-LABEL  OUT outu LEN>N want wantu T$= ;

\ Run a refusal: the source prints EW-AT's marker just before the refused call.
: REFUSES ( ptr u8 n n -- ) {: src:ptr size:n rc:n :}
   src size CHILD {: outu:len erru:len got:n :}
   src size T-LABEL  got rc T=
   src size T-LABEL  OUT outu LEN>N EW-MARK$ T$=
   src size T-LABEL  ERR erru LEN>N s" " T$= ;

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;

\ The record is hidden from ordinary source and the checker refuses a checked
\ caller: only a TRUSTED: body reaches the row.
: INTERNAL ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n cand:ptr candu:n :}
   a u T-LABEL  a u 0 RECORD XREF-FLAGS DNAME-INT and 0<> TTRUE
   a u T-LABEL  a u 0 search-wl 0 T=
   a u T-LABEL  cand candu CHECK-CANDIDATE! 0 T= ;

public

\ ---- the fixtures' words, each a TRUSTED: boundary ----------------------------
TRUSTED: EW-NS ( ptr u8 n bool -- n ) namespace-record ;
TRUSTED: EW-PRIVATE ( n -- ) namespace-private ;
TRUSTED: EW-ALIAS ( ptr u8 n n n -- ) alias-record ;
TRUSTED: EW-SCOPE ( n n -- ) package-scope! ;
TRUSTED: EW-OPEN ( ptr u8 n n n -- ) tier@ def-open ;
TRUSTED: EW-OPEN-RAW ( ptr u8 n n n n -- ) def-open ;
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

: EW-SIG$ ( -- ptr u8 n ) s" ( -- n )" ;

\ Open `name ( -- n )` as the engine's `:` leaves a definition: the name and the
\ signature captured, the signature's inner span in TSIG. The engine loop
\ compiles the body tokens that follow and publishes at `;`.
TRUSTED: EW-COLON ( ptr u8 n -- ) {: a:ptr u:n :}
   0 data-base BODYLEN-CELL + !                        \ the caller starts the capture, as `:` does
   a u get-current 0 tier@ def-open
   a u body-append
   EW-SIG$ body-append
   EW-SIG$ swap 1+ swap 2 - trust-sig! ;

\ A second writer while a definition is pending: its record is slot NDICT.
TRUSTED: EW-OPEN-TWICE ( -- )
   s" EW-FIRST" get-current 0 tier@ def-open  EW-AT  s" EW-SECOND" get-current 0 tier@ def-open ;
TRUSTED: EW-OPEN-NS ( -- )
   s" EW-FIRST" get-current 0 tier@ def-open  EW-AT  s" EW-NSP" 0 0= namespace-record drop ;
TRUSTED: EW-OPEN-ALIAS ( -- )
   ndict@ 1-  s" EW-FIRST" get-current 0 tier@ def-open
   EW-AT  s" EW-ALP" rot get-current alias-record ;

\ def-close on the definition just opened, at the tier the caller set.
TRUSTED: EW-OPEN-CLOSE ( -- )
   s" EW-FIRST" get-current 0 tier@ def-open  EW-AT  def-close ;

private

: BOUNDARY ( -- )
   s" namespace-record" s" EWX ( ptr u8 n bool -- n ) namespace-record" INTERNAL
   s" namespace-private" s" EWX ( n -- ) namespace-private" INTERNAL
   s" alias-record" s" EWX ( ptr u8 n n n -- ) alias-record" INTERNAL
   s" package-scope!" s" EWX ( n n -- ) package-scope!" INTERNAL
   s" def-open" s" EWX ( ptr u8 n n n n -- ) def-open" INTERNAL
   s" body-append" s" EWX ( ptr u8 n -- ) body-append" INTERNAL
   s" trust-sig!" s" EWX ( ptr u8 n -- ) trust-sig!" INTERNAL
   s" created-sig!" s" EWX ( ptr u8 n -- ) created-sig!" INTERNAL
   s" def-close" s" EWX ( -- ) def-close" INTERNAL ;

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
   s" parse-name DW get-current 0 2 EW-AT EW-OPEN-RAW" ENGINE-ERROR:SEAL-VIOLATION REFUSES
   s" parse-name DW get-current DKIND:VAL 0 EW-AT EW-OPEN-RAW" ENGINE-ERROR:SEAL-VIOLATION REFUSES
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

public

: EW-RUN ( -- )
   T-RESET
   s" each writer is internal and trusted-only" T-LABEL BOUNDARY T-NEXT
   s" namespace rows answer using, reopen and qualified names" T-LABEL NAMESPACES T-NEXT
   s" package-scope! opens a private scope ;package closes" T-LABEL SCOPES T-NEXT
   s" an alias runs its source and stays immediate" T-LABEL ALIASES T-NEXT
   s" def-open body-append trust-sig! then 42 ; publish natively" T-LABEL DEFINITIONS T-NEXT
   s" body-append fills BODYBUF to its last byte" T-LABEL BODIES T-NEXT
   s" the four dictionary rows refuse a live task" T-LABEL LIVE-REFUSALS T-NEXT
   s" record writers refuse what would corrupt the dictionary" T-LABEL RECORD-REFUSALS T-NEXT
   s" each row refuses its own corrupting inputs" T-LABEL ROW-REFUSALS T-NEXT
   T-REPORT ;

;package

\ The fixtures run with no package open, since several open one, and with this
\ package's words imported, which the children inherit.
using ENGINE-WRITERS-TEST
EW-RUN
;using
