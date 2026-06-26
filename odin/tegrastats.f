\ tegrastats.f - tegrastats line parser, ported from Odin's src/tegrastats.zig.
\
\ Parses one `tegrastats` line into RAM/SWAP/CPU%/GPU%/max-temp/power and renders
\ the canonical one-line summary. Pure data: no hardware/SDK dependency, fully
\ testable. Floats are parsed with lib/float.f then stored as integer tenths so
\ collection/max/render stay integer and no float-cell storage (or trust) is
\ needed. Output is built in the lib/string.f SB builder.
\
\ Internal `TEGRA` module package; the public entry is TEGRA:SUMMARY ( ptr u8 n --
\ ptr u8 n ). Depends on lib/errors.f lib/string.f lib/float.f lib/fmt.f.

package TEGRA
private

46 constant DOT
47 constant SLASH
64 constant AT
67 constant UC                          \ 'C'
37 constant PCT                         \ '%'
77 constant UM                          \ 'M'
66 constant UB                          \ 'B'
109 constant LM                         \ 'm'
87 constant UW                          \ 'W'

\ ---- parsed fields (integers; cpu/gpu/temp are tenths) --------------------
variable RAM-AVAIL  variable RAM-TOTAL  variable RAM?
variable SWAP-USED  variable SWAP-TOTAL variable SWAP?
variable CPU10      variable CPU?
variable GPU10      variable GPU?
variable TEMP10     variable TEMP?
variable POWER      variable POWER?

: RESET ( -- )
   0 RAM? !  0 SWAP? !  0 CPU? !  0 GPU? !  0 TEMP? !  0 POWER? ! ;

\ ---- small helpers --------------------------------------------------------
: IMAX ( n n -- n ) {: a:n b:n :} a b < if b else a then ;
: F>TENTHS ( r -- n ) 10.0 f* 0.5 f+ f>s ;     \ round(x*10) toward zero (positives exact)
: CHAR= ( ptr u8 n n n -- bool ) {: a:ptr u:n idx:n ch:n :}   \ a[idx]==ch, bounds-safe
   idx 0 >= idx u < and if a idx + c@ ch = else 0 0= 0= then ;

\ ---- whitespace tokenizer (single POS cursor) -----------------------------
variable POS
: SEP? ( n -- bool )
   dup STR-SPACE = over STR-TAB = or over STR-CR = or swap STR-LF = or ;
: AT-SEP? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx u < if a idx + c@ SEP? else 0 0= 0= then ;
: AT-TOK? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx u < if a idx + c@ SEP? 0= else 0 0= 0= then ;
: SKIP-SEPS ( ptr u8 n -- ) {: a:ptr u:n :}      \ advance POS past separators
   begin a u POS @ AT-SEP? while POS @ 1+ POS ! repeat ;
: MORE? ( ptr u8 n -- bool ) {: a:ptr u:n :}      \ token available after skipping seps
   a u SKIP-SEPS  POS @ u < ;
: TOK ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}    \ token at the cursor; advance past it
   POS @ {: tstart:n :}
   begin a u POS @ AT-TOK? while POS @ 1+ POS ! repeat
   a tstart +  POS @ tstart - ;
: NEXT-TOK ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u:n :}  \ skip seps, take a token, found?
   a u SKIP-SEPS
   a u TOK  dup 0 > ;

\ ---- numeric prefix parsing -----------------------------------------------
: DIGITS-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}    \ count leading decimal digits
   0 begin dup u < if a over + c@ STR-DIGIT? else 0 0= 0= then while 1+ repeat ;
: U-PREFIX ( ptr u8 n -- n n ) {: a:ptr u:n :}    \ leading-digit value + its length
   a u DIGITS-LEN {: dl:n :}
   a dl STR>NUMBER? drop  dl ;

\ ---- token field parsers --------------------------------------------------
\ "used/totalMB" -> used total bool
: MBPAIR ( ptr u8 n -- n n bool ) {: a:ptr u:n :}
   a u U-PREFIX {: used:n ulen:n :}
   a ulen 1+ + u ulen 1+ - U-PREFIX {: total:n tlen:n :}
   ulen 1+ tlen + {: mbpos:n :}
   used total
   ulen 0 >
   a u ulen SLASH CHAR= and
   tlen 0 > and
   a u mbpos UM CHAR= and
   a u mbpos 1+ UB CHAR= and ;

\ "<n>mW..." -> mW bool
: MW ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u U-PREFIX {: val:n vlen:n :}
   val
   vlen 0 >
   a u vlen LM CHAR= and
   a u vlen 1+ UW CHAR= and ;

\ "<float>%" -> percent bool
: PERCENT ( ptr u8 n -- r bool ) {: a:ptr u:n :}
   a u PCT INDEX-OF {: pct:n :}
   pct 0 < if 0.0 0 0= 0= exit then
   a pct STR>FLOAT ;

\ "...@<float>C..." -> temp bool
: TEMP-AT ( ptr u8 n n -- r bool ) {: a:ptr u:n at:n :}
   a at 1+ + u at 1+ - UC INDEX-OF {: crel:n :}
   crel 0 < if 0.0 0 0= 0= exit then
   a at 1+ + crel STR>FLOAT ;
: TEMP ( ptr u8 n -- r bool ) {: a:ptr u:n :}
   a u AT INDEX-OF {: at:n :}
   at 0 < if 0.0 0 0= 0= exit then
   a u at TEMP-AT ;

\ CPU list "[25%@1497,12%@1497,...]" -> average% bool
variable CI  variable CNT  variable NS
: CPU-STEP ( r ptr u8 n n -- r ) {: a:ptr u:n idx:n :}
   a idx + c@ {: c:n :}
   c STR-DIGIT? c DOT = or if
      NS @ 0 < if idx NS ! then exit
   then
   c PCT = NS @ 0 >= and if
      a NS @ + idx NS @ - STR>FLOAT
      if CNT @ 1+ CNT ! f+ else drop then
   then
   -1 NS ! ;
: CPU-LIST ( ptr u8 n -- r bool ) {: a:ptr u:n :}
   0.0
   0 CNT !  -1 NS !  0 CI !
   begin CI @ u < while
      a u CI @ CPU-STEP
      CI @ 1+ CI !
   repeat
   CNT @ 0= if drop 0.0 0 0= 0= exit then
   CNT @ s>f f/  0 0= ;

\ ---- field setters --------------------------------------------------------
: SET-RAM ( ptr u8 n -- ) MBPAIR {: used:n total:n ok:bool :}
   ok if total used - RAM-AVAIL ! total RAM-TOTAL ! -1 RAM? ! then ;
: SET-SWAP ( ptr u8 n -- ) MBPAIR {: used:n total:n ok:bool :}
   ok if used SWAP-USED ! total SWAP-TOTAL ! -1 SWAP? ! then ;
: SET-CPU ( ptr u8 n -- ) CPU-LIST {: ok:bool :}
   ok if F>TENTHS CPU10 ! -1 CPU? ! else drop then ;
: SET-GPU ( ptr u8 n -- ) PERCENT {: ok:bool :}
   ok if F>TENTHS GPU10 ! -1 GPU? ! else drop then ;
: SET-POWER ( ptr u8 n -- ) MW {: val:n ok:bool :}
   ok if val POWER ! -1 POWER? ! then ;
: SET-TEMP ( ptr u8 n -- ) TEMP {: ok:bool :}
   ok if
      F>TENTHS
      TEMP? @ if TEMP10 @ IMAX then
      TEMP10 !  -1 TEMP? !
   else drop then ;

\ ---- dispatch + parse -----------------------------------------------------
: DISPATCH ( ptr u8 n ptr u8 n -- ) {: ta:ptr tu:n la:ptr lu:n :}
   ta tu s" RAM"       STR= if la lu NEXT-TOK drop SET-RAM   exit then
   ta tu s" SWAP"      STR= if la lu NEXT-TOK drop SET-SWAP  exit then
   ta tu s" CPU"       STR= if la lu NEXT-TOK drop SET-CPU   exit then
   ta tu s" GR3D_FREQ" STR= if la lu NEXT-TOK drop SET-GPU   exit then
   ta tu s" VDD_IN"    STR= if la lu NEXT-TOK drop SET-POWER exit then
   ta tu SET-TEMP ;
: PARSE ( ptr u8 n -- ) {: a:ptr u:n :}
   RESET  0 POS !
   begin a u MORE? while
      a u TOK  a u DISPATCH
   repeat ;

\ ---- render (into SB) -----------------------------------------------------
: SB-TENTHS ( n -- ) {: t:n :}  t 10 / SB-INT DOT SB-APPEND-C  t 10 mod SB-U ;
: SB-RAM ( -- )
   s" ram=" SB-APPEND  RAM-AVAIL @ SB-U  s" /" SB-APPEND
   RAM-TOTAL @ SB-U  s" MB " SB-APPEND ;
: SB-SWAP ( -- )
   s" swap=" SB-APPEND  SWAP-USED @ SB-U  s" /" SB-APPEND
   SWAP-TOTAL @ SB-U  s" MB " SB-APPEND ;
: SB-CPU ( -- )  s" cpu=" SB-APPEND  CPU10 @ SB-TENTHS  s" % " SB-APPEND ;
: SB-GPU ( -- )  s" gpu=" SB-APPEND  GPU10 @ SB-TENTHS  s" % " SB-APPEND ;
: SB-TEMP ( -- ) s" temp=" SB-APPEND  TEMP10 @ SB-TENTHS  s" C " SB-APPEND ;
: SB-POWER ( -- ) s" power=" SB-APPEND  POWER @ SB-U  s" mW" SB-APPEND ;
: RENDER ( -- ptr u8 n )
   SB-RESET
   RAM?   @ if SB-RAM   then
   SWAP?  @ if SB-SWAP  then
   CPU?   @ if SB-CPU   then
   GPU?   @ if SB-GPU   then
   TEMP?  @ if SB-TEMP  then
   POWER? @ if SB-POWER then
   SB$ ;

public

\ parse a line, render the summary (empty span if no fields)
: SUMMARY ( ptr u8 n -- ptr u8 n )
   PARSE  RENDER ;

end-package
