\ maki/bpe-test.f - acceptance for the byte-level BPE tokenizer (maki/bpe.f), dot
\ habu-bpe-tokenizer-gpt-37d7f243. Proves, against the committed synthetic fixture
\ (maki/bpe-data.f): (1) ENCODE reproduces the pinned exact token-id sequences and
\ DECODE(ENCODE(x))==x for arbitrary bytes incl. invalid UTF-8; (2) BPE-TRAIN on the
\ committed corpus deterministically reproduces the locked merge table; plus the
\ hand-rolled pre-split matcher's chunk boundaries match the GPT-2 regex reference,
\ and every public word is readiness-gated and domain-validated red-first with named
\ E-codes, no partial writes on rejection (canaries), and exact-capacity success.
\ Names are BPT-prefixed (the maki suite shares one dictionary across every -test.f).

require lib/test.f
require lib/prelude.f
require maki/examples/nanogpt/bpe.f
require maki/examples/nanogpt/bpe-data.f
require maki/examples/nanogpt/bpe-real-data.f

package MAKI

512 constant BPT-CAP
create BPT-IDS BPT-CAP cells allot     \ encoded ids (float cells)
create BPT-BK  BPT-CAP allot           \ decoded bytes

\ ---- parity: encode text, compare to a pinned int id vector ----------------------
: BPT-ENC-EQ? ( ptr a n ptr u8 n -- bool )  {: ea:ptr en:n s:ptr slen:n :}
   s slen BPT-IDS BPT-CAP BPE-ENCODE {: m:n :}
   m en <> if false exit then
   en 0 ?do
      BPT-IDS i T-GET f>s  ea i cells + @  <> if false unloop exit then
   loop true ;
: BPT-PARITY? ( -- bool )
   BPD-I1 BPD-N1 BPD-S1 BPT-ENC-EQ?
   BPD-I2 BPD-N2 BPD-S2 BPT-ENC-EQ? and
   BPD-I3 BPD-N3 BPD-S3 BPT-ENC-EQ? and
   BPD-I4 BPD-N4 BPD-S4 BPT-ENC-EQ? and
   BPD-I5 BPD-N5 BPD-S5 BPT-ENC-EQ? and ;

\ ---- byte-level round-trip: decode(encode(x)) == x -------------------------------
: BPT-RT? ( ptr u8 n -- bool )  {: a:ptr u:n :}
   a u BPT-IDS BPT-CAP BPE-ENCODE {: m:n :}
   BPT-IDS m BPT-BK BPT-CAP BPE-DECODE {: nb:n :}
   nb u =  BPT-BK nb a u T-STR=  and ;
: BPT-RT-CORPUS?  ( -- bool )  BPD-CORPUS BPT-RT? ;
: BPT-RT-EMPTY?   ( -- bool )  BPD-CORPUS drop 0 BPT-RT? ;   \ zero-length input -> zero tokens

\ property: random corpora (bytes 0..255, arbitrary incl. invalid UTF-8) round-trip
variable BPT-RNG
create BPT-RSRC 64 allot
: BPT-RND  ( -- n )  BPT-RNG @ 1103515245 * 12345 + $7FFFFFFF and dup BPT-RNG ! ;
: BPT-RLEN ( -- n )  BPT-RND 48 mod 1+ ;
: BPT-FILL ( n -- )  0 ?do  BPT-RND 256 mod BPT-RSRC i + c!  loop ;
: BPT-TRIAL? ( -- bool )  BPT-RLEN {: L:n :}  L BPT-FILL  BPT-RSRC L BPT-RT? ;
: BPT-PROP? ( -- bool )
   1 BPT-RNG !
   80 0 ?do  BPT-TRIAL? 0= if false unloop exit then  loop  true ;

\ ---- deliverable-2 lock: training reproduces the committed table exactly ----------
: BPT-LOCK? ( -- bool )
   BPD-CORPUS 64 BPE-TRAIN {: n:n :}
   n BPD-MERGES <> if false exit then
   BPD-MERGES 0 ?do
      i BPE-MERGE-A  BPD-A i cells + @  <> if false unloop exit then
      i BPE-MERGE-B  BPD-B i cells + @  <> if false unloop exit then
   loop true ;

\ ---- vocab structure: 256 bytes + N merges + 1 special; EOT is the top id ---------
: BPT-STRUCT? ( -- bool )
   BPE-MERGES 14 =
   BPE-VOCAB 271 =  and       \ 256 + 14 + 1
   BPE-EOT 270 =  and ;       \ 256 + 14

\ ---- pre-split matcher parity (chunk-length sequence vs the GPT-2 regex reference) -
variable BPT-SPOS  variable BPT-SC
: BPT-SPLIT? ( ptr a n ptr u8 n -- bool )  {: el:ptr nc:n s:ptr slen:n :}
   0 BPT-SPOS !  0 BPT-SC !
   begin BPT-SPOS @ slen < while
      s slen BPT-SPOS @ BPE-CHUNK-LEN {: cl:n :}
      BPT-SC @ nc >= if false exit then
      cl  el BPT-SC @ cells + @  <> if false exit then
      BPT-SPOS @ cl + BPT-SPOS !
      BPT-SC @ 1+ BPT-SC !
   repeat
   BPT-SC @ nc = ;
create BPT-L1  2 , 3 , 1 , 3 , 4 ,          \ "To be, or not"  -> To| be|,| or| not
create BPT-L2  1 , 3 ,                        \ "  hi"           -> ' '| hi
create BPT-L3  1 , 1 , 2 ,                    \ "\t\thi"         -> tab|tab|hi
create BPT-L4  2 , 3 , 4 , 2 , 2 , 2 ,        \ "we'll can't I'm"-> we|'ll| can|'t| I|'m
create BPT-L5  3 , 3 , 2 ,                    \ "abc123!!"       -> abc|123|!!
create BPT-L6  3 , 1 , 2 ,                    \ "end.  "         -> end|.|'  '
: BPT-SPLITS? ( -- bool )
   BPT-L1 5 s" To be, or not"    BPT-SPLIT?
   BPT-L2 2 s"   hi"             BPT-SPLIT? and
   BPT-L3 3 s\" \t\thi"          BPT-SPLIT? and
   BPT-L4 6 s" we'll can't I'm"  BPT-SPLIT? and
   BPT-L5 3 s" abc123!!"         BPT-SPLIT? and
   BPT-L6 3 s" end.  "           BPT-SPLIT? and ;

\ ---- red-first guards -------------------------------------------------------------
\ pre-ready: every operational/query word rejects before any table is built
: BPT-PRE-ENC  ( -- )  BPD-S1 BPT-IDS BPT-CAP BPE-ENCODE drop ;
: BPT-PRE-DEC  ( -- )  BPT-IDS 1 BPT-BK BPT-CAP BPE-DECODE drop ;
: BPT-PRE-MRG  ( -- )  BPE-MERGES drop ;
: BPT-PRE-VOC  ( -- )  BPE-VOCAB drop ;
: BPT-PRE-EOT  ( -- )  BPE-EOT drop ;
: BPT-PRE-MA   ( -- )  0 BPE-MERGE-A drop ;
: BPT-PRE-MB   ( -- )  0 BPE-MERGE-B drop ;
\ build-state: merge/seal out of order (no live BPE-BEGIN)
: BPT-BAD-MERGE+ ( -- )  0 0 BPE-MERGE+ ;
: BPT-BAD-SEAL   ( -- )  BPE-SEAL ;

\ decode float-cell value validation (finite, exactly integral, in [0,256+N))
: BPT-NANF  ( -- r )   0.0 0.0 f/ ;
: BPT-INFF  ( -- r )   1.0 0.0 f/ ;
: BPT-NINFF ( -- r )  -1.0 0.0 f/ ;
: BPT-DEC-1BAD ( r -- )  BPT-IDS 0 T-SET  BPT-IDS 1 BPT-BK BPT-CAP BPE-DECODE drop ;
: BPT-DEC-FRAC   ( -- )  2.5 BPT-DEC-1BAD ;
: BPT-DEC-NAN    ( -- )  BPT-NANF  BPT-DEC-1BAD ;
: BPT-DEC-POSINF ( -- )  BPT-INFF  BPT-DEC-1BAD ;
: BPT-DEC-NEGINF ( -- )  BPT-NINFF BPT-DEC-1BAD ;
: BPT-DEC-NEGID  ( -- )  -3.0 BPT-DEC-1BAD ;
: BPT-DEC-HUGE   ( -- )  1000000.0 BPT-DEC-1BAD ;
: BPT-DEC-EOT    ( -- )  BPE-EOT s>f BPT-DEC-1BAD ;   \ the special carries no bytes

\ length / capacity boundaries
: BPT-ENC-NEGLEN ( -- )  BPD-S1 drop -1 BPT-IDS BPT-CAP BPE-ENCODE drop ;
: BPT-ENC-NEGCAP ( -- )  BPD-S1 BPT-IDS -1 BPE-ENCODE drop ;
: BPT-ENC-OVER   ( -- )  s" not to be" BPT-IDS 2 BPE-ENCODE drop ;   \ cap 2 < token count
: BPT-DEC-NEGLEN ( -- )  BPT-IDS -1 BPT-BK BPT-CAP BPE-DECODE drop ;
: BPT-DEC-NEGCAP ( -- )  BPT-IDS 1 BPT-BK -1 BPE-DECODE drop ;
: BPT-DEC-OVER   ( -- )
   BPD-S4 BPT-IDS BPT-CAP BPE-ENCODE {: m:n :}
   BPT-IDS m BPT-BK 2 BPE-DECODE drop ;                 \ decoded bytes > cap 2

\ malformed merges (inside a live build)
: BPT-MRG-BADCHILD ( -- )  BPE-BEGIN  0 300 BPE-MERGE+ ;   \ child id 300 not yet a token
: BPT-MRG-NEGCHILD ( -- )  BPE-BEGIN  -1 0 BPE-MERGE+ ;
: BPT-MRG-DUP      ( -- )  BPE-BEGIN  0 1 BPE-MERGE+  0 1 BPE-MERGE+ ;   \ duplicate pair

\ corpus guards
: BPT-TR-EMPTY   ( -- )  BPD-CORPUS drop 0 64 BPE-TRAIN drop ;
: BPT-TR-NEGK    ( -- )  BPD-CORPUS -1 BPE-TRAIN drop ;

\ ---- exact-capacity success + canaries (no partial write on rejection) ------------
: BPT-ENC-EXACT? ( -- bool )
   BPD-S2 BPT-IDS BPT-CAP BPE-ENCODE {: m:n :}
   171 s>f BPT-IDS m T-SET                                \ guard cell just past output
   BPD-S2 BPT-IDS m BPE-ENCODE {: p:n :}                  \ cap == exact token count
   p m =
   BPT-IDS m T-GET f>s 171 =  and ;                       \ guard untouched
: BPT-DEC-EXACT? ( -- bool )
   BPD-S2 BPT-IDS BPT-CAP BPE-ENCODE {: m:n :}
   BPT-IDS m BPT-BK BPT-CAP BPE-DECODE {: nb:n :}
   171 BPT-BK nb + c!                                     \ guard byte just past output
   BPT-IDS m BPT-BK nb BPE-DECODE {: q:n :}               \ cap == exact byte count
   q nb =
   BPT-BK nb + c@ 171 =  and
   BPT-BK nb BPD-S2 T-STR=  and ;

72 constant BPT-CANN
171 constant BPT-SENT
create BPT-CANB BPT-CANN allot           \ byte dest canary (decode)
create BPT-CANF BPT-CANN cells allot     \ float-cell dest canary (encode)
: BPT-CANB-FILL   ( -- )  BPT-CANN 0 ?do  BPT-SENT BPT-CANB i + c!  loop ;
: BPT-CANB-CLEAN? ( -- bool )
   BPT-CANN 0 ?do  BPT-CANB i + c@ BPT-SENT <> if false unloop exit then  loop  true ;
: BPT-CANF-FILL   ( -- )  BPT-CANN 0 ?do  BPT-SENT s>f BPT-CANF i T-SET  loop ;
: BPT-CANF-CLEAN? ( -- bool )
   BPT-CANN 0 ?do  BPT-CANF i T-GET f>s BPT-SENT <> if false unloop exit then  loop  true ;
: BPT-CAN-ENC-CAP? ( -- bool )   \ encode overflow leaves the caller cell buffer untouched
   BPT-CANF-FILL
   [: s" not to be" BPT-CANF 2 BPE-ENCODE drop ;] catch
   E-BPE-CAP =  BPT-CANF-CLEAN?  and ;
variable BPT-M                    \ id count shared into a catch quotation (no local closure)
: BPT-CAN-DEC-CAP? ( -- bool )   \ decode overflow leaves the caller byte buffer untouched
   BPT-CANB-FILL
   BPD-S4 BPT-IDS BPT-CAP BPE-ENCODE BPT-M !
   [: BPT-IDS BPT-M @ BPT-CANB 2 BPE-DECODE drop ;] catch
   E-BPE-CAP =  BPT-CANB-CLEAN?  and ;
: BPT-CAN-DEC-BAD? ( -- bool )   \ a rejected id leaves the caller byte buffer untouched
   BPT-CANB-FILL
   2.5 BPT-IDS 0 T-SET
   [: BPT-IDS 1 BPT-CANB BPT-CANN BPE-DECODE drop ;] catch
   E-BPE-RANGE =  BPT-CANB-CLEAN?  and ;

\ ==== real GPT-2 vocab (maki/bpe-real.f) ==========================================
\ Fixtures + the fired-merge subset are committed real-vocab data (maki/bpe-real-data.f);
\ the id vectors are exact tiktoken-gpt2 sequences (provenance there). Proves BPR-ENCODE
\ reproduces them, real-id round-trip, the measured pre-split divergence boundary, and
\ the loader's ready-gating / domain / bijection guards. BPR-prefixed; reuses BPT-CAP.
create BPR-IDS BPT-CAP cells allot
create BPR-BK  BPT-CAP allot

: BPR-ENC-EQ? ( ptr a n ptr u8 n -- bool )  {: ea:ptr en:n s:ptr slen:n :}
   s slen BPR-IDS BPT-CAP BPR-ENCODE {: m:n :}
   m en <> if false exit then
   en 0 ?do
      BPR-IDS i T-GET f>s  ea i cells + @  <> if false unloop exit then
   loop true ;
: BPR-PARITY? ( -- bool )    \ every fixture encodes to its exact tiktoken id vector
   BPR-I1  BPR-N1  BPR-S1  BPR-ENC-EQ?
   BPR-I2  BPR-N2  BPR-S2  BPR-ENC-EQ? and
   BPR-I3  BPR-N3  BPR-S3  BPR-ENC-EQ? and
   BPR-I4  BPR-N4  BPR-S4  BPR-ENC-EQ? and
   BPR-I5  BPR-N5  BPR-S5  BPR-ENC-EQ? and
   BPR-I6  BPR-N6  BPR-S6  BPR-ENC-EQ? and
   BPR-I7  BPR-N7  BPR-S7  BPR-ENC-EQ? and
   BPR-I8  BPR-N8  BPR-S8  BPR-ENC-EQ? and
   BPR-I9  BPR-N9  BPR-S9  BPR-ENC-EQ? and
   BPR-I10 BPR-N10 BPR-S10 BPR-ENC-EQ? and
   BPR-I11 BPR-N11 BPR-S11 BPR-ENC-EQ? and       \ multi-byte MATCH from here
   BPR-I12 BPR-N12 BPR-S12 BPR-ENC-EQ? and
   BPR-I13 BPR-N13 BPR-S13 BPR-ENC-EQ? and
   BPR-I14 BPR-N14 BPR-S14 BPR-ENC-EQ? and
   BPR-I15 BPR-N15 BPR-S15 BPR-ENC-EQ? and
   BPR-I16 BPR-N16 BPR-S16 BPR-ENC-EQ? and ;

: BPR-RT? ( ptr u8 n -- bool )  {: a:ptr u:n :}   \ decode(encode(x))==x in real-id space
   a u BPR-IDS BPT-CAP BPR-ENCODE {: m:n :}
   BPR-IDS m BPR-BK BPT-CAP BPR-DECODE {: nb:n :}
   nb u =  BPR-BK nb a u T-STR=  and ;
: BPR-RT-SAMPLE? ( -- bool )  BPR-RT-S BPR-RT? ;   \ real multi-byte corpus sample
: BPR-RT-FIX? ( -- bool )     \ every multi-byte fixture (incl. the divergent one) round-trips
   BPR-S11 BPR-RT?  BPR-S12 BPR-RT? and  BPR-S13 BPR-RT? and
   BPR-S14 BPR-RT? and  BPR-S15 BPR-RT? and  BPR-S16 BPR-RT? and
   BPR-DIV-S BPR-RT? and ;

: BPR-STRUCT? ( -- bool )
   BPR-VOCAB 50257 =  BPR-EOT 50256 =  and  BPR-MERGES BPR-D-MERGES =  and ;

\ boundary proof: the byte-level matcher output for "naïve" equals the pinned matcher
\ ids AND differs from the pinned tiktoken (unicode pre-split) ids - measured, not silent.
: BPR-DIV? ( -- bool )
   BPR-DIV-HABU BPR-DIV-HABU-N BPR-DIV-S BPR-ENC-EQ?
   BPR-DIV-TIK  BPR-DIV-TIK-N  BPR-DIV-S BPR-ENC-EQ? 0=  and ;

\ pre-ready: every real-vocab public word rejects before BPR-INSTALL (even though the
\ engine is already sealed from the synthetic-table tests above)
: BPR-PRE-ENC ( -- )  BPR-S1 BPR-IDS BPT-CAP BPR-ENCODE drop ;
: BPR-PRE-DEC ( -- )  BPR-IDS 1 BPR-BK BPT-CAP BPR-DECODE drop ;
: BPR-PRE-VOC ( -- )  BPR-VOCAB drop ;
: BPR-PRE-EOT ( -- )  BPR-EOT drop ;
: BPR-PRE-MRG ( -- )  BPR-MERGES drop ;

\ decode real-id domain validation (finite, exactly integral, in [0,50257), in the map)
: BPR-DEC-1BAD ( r -- )  BPR-IDS 0 T-SET  BPR-IDS 1 BPR-BK BPT-CAP BPR-DECODE drop ;
: BPR-DEC-FRAC  ( -- )  2.5      BPR-DEC-1BAD ;
: BPR-DEC-NEG   ( -- )  -1.0     BPR-DEC-1BAD ;
: BPR-DEC-HUGE  ( -- )  50257.0  BPR-DEC-1BAD ;   \ >= vocab
: BPR-DEC-EOT   ( -- )  50256.0  BPR-DEC-1BAD ;   \ the special: in range but carries no bytes / absent from map
: BPR-DEC-UNMAP ( -- )  49999.0  BPR-DEC-1BAD ;   \ in range but outside the loaded subset map
: BPR-DEC-NEGLEN ( -- )  BPR-IDS -1 BPR-BK BPT-CAP BPR-DECODE drop ;

\ malformed vocab: a bad byte-id table is rejected BEFORE any mutation (fail-closed), so
\ the loaded real vocab survives. Scratch byte-id table seeded identity 0..255, then broken.
create BPR-BADBID 256 cells allot
: BPR-BADBID-ID ( -- )  256 0 ?do  i BPR-BADBID i cells + !  loop ;
: BPR-INST-BADRANGE ( -- )        \ entry out of [0,50257)
   BPR-BADBID-ID  50257 BPR-BADBID 0 cells + !
   BPR-BADBID BPR-D-MA BPR-D-MB BPR-D-MID BPR-D-MERGES BPR-INSTALL ;
: BPR-INST-DUP ( -- )             \ duplicate byte id breaks the bijection
   BPR-BADBID-ID  0 BPR-BADBID 5 cells + !
   BPR-BADBID BPR-D-MA BPR-D-MB BPR-D-MID BPR-D-MERGES BPR-INSTALL ;

\ canaries: a rejected real-vocab call writes no caller element
: BPR-CAN-ENC-CAP? ( -- bool )    \ encode overflow leaves the caller cell buffer untouched
   BPT-CANF-FILL
   [: BPR-S8 BPT-CANF 2 BPR-ENCODE drop ;] catch
   E-BPE-CAP =  BPT-CANF-CLEAN?  and ;
: BPR-CAN-DEC-BAD? ( -- bool )    \ a rejected real id leaves the caller byte buffer untouched
   BPT-CANB-FILL
   2.5 BPR-IDS 0 T-SET
   [: BPR-IDS 1 BPT-CANB BPT-CANN BPR-DECODE drop ;] catch
   E-BPR-RANGE =  BPT-CANB-CLEAN?  and ;

T-RESET

\ (1) pre-ready + build-state guards MUST hold before any table is built
' BPT-PRE-ENC     E-BPE-EMPTY TTHROWS
' BPT-PRE-DEC     E-BPE-EMPTY TTHROWS
' BPT-PRE-MRG     E-BPE-EMPTY TTHROWS
' BPT-PRE-VOC     E-BPE-EMPTY TTHROWS
' BPT-PRE-EOT     E-BPE-EMPTY TTHROWS
' BPT-PRE-MA      E-BPE-EMPTY TTHROWS
' BPT-PRE-MB      E-BPE-EMPTY TTHROWS
' BPT-BAD-MERGE+  E-BPE-STATE TTHROWS
' BPT-BAD-SEAL    E-BPE-STATE TTHROWS

\ (2) pre-split matcher parity (needs no table)
BPT-SPLITS? TTRUE

\ (3) load the committed fixture table, then prove encode parity + round-trip
BPD-LOAD
BPT-STRUCT?      TTRUE
BPT-PARITY?      TTRUE
BPT-RT-CORPUS?   TTRUE
BPT-RT-EMPTY?    TTRUE
BPT-PROP?        TTRUE
BPT-ENC-EXACT?   TTRUE
BPT-DEC-EXACT?   TTRUE

\ (4) decode value validation + capacity guards (table ready)
' BPT-DEC-FRAC    E-BPE-RANGE TTHROWS
' BPT-DEC-NAN     E-BPE-RANGE TTHROWS
' BPT-DEC-POSINF  E-BPE-RANGE TTHROWS
' BPT-DEC-NEGINF  E-BPE-RANGE TTHROWS
' BPT-DEC-NEGID   E-BPE-RANGE TTHROWS
' BPT-DEC-HUGE    E-BPE-RANGE TTHROWS
' BPT-DEC-EOT     E-BPE-RANGE TTHROWS
' BPT-ENC-NEGLEN  E-BPE-CAP   TTHROWS
' BPT-ENC-NEGCAP  E-BPE-CAP   TTHROWS
' BPT-ENC-OVER    E-BPE-CAP   TTHROWS
' BPT-DEC-NEGLEN  E-BPE-CAP   TTHROWS
' BPT-DEC-NEGCAP  E-BPE-CAP   TTHROWS
' BPT-DEC-OVER    E-BPE-CAP   TTHROWS

\ (5) canaries: a rejected call writes no caller element
BPT-CAN-ENC-CAP? TTRUE
BPT-CAN-DEC-CAP? TTRUE
BPT-CAN-DEC-BAD? TTRUE

\ (6) deliverable-2: training reproduces the committed locked table (re-seals ready)
BPT-LOCK?        TTRUE
BPT-PARITY?      TTRUE       \ the trained table encodes identically to the fixture

\ (7) malformed-merge + corpus guards LAST (they leave a dirty half-build)
' BPT-MRG-BADCHILD E-BPE-MERGE  TTHROWS
' BPT-MRG-NEGCHILD E-BPE-MERGE  TTHROWS
' BPT-MRG-DUP      E-BPE-MERGE  TTHROWS
' BPT-TR-EMPTY     E-BPE-CORPUS TTHROWS
' BPT-TR-NEGK      E-BPE-CORPUS TTHROWS

\ (8) real GPT-2 vocab: pre-ready guards, then load the committed real subset and prove
\ exact tiktoken parity (ASCII + multi-byte), real-id round-trip, the measured pre-split
\ boundary, and the loader domain / bijection / canary guards. BPR-D-LOAD re-seals the
\ engine (a fresh BPE-BEGIN..SEAL), recovering the dirty half-build section (7) leaves.
' BPR-PRE-ENC  E-BPR-EMPTY TTHROWS
' BPR-PRE-DEC  E-BPR-EMPTY TTHROWS
' BPR-PRE-VOC  E-BPR-EMPTY TTHROWS
' BPR-PRE-EOT  E-BPR-EMPTY TTHROWS
' BPR-PRE-MRG  E-BPR-EMPTY TTHROWS

BPR-D-LOAD
BPR-STRUCT?      TTRUE
BPR-PARITY?      TTRUE
BPR-RT-SAMPLE?   TTRUE
BPR-RT-FIX?      TTRUE
BPR-DIV?         TTRUE
BPR-CAN-ENC-CAP? TTRUE
BPR-CAN-DEC-BAD? TTRUE

' BPR-DEC-FRAC      E-BPR-RANGE TTHROWS
' BPR-DEC-NEG       E-BPR-RANGE TTHROWS
' BPR-DEC-HUGE      E-BPR-RANGE TTHROWS
' BPR-DEC-EOT       E-BPR-RANGE TTHROWS
' BPR-DEC-UNMAP     E-BPR-RANGE TTHROWS
' BPR-DEC-NEGLEN    E-BPR-CAP   TTHROWS
' BPR-INST-BADRANGE E-BPR-VOCAB TTHROWS
' BPR-INST-DUP      E-BPR-VOCAB TTHROWS
BPR-PARITY?      TTRUE        \ malformed-vocab rejections left the loaded real vocab intact

T-REPORT

;package
