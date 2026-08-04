\ gpt2-token.f - model-owned GPT-2 byte-pair tokenizer state.

require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require lib/memory.f
require lib/string.f
require lib/unicode/class.f
require src/core/sha256.f
require maki/infer/gpt2-pin.f

package GPT2
private

-5304 constant E-TOK-RANGE
-5324 constant E-TOK-CAP
-5308 constant E-TOK-VOCAB
-5309 constant E-TOK-IO
-5664 constant E-TOK-DIGEST

256 constant T-BYTE-N
50000 constant T-MERGE-N
50256 constant T-GID-N
50257 constant T-VOCAB-N
50256 constant T-EOT
4096 constant T-ID-CAP
1024 constant T-WORK-CAP
8192 constant T-DEC-CAP
400000 constant T-ARENA-CAP
131072 constant T-HCAP
131072 constant T-SCAP
32 constant T-DIGEST-LEN
64 constant T-HEX-LEN
T-DIGEST-LEN T-HEX-LEN + constant T-DIGEST-CAP

$CBF29CE484222325 constant T-FNV-OFF
$100000001B3 constant T-FNV-PRIME
50257 constant T-KSTRIDE
$9E3779B97F4A7C15 constant T-HMIX

0 constant T-N
1 constant T-WLEN
2 constant T-BEST
3 constant T-WI
4 constant T-RI
5 constant T-OUTN
6 constant T-POS
7 constant T-DECN
8 constant T-AN
9 constant T-NP
10 constant T-READY
16 constant T-HDR-N

T-HDR-N constant T-A
T-A T-MERGE-N + constant T-B
T-B T-MERGE-N + constant T-HK
T-HK T-HCAP + constant T-HU
T-HU T-HCAP + constant T-HV
T-HV T-HCAP + constant T-WORK
T-WORK T-WORK-CAP + constant T-OUT
T-OUT T-ID-CAP + constant T-GID
T-GID T-GID-N + constant T-R2I
T-R2I T-VOCAB-N + constant T-INT
T-INT T-ID-CAP + constant T-DEC
T-DEC T-DEC-CAP + constant T-ARENA
T-ARENA T-ARENA-CAP + constant T-SUSED
T-SUSED T-SCAP + constant T-SOFF
T-SOFF T-SCAP + constant T-SLEN
T-SLEN T-SCAP + constant T-SVAL
T-SVAL T-SCAP + constant T-IDS
T-IDS T-ID-CAP + constant T-LOGITS
50257 4 * 1 cells 1- + 1 cells / constant T-LOGIT-CELLS
T-LOGITS T-LOGIT-CELLS + constant T-CELLS

create T-BYTE-ID
   188 , 189 , 190 , 191 , 192 , 193 , 194 , 195 , 196 , 197 , 198 , 199 ,
   200 , 201 , 202 , 203 , 204 , 205 , 206 , 207 , 208 , 209 , 210 , 211 ,
   212 , 213 , 214 , 215 , 216 , 217 , 218 , 219 , 220 , 0 , 1 , 2 , 3 , 4 ,
   5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 20 ,
   21 , 22 , 23 , 24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 35 ,
   36 , 37 , 38 , 39 , 40 , 41 , 42 , 43 , 44 , 45 , 46 , 47 , 48 , 49 , 50 ,
   51 , 52 , 53 , 54 , 55 , 56 , 57 , 58 , 59 , 60 , 61 , 62 , 63 , 64 , 65 ,
   66 , 67 , 68 , 69 , 70 , 71 , 72 , 73 , 74 , 75 , 76 , 77 , 78 , 79 , 80 ,
   81 , 82 , 83 , 84 , 85 , 86 , 87 , 88 , 89 , 90 , 91 , 92 , 93 , 221 ,
   222 , 223 , 224 , 225 , 226 , 227 , 228 , 229 , 230 , 231 , 232 , 233 ,
   234 , 235 , 236 , 237 , 238 , 239 , 240 , 241 , 242 , 243 , 244 , 245 ,
   246 , 247 , 248 , 249 , 250 , 251 , 252 , 253 , 254 , 94 , 95 , 96 , 97 ,
   98 , 99 , 100 , 101 , 102 , 103 , 104 , 105 , 255 , 106 , 107 , 108 , 109 ,
   110 , 111 , 112 , 113 , 114 , 115 , 116 , 117 , 118 , 119 , 120 , 121 ,
   122 , 123 , 124 , 125 , 126 , 127 , 128 , 129 , 130 , 131 , 132 , 133 ,
   134 , 135 , 136 , 137 , 138 , 139 , 140 , 141 , 142 , 143 , 144 , 145 ,
   146 , 147 , 148 , 149 , 150 , 151 , 152 , 153 , 154 , 155 , 156 , 157 ,
   158 , 159 , 160 , 161 , 162 , 163 , 164 , 165 , 166 , 167 , 168 , 169 ,
   170 , 171 , 172 , 173 , 174 , 175 , 176 , 177 , 178 , 179 , 180 , 181 ,
   182 , 183 , 184 , 185 , 186 , 187 ,

: T-AT ( ptr a n -- ptr a ) cells + ;
: T@ ( ptr a n -- n ) T-AT @ ;
: T! ( n ptr a n -- ) T-AT ! ;
: TV@ ( ptr a n n -- n ) + T@ ;
: TV! ( n ptr a n n -- ) + T! ;

: T-TRUE ( -- bool ) 0 0= ;
: T-FALSE ( -- bool ) T-TRUE 0= ;

: T-NEW ( -- ptr a )
   T-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS ;

: T-FREE ( ptr a -- )
   T-CELLS >COUNT MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: T-READY? ( ptr a -- )
   T-READY T@ 0= if E-TOK-VOCAB throw then ;

\ bytes_to_unicode: byte -> codepoint, with non-printable bytes assigned in order.
: T-PRINT? ( n -- bool ) {: b:n :}
   b 33 >= b 126 <= and  b 161 >= b 172 <= and or
   b 174 >= b 255 <= and or ;

: T-CP ( ptr a n -- n ) {: t:ptr b:n :}
   b T-PRINT? if b exit then
   t T-NP T@ {: n:n :}
   n 1+ t T-NP T!
   256 n + ;

: T-AUTF8 ( n ptr a n -- n ) {: cp:n t:ptr off:n :}
   cp 128 < if cp t T-ARENA off + T! 1 exit then
   cp 6 rshift 192 or t T-ARENA off + T!
   cp 63 and 128 or t T-ARENA off 1+ + T!
   2 ;

: T-SHASH-STEP ( ptr u8 n n n -- n ) {: a:ptr u:n i:n h:n :}
   i u = if h exit then
   a i + c@ h xor T-FNV-PRIME * {: next:n :}
   a u i 1+ next recurse ;

: T-SHASH ( ptr u8 n -- n )
   0 T-FNV-OFF T-SHASH-STEP ;

: T-AHASH-STEP ( ptr a n n n n -- n ) {: t:ptr off:n u:n i:n h:n :}
   i u = if h exit then
   t T-ARENA off i + + T@ h xor T-FNV-PRIME * {: next:n :}
   t off u i 1+ next recurse ;

: T-AHASH ( ptr a n n -- n )
   0 T-FNV-OFF T-AHASH-STEP ;

: T-SEQ-STEP ( ptr a ptr u8 n n n -- bool ) {: t:ptr a:ptr u:n off:n i:n :}
   i u = if T-TRUE exit then
   a i + c@ t T-ARENA off i + + T@ <> if T-FALSE exit then
   t a u off i 1+ recurse ;

: T-SEQ? ( ptr a ptr u8 n n n -- bool ) {: t:ptr a:ptr u:n off:n len:n :}
   u len <> if T-FALSE exit then
   t a u off 0 T-SEQ-STEP ;

: T-AEQ-STEP ( ptr a n n n n -- bool ) {: t:ptr aoff:n boff:n u:n i:n :}
   i u = if T-TRUE exit then
   t T-ARENA aoff i + + T@ t T-ARENA boff i + + T@ <> if T-FALSE exit then
   t aoff boff u i 1+ recurse ;

: T-SFIND-STEP ( ptr a ptr u8 n n -- n ) {: t:ptr a:ptr u:n slot:n :}
   t T-SUSED slot TV@ 0= if slot exit then
   t a u t T-SOFF slot TV@ t T-SLEN slot TV@ T-SEQ? if slot exit then
   t a u slot 1+ T-SCAP 1- and recurse ;

: T-SFIND ( ptr a ptr u8 n -- n ) {: t:ptr a:ptr u:n :}
   t a u a u T-SHASH T-SCAP 1- and T-SFIND-STEP ;

: T-AFIND-STEP ( ptr a n n n -- n ) {: t:ptr off:n u:n slot:n :}
   t T-SUSED slot TV@ 0= if slot exit then
   t T-SLEN slot TV@ u = if
      t off t T-SOFF slot TV@ u 0 T-AEQ-STEP if slot exit then
   then
   t off u slot 1+ T-SCAP 1- and recurse ;

: T-AFIND ( ptr a n n -- n ) {: t:ptr off:n u:n :}
   t off u t off u T-AHASH T-SCAP 1- and T-AFIND-STEP ;

: T-SPUT-A ( ptr a n n n -- ) {: t:ptr off:n u:n id:n :}
   t off u T-AFIND {: s:n :}
   t T-SUSED s TV@ 0<> if E-TOK-VOCAB throw then
   1 t T-SUSED s TV!
   off t T-SOFF s TV!
   u t T-SLEN s TV!
   id t T-SVAL s TV! ;

: T-SGET ( ptr a ptr u8 n -- n ) {: t:ptr a:ptr u:n :}
   t a u T-SFIND {: s:n :}
   t T-SUSED s TV@ 0= if E-TOK-VOCAB throw then
   t T-SVAL s TV@ ;

: T-SEED-STEP ( ptr a n -- ) {: t:ptr b:n :}
   b T-BYTE-N = if exit then
   t T-AN T@ {: off:n :}
   t b T-CP t off T-AUTF8 {: u:n :}
   t off u b T-SPUT-A
   off u + t T-AN T!
   t b 1+ recurse ;

: T-SEED ( ptr a -- ) {: t:ptr :}
   0 t T-NP T! 0 t T-AN T!
   t 0 T-SEED-STEP ;

: T-NL ( ptr u8 n n -- n ) {: a:ptr u:n p:n :}
   p u >= if u exit then
   a p + c@ 10 = if p exit then
   a u p 1+ recurse ;

: T-SP ( ptr u8 n n -- n ) {: a:ptr p:n e:n :}
   p e >= if e exit then
   a p + c@ 32 = if p exit then
   a p 1+ e recurse ;

: T-HKEY ( n n -- n ) {: a:n b:n :}
   a T-KSTRIDE * b + T-HMIX * ;

: T-HPROBE ( ptr a n n -- n ) {: t:ptr key:n slot:n :}
   t T-HU slot TV@ 0= if slot exit then
   t T-HK slot TV@ key = if slot exit then
   t key slot 1+ T-HCAP 1- and recurse ;

: T-HSLOT ( ptr a n n -- n ) {: t:ptr a:n b:n :}
   a b T-HKEY {: key:n :}
   t key key T-HCAP 1- and T-HPROBE ;

: T-HPUT ( ptr a n n n -- ) {: t:ptr a:n b:n rank:n :}
   t a b T-HSLOT {: s:n :}
   t T-HU s TV@ 0<> if E-TOK-VOCAB throw then
   a b T-HKEY t T-HK s TV!
   rank t T-HV s TV!
   1 t T-HU s TV! ;

: T-RANK ( ptr a n n -- n ) {: t:ptr a:n b:n :}
   t a b T-HSLOT {: s:n :}
   t T-HU s TV@ 0= if -1 exit then
   t T-HV s TV@ ;

: T-LINE-COPY ( ptr a ptr u8 n n n n -- )
   {: t:ptr a:ptr ls:n sp:n le:n off:n :}
   sp ls - {: la:n :}
   le sp 1+ - {: lb:n :}
   la 0 ?do
      a ls + i + c@ {: byte:n :}
      T-ARENA off + i + {: dst:n :}
      byte t dst T!
   loop
   lb 0 ?do
      a sp 1+ + i + c@ {: byte:n :}
      T-ARENA off + la + i + {: dst:n :}
      byte t dst T!
   loop ;

: T-LINE ( ptr a ptr u8 n n n -- ) {: t:ptr a:ptr ls:n le:n rank:n :}
   a ls le T-SP {: sp:n :}
   sp le >= if E-TOK-VOCAB throw then
   t a ls + sp ls - T-SGET {: ia:n :}
   t a sp 1+ + le sp 1+ - T-SGET {: ib:n :}
   ia 0 < ia T-BYTE-N rank + >= or if E-TOK-VOCAB throw then
   ib 0 < ib T-BYTE-N rank + >= or if E-TOK-VOCAB throw then
   ia t T-A rank TV!
   ib t T-B rank TV!
   t ia ib rank T-HPUT
   sp ls - {: la:n :}
   le sp 1+ - {: lb:n :}
   t T-AN T@ {: off:n :}
   off la + lb + T-ARENA-CAP > if E-TOK-CAP throw then
   t a ls sp le off T-LINE-COPY
   t off la lb + T-BYTE-N rank + T-SPUT-A
   off la + lb + t T-AN T! ;

: T-PARSE-STEP ( ptr a ptr u8 n n n -- n )
   {: t:ptr a:ptr u:n p:n rank:n :}
   p u >= if rank exit then
   a u p T-NL {: le:n :}
   le p > if
      rank T-MERGE-N >= if E-TOK-CAP throw then
      t a p le rank T-LINE
      rank 1+
   else rank then {: next:n :}
   le 1+ u min {: np:n :}
   t a u np next recurse ;

: T-PARSE ( ptr a ptr u8 n -- ) {: t:ptr a:ptr u:n :}
   t T-SEED
   a u 0 T-NL {: h:n :}
   h u >= if E-TOK-VOCAB throw then
   t a u h 1+ 0 T-PARSE-STEP {: n:n :}
   n T-MERGE-N <> if E-TOK-VOCAB throw then
   n t T-N T! ;

: T-AUTH ( ptr u8 n ptr u8 n n ptr u8 -- )
   {: data:ptr u:n want:ptr wantu:n exact:n dig:ptr :}
   u exact <> if E-TOK-DIGEST throw then
   wantu T-HEX-LEN <> if E-TOK-DIGEST throw then
   data u dig SHA256
   dig dig T-DIGEST-LEN + SHA256>HEX
   dig T-DIGEST-LEN + T-HEX-LEN want wantu STR= 0= if E-TOK-DIGEST throw then ;

: T-VOCAB-BODY ( ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- ptr u8 n )
   {: root:ptr rootu:n buf:ptr alen:CAD-NUM:alloc-byte-len :}
   buf GPT2PIN:VOCAB-LEN T-DIGEST-CAP + + {: path:ptr :}
   root rootu GPT2PIN:VOCAB-NAME$ path JOIN-PATH {: pathu:n :}
   path pathu FILE? 0= if E-TOK-IO throw then
   path pathu buf GPT2PIN:VOCAB-LEN READ-ALL {: u:n :}
   buf u GPT2PIN:VOCAB-SHA256$ GPT2PIN:VOCAB-LEN buf GPT2PIN:VOCAB-LEN + T-AUTH
   root rootu ;

: T-VOCAB ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu
   GPT2PIN:VOCAB-LEN T-DIGEST-CAP + FS-PATH-CAP + MEM:BYTES-ALLOC-LEN
   [: T-VOCAB-BODY ;] MEM:WITH-BYTES
   2drop ;

: T-MERGES-BODY
   ( ptr a ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- ptr a ptr u8 n )
   {: t:ptr root:ptr rootu:n buf:ptr alen:CAD-NUM:alloc-byte-len :}
   buf GPT2PIN:MERGES-LEN T-DIGEST-CAP + + {: path:ptr :}
   root rootu GPT2PIN:MERGES-NAME$ path JOIN-PATH {: pathu:n :}
   path pathu FILE? 0= if E-TOK-IO throw then
   path pathu buf GPT2PIN:MERGES-LEN READ-ALL {: u:n :}
   buf u GPT2PIN:MERGES-SHA256$ GPT2PIN:MERGES-LEN buf GPT2PIN:MERGES-LEN + T-AUTH
   t buf u T-PARSE
   t root rootu ;

: T-MERGES ( ptr a ptr u8 n -- ) {: t:ptr root:ptr rootu:n :}
   t root rootu
   GPT2PIN:MERGES-LEN T-DIGEST-CAP + FS-PATH-CAP + MEM:BYTES-ALLOC-LEN
   [: T-MERGES-BODY ;] MEM:WITH-BYTES
   2drop drop ;

: T-IDMAP ( ptr a -- ) {: t:ptr :}
   T-VOCAB-N 0 ?do -1 t T-R2I i TV! loop
   T-BYTE-N 0 ?do
      T-BYTE-ID i cells + @ {: id:n :}
      id t T-GID i TV!
      i t T-R2I id TV!
   loop
   T-MERGE-N 0 ?do
      T-BYTE-N i + {: id:n :}
      id t T-GID id TV!
      id t T-R2I id TV!
   loop ;

: T-BUILD ( ptr a ptr u8 n -- ) {: t:ptr root:ptr rootu:n :}
   root rootu T-VOCAB
   t root rootu T-MERGES
   t T-IDMAP
   1 t T-READY T! ;

\ Total UTF-8 decoder. Invalid sequences are one raw byte so byte-level encoding remains total.
: T-CP@ ( ptr u8 n n -- n n ) {: a:ptr n:n p:n :}
   a p + c@ {: b0:n :}
   b0 128 < if b0 1 exit then
   b0 192 < if b0 1 exit then
   b0 224 < if b0 31 and 2 128
   else b0 240 < if b0 15 and 3 2048
   else b0 248 < if b0 7 and 4 65536
   else b0 1 exit then then then {: acc0:n len:n mincp:n :}
   p len + n > if b0 1 exit then
   acc0
   len 1 ?do
      a p i + + c@ dup 192 and 128 <> if drop drop b0 1 unloop exit then
      63 and swap 6 lshift or
   loop
   dup mincp < if drop b0 1 exit then
   dup UNICODE-CLASS:SCALAR? 0= if drop b0 1 exit then
   len ;

: T-LET-RUN ( ptr u8 n n n -- n ) {: a:ptr n:n p:n off:n :}
   p off + n >= if off exit then
   a n p off + T-CP@ {: cp:n w:n :}
   cp UNICODE-CLASS:LETTER? 0= if off exit then
   a n p off w + recurse ;

: T-NUM-RUN ( ptr u8 n n n -- n ) {: a:ptr n:n p:n off:n :}
   p off + n >= if off exit then
   a n p off + T-CP@ {: cp:n w:n :}
   cp UNICODE-CLASS:NUMBER? 0= if off exit then
   a n p off w + recurse ;

: T-OTHER? ( n -- bool ) {: cp:n :}
   cp UNICODE-CLASS:WHITE-SPACE?
   cp UNICODE-CLASS:LETTER? or
   cp UNICODE-CLASS:NUMBER? or 0= ;

: T-OTHER-RUN ( ptr u8 n n n -- n ) {: a:ptr n:n p:n off:n :}
   p off + n >= if off exit then
   a n p off + T-CP@ {: cp:n w:n :}
   cp T-OTHER? 0= if off exit then
   a n p off w + recurse ;

: T-WS-RUN ( ptr u8 n n n n -- n n ) {: a:ptr n:n p:n off:n last:n :}
   p off + n >= if off last exit then
   a n p off + T-CP@ {: cp:n w:n :}
   cp UNICODE-CLASS:WHITE-SPACE? 0= if off last exit then
   a n p off w + off recurse ;

: T-WS-TAIL ( ptr u8 n n -- n ) {: a:ptr n:n p:n :}
   a n p 0 0 T-WS-RUN {: off:n last:n :}
   p off + n >= if off exit then
   last dup 0= if drop off then ;

: T-CONTRACT ( ptr u8 n n -- n ) {: a:ptr n:n p:n :}
   p 1+ n >= if 0 exit then
   a p 1+ + c@ {: c:n :}
   p 2 + n < if
      c [char] r = a p 2 + + c@ [char] e = and if 3 exit then
      c [char] v = a p 2 + + c@ [char] e = and if 3 exit then
      c [char] l = a p 2 + + c@ [char] l = and if 3 exit then
   then
   c [char] s = if 2 exit then
   c [char] t = if 2 exit then
   c [char] m = if 2 exit then
   c [char] d = if 2 exit then
   0 ;

: T-CHUNK ( ptr u8 n n -- n ) {: a:ptr n:n p:n :}
   a n p T-CP@ drop {: cp:n :}
   cp 39 = if a n p T-CONTRACT dup 0 > if exit then drop then
   cp UNICODE-CLASS:LETTER? if a n p 0 T-LET-RUN exit then
   cp UNICODE-CLASS:NUMBER? if a n p 0 T-NUM-RUN exit then
   cp 32 = if
      p 1+ n < if
         a n p 1+ T-CP@ drop {: next:n :}
         next UNICODE-CLASS:LETTER? if 1 a n p 1+ 0 T-LET-RUN + exit then
         next UNICODE-CLASS:NUMBER? if 1 a n p 1+ 0 T-NUM-RUN + exit then
         next T-OTHER? if 1 a n p 1+ 0 T-OTHER-RUN + exit then
      then
   then
   cp T-OTHER? if a n p 0 T-OTHER-RUN exit then
   a n p T-WS-TAIL ;

: T-ENC-FITS? ( ptr u8 n n -- bool ) {: a:ptr u:n p:n :}
   p u >= if T-TRUE exit then
   a u p T-CHUNK {: len:n :}
   len 0 <= len T-WORK-CAP > or if T-FALSE exit then
   a u p len + recurse ;

: T-WORK-IN ( ptr a ptr u8 n n -- ) {: t:ptr a:ptr p:n u:n :}
   u T-WORK-CAP > if E-TOK-CAP throw then
   u 0 ?do
      a p + i + c@ {: byte:n :}
      byte t T-WORK i TV!
   loop
   u t T-WLEN T! ;

: T-MIN-STEP ( ptr a n n -- n ) {: t:ptr i:n best:n :}
   i t T-WLEN T@ 1- >= if best exit then
   t t T-WORK i TV@ t T-WORK i 1+ TV@ T-RANK {: rank:n :}
   rank 0 >= if
      best 0 < rank best < or if rank else best then
   else best then {: next:n :}
   t i 1+ next recurse ;

: T-MIN ( ptr a -- n )
   0 -1 T-MIN-STEP ;

: T-APPLY-STEP ( ptr a n n n n -- n )
   {: t:ptr rank:n a:n b:n ri:n :}
   ri t T-WLEN T@ >= if t T-WI T@ exit then
   ri t T-WLEN T@ 1- <
   t T-WORK ri TV@ a = and
   t T-WORK ri 1+ TV@ b = and if
      T-BYTE-N rank + t T-WORK t T-WI T@ TV!
      ri 2 +
   else
      t T-WORK ri TV@ t T-WORK t T-WI T@ TV!
      ri 1+
   then {: next:n :}
   t T-WI T@ 1+ t T-WI T!
   t rank a b next recurse ;

: T-APPLY ( ptr a n -- ) {: t:ptr rank:n :}
   0 t T-WI T!
   t rank t T-A rank TV@ t T-B rank TV@ 0 T-APPLY-STEP
   t T-WLEN T! ;

: T-MERGE ( ptr a -- ) {: t:ptr :}
   t T-WLEN T@ 2 < if exit then
   t T-MIN {: rank:n :}
   rank 0 < if exit then
   t rank T-APPLY
   t recurse ;

: T-OUT-WORK ( ptr a n -- ) {: t:ptr i:n :}
   i t T-WLEN T@ = if exit then
   t T-OUTN T@ T-ID-CAP >= if E-TOK-CAP throw then
   t T-WORK i TV@ t T-OUT t T-OUTN T@ TV!
   t T-OUTN T@ 1+ t T-OUTN T!
   t i 1+ recurse ;

: T-ENC-STEP ( ptr a ptr u8 n n -- ) {: t:ptr a:ptr u:n p:n :}
   p u >= if exit then
   a u p T-CHUNK {: len:n :}
   len 0 <= if E-TOK-VOCAB throw then
   t a p len T-WORK-IN
   t T-MERGE
   t 0 T-OUT-WORK
   t a u p len + recurse ;

: T-ENCODE ( ptr a ptr u8 n -- n ) {: t:ptr a:ptr u:n :}
   t T-READY?
   u 0 < u T-ID-CAP > or if E-TOK-CAP throw then
   0 t T-OUTN T!
   t a u 0 T-ENC-STEP
   t T-OUTN T@ {: n:n :}
   n 0 ?do
      t T-GID t T-OUT i TV@ TV@ t T-IDS i TV!
   loop
   n ;

: T-ID@ ( ptr a n -- n ) {: t:ptr i:n :}
   i 0 < i T-ID-CAP >= or if E-TOK-RANGE throw then
   t T-IDS i TV@ ;

: T-ID! ( ptr a n n -- ) {: t:ptr id:n i:n :}
   i 0 < i T-ID-CAP >= or if E-TOK-RANGE throw then
   id 0 < id T-VOCAB-N >= or if E-TOK-RANGE throw then
   id t T-IDS i TV! ;

: T-EMIT ( ptr a n n -- n ) {: t:ptr id:n ix:n :}
   id T-BYTE-N < if
      ix T-DEC-CAP >= if E-TOK-CAP throw then
      id t T-DEC ix TV!
      ix 1+ exit
   then
   id T-BYTE-N - {: rank:n :}
   rank 0 < rank t T-N T@ >= or if E-TOK-RANGE throw then
   t t T-A rank TV@ ix recurse {: next:n :}
   t t T-B rank TV@ next recurse ;

: T-DECODE-STAGE ( ptr a n n -- n ) {: t:ptr count:n i:n :}
   i count = if t T-DECN T@ exit then
   t i T-ID@ {: real:n :}
   real T-EOT = if E-TOK-RANGE throw then
   t T-R2I real TV@ {: id:n :}
   id 0 < if E-TOK-RANGE throw then
   t id t T-DECN T@ T-EMIT t T-DECN T!
   t count i 1+ recurse ;

: T-DECODE-COPY ( ptr a ptr u8 n n -- ) {: t:ptr out:ptr u:n i:n :}
   i u = if exit then
   t T-DEC i TV@ out i + c!
   t out u i 1+ recurse ;

: T-DECODE-LEN ( ptr a n -- n ) {: t:ptr count:n :}
   t T-READY?
   count 0 < count T-ID-CAP > or if E-TOK-CAP throw then
   0 t T-DECN T!
   t count 0 T-DECODE-STAGE ;

: T-DECODE-OUT ( ptr a ptr u8 n -- )
   0 T-DECODE-COPY ;

: T-DECODE ( ptr a n ptr u8 n -- n ) {: t:ptr count:n out:ptr cap:n :}
   cap 0 < if E-TOK-CAP throw then
   t count T-DECODE-LEN {: u:n :}
   u cap > if E-TOK-CAP throw then
   t out u T-DECODE-OUT
   u ;

;package
