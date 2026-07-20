\ maki/bpe-real-data.f - committed REAL GPT-2 vocab SUBSET + tiktoken parity fixtures
\ for maki/bpe-real.f (dot habu-bpe-real-vocab-c973932a, follow-up to the landed
\ synthetic BPE habu-bpe-tokenizer-gpt-37d7f243 / 815f6437).
\ GENERATED DATA - regenerate with scratchpad gen.py; do not hand-edit.
\
\ PROVENANCE. The real GPT-2 byte-level BPE artifacts (identical across every GPT-2
\ model size; these are the canonical 117M copies):
\   encoder.json  https://openaipublic.blob.core.windows.net/gpt-2/models/117M/encoder.json
\     size 1042301 bytes  sha256 196139668be63f3b5d6574427317ae82f612a97c5d1cdaf36ed2256dbf636783
\   vocab.bpe     https://openaipublic.blob.core.windows.net/gpt-2/models/117M/vocab.bpe
\     size 456318 bytes  sha256 1ce1664773c50f3e0cc8842619a93edc4624525b728b188a9e0be33b7726adc5
\ maki/examples/nanogpt/fetch-gpt2-vocab.sh re-fetches and re-verifies both hashes. The REFERENCE
\ token ids below were produced by tiktoken 0.13.0 (real encoder, `get_encoding("gpt2")`,
\ installed user-locally into ~/Work/ml/.venv) and independently reproduced byte-for-byte
\ by a clean-room encoder over the fetched artifacts (bytes_to_unicode + the vocab.bpe
\ merge ranks + the GPT-2 unicode regex) - 0 mismatches across the parity corpus.
\
\ DATA BUDGET. The FULL vocab is 50257 tokens = 256 byte-tokens + 50000 merges + 1
\ special; encoder.json is ~1.02 MB and vocab.bpe ~446 KB, so the full table is
\ RUNTIME-LOADED external reference data (fetch script + pinned hashes), NEVER committed
\ (repo rule: no large regenerable artifacts). What IS committed here is a small pinned
\ SUBSET that makes the parity suite HERMETIC: the 256 real byte-token ids (a fixed
\ 2 KB table) plus exactly the merges that FIRE when the fixture strings below are
\ encoded (the closed fired-merge set - any full-table merge able to fire on a fixture
\ IS in this set), remapped to compact ranks 0..78 preserving priority order.
\ Subset = 79 merges; committed footprint (256+3*79 cells + fixtures) is a
\ few KB, measured well within the suite DATA budget. Loading the subset and encoding
\ any fixture reproduces the full-table result exactly (proven in maki/bpe-test.f).
\
\ DIVERGENCE BOUNDARY (honest, measured - NO silent approximation). The landed
\ pre-split matcher (bpe.f BPE-CHUNK-LEN) classifies raw bytes with ASCII classes
\ (\p{L}->[A-Za-z], \p{N}->[0-9]); GPT-2 pre-splits UNICODE codepoints. Measured result:
\ token-id parity HOLDS for pure-ASCII text AND for runs of purely non-ASCII codepoints
\ (CJK, cyrillic, greek, emoji/symbol, punctuation, non-ASCII digits) - a byte >=0x80 is
\ "other" in the matcher and GPT-2 groups the same run into one chunk, and BPE is a pure
\ function of the chunk's bytes, so the ids agree (fixtures below, incl. multi-byte).
\ Parity DIVERGES exactly when a multi-byte \p{L}/\p{N} codepoint is adjacent to an
\ ASCII letter/digit that GPT-2 folds into one \p{L}+/\p{N}+ run but the matcher splits
\ at the ASCII/non-ASCII boundary. Demonstrator: "naive" with an i-with-diaeresis (the
\ precomposed U+00EF, 2 UTF-8 bytes) between the ASCII letters -> the matcher chunks
\ [2,2,2] bytes (na|i-diaeresis|ve) giving ids [2616, 26884, 303]; tiktoken folds the
\ whole word into one \p{L}+ run giving [2616, 38776]. Both round-trip (split-independent).
\ Closing this class fully needs a UTF-8 codepoint matcher with real unicode L/N tables
\ (its own dot); here the boundary is recorded and PROVEN by fixtures on both sides.
\ bpe-real-data owns no E-codes (it declares none).

require maki/examples/nanogpt/bpe-real.f

package MAKI
public

79 constant BPR-D-MERGES        \ committed fired-merge subset size

\ byte value -> real GPT-2 token id (bytes_to_unicode single-char token, encoder.json)
create BPR-D-BYTEID
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

\ compact subset merges (rank -> child a, child b as ENGINE-internal ids: byte=b,
\ merge=256+rank); priority order preserved from vocab.bpe. Loaded via BPE-MERGE+.
create BPR-D-MA
   32 , 104 , 111 , 32 , 105 , 111 , 101 , 32 , 101 , 32 , 111 , 256 , 32 ,
   105 , 97 , 105 , 108 , 118 , 114 , 263 , 111 , 105 , 108 , 32 , 259 , 97 ,
   262 , 113 , 266 , 84 , 39 , 259 , 281 , 266 , 270 , 32 , 269 , 32 , 119 ,
   268 , 105 , 287 , 277 , 111 , 116 , 39 , 293 , 263 , 256 , 110 , 302 ,
   293 , 84 , 110 , 296 , 291 , 279 , 303 , 114 , 240 , 130 , 32 , 257 , 208 ,
   230 , 208 , 209 , 67 , 209 , 265 , 312 , 226 , 153 , 226 , 195 , 318 ,
   230 , 208 , 232 ,
create BPR-D-MB
   116 , 101 , 110 , 119 , 115 , 114 , 115 , 98 , 100 , 102 , 117 , 111 ,
   100 , 99 , 115 , 258 , 108 , 101 , 111 , 101 , 116 , 109 , 100 , 257 ,
   101 , 103 , 116 , 117 , 116 , 257 , 116 , 261 , 101 , 115 , 101 , 52 ,
   107 , 283 , 110 , 258 , 120 , 278 , 262 , 120 , 257 , 272 , 282 , 274 ,
   298 , 276 , 271 , 292 , 111 , 97 , 264 , 50 , 272 , 294 , 288 , 159 , 172 ,
   230 , 272 , 181 , 156 , 184 , 130 , 290 , 128 , 299 , 111 , 152 , 130 ,
   316 , 175 , 111 , 151 , 178 , 170 ,
\ rank -> REAL GPT-2 id of the merged token (encoder.json), for the id translation
create BPR-D-MID
   256 , 258 , 261 , 266 , 271 , 273 , 274 , 275 , 276 , 277 , 280 , 284 ,
   288 , 291 , 292 , 295 , 297 , 303 , 305 , 307 , 313 , 320 , 335 , 339 ,
   356 , 363 , 395 , 421 , 448 , 464 , 470 , 476 , 496 , 516 , 589 , 604 ,
   624 , 627 , 675 , 836 , 844 , 995 , 999 , 1140 , 1169 , 1183 , 1235 ,
   1379 , 1661 , 1662 , 1808 , 2068 , 2514 , 2616 , 2966 , 5433 , 5968 ,
   7586 , 8394 , 8582 , 8955 , 10545 , 12758 , 16843 , 17312 , 18849 , 20375 ,
   20448 , 21169 , 21831 , 23748 , 24583 , 25081 , 26391 , 26884 , 31373 ,
   33768 , 38857 , 45739 ,

\ Install the committed real subset into the engine + real-id translation layer.
: BPR-D-LOAD ( -- )
   BPR-D-BYTEID  BPR-D-MA BPR-D-MB BPR-D-MID  BPR-D-MERGES  BPR-INSTALL ;

\ ---- parity fixtures: string -> exact tiktoken-gpt2 id sequence (real ids) --------
: BPR-S1 ( -- ptr u8 n )  s" To be" ;
create BPR-I1
   2514 , 307 ,
2 constant BPR-N1
: BPR-S2 ( -- ptr u8 n )  s" the question" ;
create BPR-I2
   1169 , 1808 ,
2 constant BPR-N2
: BPR-S3 ( -- ptr u8 n )  s" 'tis" ;
create BPR-I3
   470 , 271 ,
2 constant BPR-N3
: BPR-S4 ( -- ptr u8 n )  s" not to be" ;
create BPR-I4
   1662 , 284 , 307 ,
3 constant BPR-N4
: BPR-S5 ( -- ptr u8 n )  s" outrageous" ;
create BPR-I5
   448 , 8394 , 516 ,
3 constant BPR-N5
: BPR-S6 ( -- ptr u8 n )  s" hello world" ;
create BPR-I6
   31373 , 995 ,
2 constant BPR-N6
: BPR-S7 ( -- ptr u8 n )  s"  hello" ;
create BPR-I7
   23748 ,
1 constant BPR-N7
: BPR-S8 ( -- ptr u8 n )  s" The quick brown fox" ;
create BPR-I8
   464 , 2068 , 7586 , 21831 ,
4 constant BPR-N8
: BPR-S9 ( -- ptr u8 n )  s"  don't we'll" ;
create BPR-I9
   836 , 470 , 356 , 1183 ,
4 constant BPR-N9
: BPR-S10 ( -- ptr u8 n )  s" MixedCase 42 times" ;
create BPR-I10
   44 , 2966 , 20448 , 5433 , 1661 ,
5 constant BPR-N10
: BPR-S11 ( -- ptr u8 n )  s" 日本語" ;   \ multi-byte MATCH
create BPR-I11
   33768 , 98 , 17312 , 105 , 45739 , 252 ,
6 constant BPR-N11
: BPR-S12 ( -- ptr u8 n )  s" ☃🙂" ;   \ multi-byte MATCH
create BPR-I12
   24583 , 225 , 8582 , 25081 ,
4 constant BPR-N12
: BPR-S13 ( -- ptr u8 n )  s" €" ;   \ multi-byte MATCH
create BPR-I13
   26391 ,
1 constant BPR-N13
: BPR-S14 ( -- ptr u8 n )  s" ٤٢" ;   \ multi-byte MATCH
create BPR-I14
   149 , 97 , 149 , 95 ,
4 constant BPR-N14
: BPR-S15 ( -- ptr u8 n )  s" привет" ;   \ multi-byte MATCH
create BPR-I15
   140 , 123 , 21169 , 18849 , 38857 , 16843 , 20375 ,
7 constant BPR-N15
: BPR-S16 ( -- ptr u8 n )  s"  日本" ;   \ multi-byte MATCH
create BPR-I16
   10545 , 245 , 98 , 17312 , 105 ,
5 constant BPR-N16

10 constant BPR-ASCII-HI        \ fixtures 1..10 are pure ASCII
16 constant BPR-FIX-HI           \ fixtures 11..16 are multi-byte MATCH

\ ---- divergence fixture: matcher output vs tiktoken (boundary proof) --------------
: BPR-DIV-S ( -- ptr u8 n )  s" naïve" ;   \ multi-byte letter inside an ASCII letter run
create BPR-DIV-HABU
   2616 , 26884 , 303 ,   \ engine (byte-level matcher) ids
3 constant BPR-DIV-HABU-N
create BPR-DIV-TIK
   2616 , 38776 ,    \ tiktoken (unicode presplit) ids - DIFFERENT
2 constant BPR-DIV-TIK-N

\ ---- real multi-byte round-trip sample (decode(encode(x))==x; split-independent) --
: BPR-RT-S ( -- ptr u8 n )  s" café ☕ 日本語 — naïve? 42%" ;

;package
