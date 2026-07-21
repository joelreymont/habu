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
\ IS in this set), remapped to compact ranks 0..90 preserving priority order.
\ Subset = 91 merges; committed footprint (256+3*91 cells + fixtures) is a
\ few KB, measured well within the suite DATA budget. Loading the subset and encoding
\ any fixture reproduces the full-table result exactly (proven in maki/bpe-test.f).
\
\ DIVERGENCE CLOSED (dot habu-bpe-unicode-pre-e6e7f34f). The matcher (bpe.f
\ BPE-CHUNK-LEN) now decodes each UTF-8 codepoint (BPE-CP@) and classifies \p{L}/\p{N}
\ by real unicode Letter/Number category over the bounded BPE-ULET/BPE-UNUM tables
\ (provenance + recipe + cost + residual boundary documented at bpe.f BPE-ULET). The
\ former demonstrator flips to MATCH: "naive" with a precomposed i-with-diaeresis
\ (U+00EF) now folds to one \p{L}+ run giving tiktoken's [2616, 38776] (was the ASCII
\ matcher's [2616, 26884, 303]) - pinned MATCH in BPR-DIV below, red-first (the unfixed
\ base's byte-split ids fail the MATCH assertion). Coverage is BOUNDED to the unicode
\ blocks the GPT-2 vocab exercises; codepoints outside (fullwidth forms, Thai, astral)
\ stay "other", so the matcher may chunk them unlike tiktoken at an ASCII boundary, but
\ GPT-2 has no learned merge crossing such a boundary, so the emitted ids still coincide
\ (measured: 0 id-level residuals over the BMP - see bpe.f). All fixtures round-trip.
\ bpe-real-data owns no E-codes (it declares none).

require maki/examples/nanogpt/bpe-real.f

package MAKI
public

91 constant BPR-D-MERGES        \ committed fired-merge subset size

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
   32 , 104 , 111 , 32 , 105 , 111 , 101 , 32 , 101 , 32 , 111 , 256 ,
   32 , 105 , 97 , 105 , 108 , 32 , 118 , 114 , 263 , 111 , 105 , 108 ,
   32 , 259 , 97 , 262 , 113 , 226 , 266 , 84 , 39 , 259 , 282 , 266 ,
   32 , 270 , 32 , 269 , 32 , 119 , 268 , 105 , 292 , 289 , 278 , 111 ,
   116 , 39 , 296 , 263 , 256 , 110 , 306 , 97 , 296 , 32 , 84 , 110 ,
   195 , 299 , 294 , 280 , 307 , 114 , 240 , 130 , 32 , 273 , 257 , 208 ,
   230 , 208 , 32 , 209 , 67 , 209 , 265 , 319 , 226 , 153 , 226 , 195 ,
   326 , 230 , 313 , 339 , 208 , 325 , 232 ,
create BPR-D-MB
   116 , 101 , 110 , 119 , 115 , 114 , 115 , 98 , 100 , 102 , 117 , 111 ,
   100 , 99 , 115 , 258 , 108 , 110 , 101 , 111 , 101 , 116 , 109 , 100 ,
   257 , 101 , 103 , 116 , 117 , 128 , 116 , 257 , 116 , 261 , 101 , 115 ,
   285 , 101 , 52 , 107 , 284 , 110 , 258 , 120 , 148 , 279 , 262 , 120 ,
   257 , 272 , 283 , 275 , 302 , 277 , 271 , 102 , 295 , 226 , 111 , 97 ,
   169 , 264 , 50 , 272 , 297 , 290 , 159 , 172 , 230 , 97 , 272 , 181 ,
   156 , 184 , 217 , 130 , 293 , 128 , 303 , 111 , 152 , 130 , 323 , 175 ,
   111 , 151 , 152 , 274 , 178 , 343 , 170 ,
\ rank -> REAL GPT-2 id of the merged token (encoder.json), for the id translation
create BPR-D-MID
   256 , 258 , 261 , 266 , 271 , 273 , 274 , 275 , 276 , 277 , 280 , 284 ,
   288 , 291 , 292 , 295 , 297 , 299 , 303 , 305 , 307 , 313 , 320 , 335 ,
   339 , 356 , 363 , 395 , 421 , 447 , 448 , 464 , 470 , 476 , 496 , 516 ,
   564 , 589 , 604 , 624 , 627 , 675 , 836 , 844 , 851 , 995 , 999 , 1140 ,
   1169 , 1183 , 1235 , 1379 , 1661 , 1662 , 1808 , 1878 , 2068 , 2343 , 2514 , 2616 ,
   2634 , 2966 , 5433 , 5968 , 7586 , 8394 , 8582 , 8955 , 10545 , 12385 , 12758 , 16843 ,
   17312 , 18849 , 18923 , 20375 , 20448 , 21169 , 21831 , 23748 , 24583 , 25081 , 26391 , 26884 ,
   31373 , 33768 , 34719 , 38776 , 38857 , 41492 , 45739 ,

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

\ ---- former divergence fixture, now CLOSED (asserted MATCH in bpe-test.f, red-first).
\ "naïve" (precomposed U+00EF between ASCII letters) folds to one \p{L}+ run -> tiktoken's
\ [2616, 38776]; the unfixed ASCII matcher byte-split it to [2616, 26884, 303]. café adds a
\ second precomposed-Latin id-level closure (U+00E9). Escapes pin the exact NFC bytes.
: BPR-DIV-S  ( -- ptr u8 n )  s\" na\xC3\xAFve" ;
create BPR-DIV-I    2616 , 38776 ,
2 constant BPR-DIV-N
: BPR-CAFE-S ( -- ptr u8 n )  s\" caf\xC3\xA9" ;
create BPR-CAFE-I   66 , 1878 , 2634 ,
3 constant BPR-CAFE-N

\ ---- real multi-byte round-trip sample (decode(encode(x))==x; split-independent) --
: BPR-RT-S ( -- ptr u8 n )  s" café ☕ 日本語 — naïve? 42%" ;

\ ---- full-table red-first fixture (dot habu-bpe-full-50k-a598ba57) ----------------
\ "tokenization" needs merges beyond the committed 91-merge subset. tiktoken 0.13.0 and the
\ full 50000-merge table (maki/bpe-full.f) fold it to BPR-TOK-FULL; the subset table (those
\ merges absent) yields BPR-TOK-SUB, reproduced by the committed subset engine. The hermetic
\ gate proves the subset gives BPR-TOK-SUB and NOT BPR-TOK-FULL (so the full table changes the
\ outcome - the parity fixture fails with the subset-only table); the presence-gated full-load
\ test proves the full table gives BPR-TOK-FULL.
: BPR-TOK-S ( -- ptr u8 n )  s" tokenization" ;
create BPR-TOK-SUB   83 , 78 , 74 , 68 , 77 , 72 , 89 , 64 , 83 , 295 ,
10 constant BPR-TOK-SUB-N
create BPR-TOK-FULL  30001 , 1634 ,
2 constant BPR-TOK-FULL-N

;package
