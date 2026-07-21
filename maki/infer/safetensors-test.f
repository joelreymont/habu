\ safetensors-test.f - hermetic red-first coverage for the safetensors loader
\ (maki/infer/safetensors.f, epic habu-epic-gb10-uma-391d12e8). Run standalone:
\ bin/hb --load maki/infer/safetensors-test.f
\
\ Fixtures are BUILT in Forth: the valid leg writes a tiny synthetic safetensors
\ file (2 tensors + a skipped __metadata__) to disk and parses it back through the
\ real mmap path; every malformed leg builds an in-memory image and asserts the
\ loader rejects it with its NAMED E-ST-* code (no partial registration). JSON is
\ written with backtick placeholders for the double quotes (a `s"` literal cannot
\ contain `"`); BUILD-J rewrites ` -> " while assembling the header. The
\ presence-gated real leg parses HuggingFace GPT-2 (openai-community/gpt2) and
\ asserts the tensor census, mirroring the vocab full-load leg
\ (maki/examples/nanogpt/bpe-full-test.f).

require lib/test.f
require maki/infer/safetensors.f

package SAFET-TEST

34 constant STT-DQ                              \ "
96 constant STT-BT                              \ ` placeholder for " in s" literals

create IMG 1024 allot                           \ assembled file image
variable IMG-LEN

: IMG-C ( n -- )                                \ append one byte
   IMG IMG-LEN @ + c!  IMG-LEN @ 1+ IMG-LEN ! ;

: IMG-JSON ( ptr u8 n -- )                      \ append header text, ` -> "
   {: a:ptr u :}
   u 0 ?do
      a i + c@ dup STT-BT = if drop STT-DQ then IMG-C
   loop ;

: IMG$ ( -- ptr u8 n )  IMG IMG-LEN @ ;

\ Build a full image: 8-byte LE header length, then the (backtick-rewritten)
\ header JSON, then `dcount` data bytes whose value is their index.
: BUILD-J ( ptr u8 n n -- )
   {: ja:ptr ju dcount :}
   0 IMG-LEN !
   8 IMG-LEN !                                  \ reserve the length slot
   ja ju IMG-JSON
   IMG-LEN @ 8 -  {: hlen :}                     \ header byte length
   8 0 ?do  hlen i 8 * rshift $FF and  IMG i +  c!  loop
   dcount 0 ?do i $FF and IMG-C loop ;

\ ---- the valid synthetic file (F32 [2,2] + BF16 [4], __metadata__ skipped) ---
: BUILD-VALID ( -- )
   s" {`__metadata__`:{`format`:`pt`},`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]},`b`:{`dtype`:`BF16`,`shape`:[4],`data_offsets`:[16,24]}}"
   24 BUILD-J ;

: SYNTH-PATH ( -- ptr u8 n )  s" /tmp/hb-st-synth.safetensors" ;

: PARSE-IMG ( -- )  IMG$ SAFET:PARSE-SPAN ;

\ ---- malformed images: each rejected with its named code -------------------
: R-TRUNC ( -- )                                \ file shorter than the 8-byte length
   0 IMG-LEN !  4 IMG-LEN !  PARSE-IMG ;
: R-HDRBIG ( -- )                               \ header length runs past the file
   0 IMG-LEN !  8 IMG-LEN !
   8 0 ?do 1000 i 8 * rshift $FF and IMG i + c! loop  PARSE-IMG ;
: R-BADJSON ( -- )    s" { bad" 0 BUILD-J PARSE-IMG ;
: R-VALNOTOBJ ( -- )  s" {`a`:5}" 0 BUILD-J PARSE-IMG ;
: R-OOR ( -- )        s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,64]}}" 16 BUILD-J PARSE-IMG ;
: R-OVERLAP ( -- )    s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]},`b`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[8,24]}}" 24 BUILD-J PARSE-IMG ;
: R-SIZE ( -- )       s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,8]}}" 8 BUILD-J PARSE-IMG ;
: R-MISALIGN ( -- )   s" {`a`:{`dtype`:`BF16`,`shape`:[1],`data_offsets`:[0,2]},`b`:{`dtype`:`F32`,`shape`:[1],`data_offsets`:[2,6]}}" 6 BUILD-J PARSE-IMG ;
: R-BADDTYPE ( -- )   s" {`a`:{`dtype`:`F99`,`shape`:[1],`data_offsets`:[0,4]}}" 4 BUILD-J PARSE-IMG ;
: R-MISSING ( -- )    s" {`a`:{`dtype`:`F32`,`data_offsets`:[0,4]}}" 4 BUILD-J PARSE-IMG ;
: R-NEGDIM ( -- )     s" {`a`:{`dtype`:`F32`,`shape`:[-1],`data_offsets`:[0,4]}}" 4 BUILD-J PARSE-IMG ;

: CLEANUP ( -- )  SYNTH-PATH FS-PATHZ unlink drop ;

\ ---- valid round trip: write a real file, mmap + parse it back --------------
: CENSUS ( -- )
   BUILD-VALID  SYNTH-PATH IMG$ WRITE-ALL
   SYNTH-PATH SAFET:LOAD
   SAFET:COUNT 2 T=                             \ __metadata__ not counted
   s" a" SAFET:FIND {: ia :}
   ia 0 < 0= TTRUE
   ia SAFET:RANK@ 2 T=
   ia 0 SAFET:DIM@ 2 T=   ia 1 SAFET:DIM@ 2 T=
   ia SAFET:DTYPE@ SAFET:ST-DT-F32 T=
   ia SAFET:NBYTES@ 16 T=
   ia SAFET:DATA-PTR c@ 0 T=                    \ a's data starts at data byte 0
   s" b" SAFET:FIND {: ib :}
   ib 0 < 0= TTRUE
   ib SAFET:DTYPE@ SAFET:ST-DT-BF16 T=
   ib SAFET:RANK@ 1 T=
   ib 0 SAFET:DIM@ 4 T=
   ib SAFET:NBYTES@ 8 T=
   ib SAFET:DATA-PTR c@ 16 T=                   \ b's data starts at data byte 16
   s" absent" SAFET:FIND -1 T=                  \ unknown name -> -1
   SAFET:UNMAP
   CLEANUP ;

\ ---- presence-gated real artifact (HF gpt2 model.safetensors) ---------------
: REAL-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

: REAL-RUN ( -- )
   REAL-PATH SAFET:PRESENT? 0= if
      s" safetensors: gpt2-model/model.safetensors absent -> real-artifact leg SKIPPED (run maki/examples/nanogpt/fetch-gpt2-model.sh)" type cr
      0 0= TTRUE exit
   then
   REAL-PATH SAFET:LOAD
   s" real gpt2 tensors=" type SAFET:COUNT . cr
   SAFET:COUNT 160 T=
   s" wte.weight" SAFET:FIND {: w :}
   w 0 < 0= TTRUE
   w SAFET:RANK@ 2 T=
   w 0 SAFET:DIM@ 50257 T=
   w 1 SAFET:DIM@ 768 T=
   w SAFET:DTYPE@ SAFET:ST-DT-F32 T=
   w SAFET:NBYTES@ 154389504 T=                 \ 50257*768*4
   s" wpe.weight" SAFET:FIND {: p :}
   p 0 SAFET:DIM@ 1024 T=   p 1 SAFET:DIM@ 768 T=
   s" h.0.attn.c_attn.weight" SAFET:FIND {: c :}
   c 0 SAFET:DIM@ 768 T=                         \ Conv1D: [in, out], NOT [out, in]
   c 1 SAFET:DIM@ 2304 T=
   SAFET:UNMAP ;

: RUN ( -- )
   T-RESET
   [: R-TRUNC ;]     E-ST-HEADER  TTHROWSQ
   [: R-HDRBIG ;]    E-ST-HEADER  TTHROWSQ
   [: R-BADJSON ;]   E-ST-JSON    TTHROWSQ
   [: R-VALNOTOBJ ;] E-ST-JSON    TTHROWSQ
   [: R-OOR ;]       E-ST-OFFSETS TTHROWSQ
   [: R-OVERLAP ;]   E-ST-OFFSETS TTHROWSQ
   [: R-SIZE ;]      E-ST-SHAPE   TTHROWSQ
   [: R-MISALIGN ;]  E-ST-ALIGN   TTHROWSQ
   [: R-BADDTYPE ;]  E-ST-DTYPE   TTHROWSQ
   [: R-MISSING ;]   E-ST-FIELD   TTHROWSQ
   [: R-NEGDIM ;]    E-ST-SHAPE   TTHROWSQ
   SAFET:COUNT 0 T=                             \ no partial registration after a reject
   CENSUS
   REAL-RUN
   T-REPORT ;

RUN

;package
