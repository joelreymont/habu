\ maki/bpe-full-data.f - committed FULL-vocab parity fixtures for maki/bpe-full.f
\ (dot habu-bpe-full-50k-a598ba57).
\
\ PROVENANCE. Each id vector is the exact tiktoken 0.13.0 (`get_encoding("gpt2")`,
\ ~/Work/ml/.venv) tokenization of the string, and is independently reproduced by
\ this engine over the full runtime-loaded vocab.bpe (maki/bpe-full.f BPF-LOAD) -
\ proven by maki/bpe-full-test.f. Every string is pure ASCII whose tiktoken tokens
\ depend on merges BEYOND the committed 79-merge subset (maki/bpe-real-data.f), so
\ these fixtures exercise the full table specifically; the subset table encodes them
\ differently (e.g. "tokenization" -> the 10 subset ids in maki/bpe-real-data.f, not
\ the 2 full ids here). No artifact is committed: the vocab.bpe these load from is the
\ runtime-fetched file (fetch-gpt2-vocab.sh, pinned hashes), so the full-load test
\ presence-gates on it. bpe-full-data owns no E-codes.

require maki/examples/nanogpt/bpe-full.f

package MAKI
public

: BPFD-S1 ( -- ptr u8 n )  s" tokenization" ;
create BPFD-I1   30001 , 1634 ,
2 constant BPFD-N1
: BPFD-S2 ( -- ptr u8 n )  s" throughput matters" ;
create BPFD-I2   9579 , 1996 , 6067 ,
3 constant BPFD-N2
: BPFD-S3 ( -- ptr u8 n )  s" The quick brown fox jumps" ;
create BPFD-I3   464 , 2068 , 7586 , 21831 , 18045 ,
5 constant BPFD-N3
: BPFD-S4 ( -- ptr u8 n )  s" internationalization" ;
create BPFD-I4   45609 , 1634 ,
2 constant BPFD-N4
: BPFD-S5 ( -- ptr u8 n )  s" transformer attention" ;
create BPFD-I5   7645 , 16354 , 3241 ,
3 constant BPFD-N5
: BPFD-S6 ( -- ptr u8 n )  s" embeddings and gradients" ;
create BPFD-I6   20521 , 67 , 654 , 290 , 3915 , 2334 ,
6 constant BPFD-N6
: BPFD-S7 ( -- ptr u8 n )  s" supercalifragilistic" ;
create BPFD-I7   16668 , 9948 , 361 , 22562 , 346 , 2569 ,
6 constant BPFD-N7
: BPFD-S8 ( -- ptr u8 n )  s" GPT tokenizer 2024" ;
create BPFD-I8   38 , 11571 , 11241 , 7509 , 48609 ,
5 constant BPFD-N8
: BPFD-S9 ( -- ptr u8 n )  s" questionable behavior" ;
create BPFD-I9   25652 , 540 , 4069 ,
3 constant BPFD-N9
: BPFD-S10 ( -- ptr u8 n )  s" neural networks" ;
create BPFD-I10  710 , 1523 , 7686 ,
3 constant BPFD-N10

10 constant BPFD-FIX-N

\ real-text round-trip sample (decode(encode(x))==x; byte-level, split-independent)
: BPFD-RT-S ( -- ptr u8 n )
   s" The rain in Spain falls mainly on the plain; 3.14159 pi, 42% off, e=2.71828!" ;

\ throughput corpus (337 bytes, 70 tokens) - encoded repeatedly for a tokens/sec measurement
: BPFD-CORPUS ( -- ptr u8 n )
   s" Byte-level BPE tokenization turns raw text into subword tokens by greedily merging the highest-priority adjacent pair until none remain. The GPT-2 vocabulary has fifty thousand merges plus two hundred fifty-six byte tokens and one special marker. Throughput depends on making each pair-rank lookup constant time instead of a linear scan." ;

;package
