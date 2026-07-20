\ maki/bpe-data.f - committed BPE fixtures for maki/bpe.f (dot habu-bpe-tokenizer-gpt-37d7f243).
\ GENERATED DATA - regenerate rather than hand-edit.
\
\ Reality forced the SYNTHETIC-fixture path: this box has no network, tiktoken is
\ not installed (~/Work/ml/.venv and ~/Work/odin-spark-envs both lack it), and no
\ GPT-2 encoder.json/vocab.bpe/merges.txt exists anywhere under ~ (searched). So the
\ real tiktoken parity fixture is a follow-up dot; here deliverable (1) runs against a
\ small, exactly-verifiable committed vocab+merges fixture, and that same fixture is
\ the locked table deliverable (2) reproduces from the committed corpus.
\
\ Provenance: a clean-room GPT-2 byte-level BPE reference in Python (tiktoken absent)
\ generated every number below. It pre-splits with the TRUE GPT-2 regex under ASCII
\ classes (exact for ASCII, since \p{L}==[A-Za-z], \p{N}==[0-9], \s==[ \t\n\v\f\r]),
\ then applies GPT-2's bpe()/training with the same tie-break as maki/bpe.f. The
\ agreement of that independent regex-based reference with the hand-rolled Forth
\ matcher IS the pre-split parity evidence. Generator (also in scratchpad bpe_gen.py):
\
\   PAT = re.compile(r"'s|'t|'re|'ve|'m|'ll|'d| ?[A-Za-z]+| ?[0-9]+"
\                    r"| ?[^\sA-Za-z0-9]+|\s+(?!\S)|\s+")
\   def presplit(t): return [m.group(0) for m in PAT.finditer(t)]
\   def train(t,k):   # count within-chunk pairs; pick max count, tie -> smallest a,b;
\       ...           # replace all occurrences with id 256+rank; stop at k or count<2
\   def encode(t):    # per chunk: while a mergeable pair exists, merge the min-rank
\       ...           # bigram's every occurrence; ids are 256+rank
\   merges = train(CORPUS, 64)                    # -> 14 merges
\   for s in PARITY: print(encode(s))             # -> the BPD-I* ids below
\
\ CORPUS: three public-domain Hamlet lines (bounded slice - the full tiny-shakespeare
\ corpus is ~1.1 MB and kept OUTSIDE the repo, so a bounded committed corpus with the
\ lock derived from it is the honest path; 130 bytes, 33 pre-split chunks, well within
\ the suite budget). Training on it yields 14 merges (it runs out of count>=2 pairs).
\
\ Storage: 14 merges + 5 parity id-vectors = a few hundred bytes, so COMMITTED here
\ (measured within the DATA budget). The full 50257 GPT-2 vocab is ~MB-scale and would
\ be a runtime-loaded checked artifact - deferred with the tiktoken parity fixture.
\ bpe-data owns no E-codes (it declares none).

require maki/examples/nanogpt/bpe.f

package MAKI
public

: BPD-CORPUS ( -- ptr u8 n )
   s\" To be, or not to be, that is the question:\nWhether 'tis nobler in the mind to suffer\nThe slings and arrows of outrageous fortune,\n" ;

14 constant BPD-MERGES        \ locked merge count trained from BPD-CORPUS

\ locked merge table (rank -> child a, child b); merged token id = 256+rank
create BPD-A   32 , 104 ,  32 , 105 ,  32 ,  32 ,  32 ,  32 , 101 , 105 , 256 , 256 , 261 , 262 ,
create BPD-B  116 , 101 , 111 , 110 ,  97 ,  98 , 110 , 115 , 114 , 115 , 111 , 257 , 101 , 111 ,

\ Load the committed locked table into the BPE engine (the deliverable-1 fixture path).
: BPD-LOAD ( -- )
   BPE-BEGIN
   BPD-MERGES 0 ?do  BPD-A i cells + @  BPD-B i cells + @  BPE-MERGE+  loop
   BPE-SEAL ;

\ ---- parity strings + pinned exact token-id sequences (generator PARITY) ----------
: BPD-S1 ( -- ptr u8 n )  s" To be" ;
create BPD-I1  84 , 111 , 268 ,
3 constant BPD-N1

: BPD-S2 ( -- ptr u8 n )  s" the question" ;
create BPD-I2  116 , 257 , 32 , 113 , 117 , 101 , 115 , 116 , 105 , 111 , 110 ,
11 constant BPD-N2

: BPD-S3 ( -- ptr u8 n )  s" 'tis" ;
create BPD-I3  39 , 116 , 265 ,
3 constant BPD-N3

: BPD-S4 ( -- ptr u8 n )  s" not to be" ;
create BPD-I4  110 , 111 , 116 , 266 , 268 ,
5 constant BPD-N4

: BPD-S5 ( -- ptr u8 n )  s" outrageous" ;
create BPD-I5  111 , 117 , 116 , 114 , 97 , 103 , 101 , 111 , 117 , 115 ,
10 constant BPD-N5

;package
