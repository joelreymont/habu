\ gpt2-pin.f - pinned GPT-2 artifact identity.

package GPT2PIN

public

: REVISION$ ( -- ptr u8 n )
   s" 607a30d783dfa663caf39e06633721c8d4cfcd7e" ;

: CONFIG-NAME$ ( -- ptr u8 n )
   s" config.json" ;
665 constant CONFIG-LEN
: CONFIG-SHA256$ ( -- ptr u8 n )
   s" 0daed7749b4f02b8f76240d5444551d7b08712dab4d0adb8239c56ba823bb7b4" ;

: MODEL-NAME$ ( -- ptr u8 n )
   s" model.safetensors" ;
548105171 constant MODEL-LEN
: MODEL-SHA256$ ( -- ptr u8 n )
   s" 248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707" ;

: VOCAB-NAME$ ( -- ptr u8 n )
   s" vocab.json" ;
1042301 constant VOCAB-LEN
: VOCAB-SHA256$ ( -- ptr u8 n )
   s" 196139668be63f3b5d6574427317ae82f612a97c5d1cdaf36ed2256dbf636783" ;

: MERGES-NAME$ ( -- ptr u8 n )
   s" merges.txt" ;
456318 constant MERGES-LEN
: MERGES-SHA256$ ( -- ptr u8 n )
   s" 1ce1664773c50f3e0cc8842619a93edc4624525b728b188a9e0be33b7726adc5" ;

;package
