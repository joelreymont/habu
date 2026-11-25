\ text-made.f - a word that text a definer evaluates makes.
: RQC-TEXT-MAKE ( -- )
   parse-name drop  s" : RQC-TEXT-MADE ( -- n ) 5 ;" evaluate-closed ;
generates: RQC-TEXT-MAKE ( -- n )
RQC-TEXT-MAKE RQC-TEXT-MADE
