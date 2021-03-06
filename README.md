Allen interval calculus
=======================

Allen interval calculus is a way to reason about temporal events. 

Here's a simple example: given the following facts:

      A patient's fever immediately preceded her cough;
           her cough immediately preceded lab values showing hypokalemia;
           neutrophilia also immediately preceded the hypokalemia.

One can make queries of the relationships between intervals, for example:

    When did her neutrophila occur in relationship with her fever?
``` Racket
define (symptoms)
  
  (set-events '(fever neutrophilia hypokalemia cough))
  (set-temporal-relationship 'fever 'cough '(m))
  (set-temporal-relationship 'cough 'hypokalemia '(m))
  (set-temporal-relationship 'neutrophilia 'hypokalemia '(m))
  (get-temporal-relationship 'neutrophilia 'fever)
)

Welcome to Racket v5.3.4.
racket@> ,enter "/Users/keith/medsim/temporal-logic/temporal.rkt"
racket@temporal.rkt> (symptoms)
interval fever
interval neutrophilia
interval hypokalemia
interval cough
'(> mi di si oi)
```
The result list ```'(> mi di si oi)``` indicates that several relationships are possible - the episode of neutrophilia either came after the fever (not necessarily immediately after), did come immediately after the fever, started concurrently with the fever, and/or overlapped with the duration of the fever.

![Symptom example](example.png)

This implementation in Racket is based on Stephen Tanimoto's Common Lisp code as described in Chapter 8 ("Commonsense Reasoning") of his book *Elements of Artificial Intelligence*.

 There are 13 possible relationships between intervals, encoded as follows:

     "before"          <
     "after"           >
     "during"          d
     "contains"        di
     "overlaps"        o
     "overlapped-by"   oi
     "meets"           m
     "met-by"          mi
     "starts"          s
     "started-by"      si
     "finishes"        f
     "finished-by"     fi
     "equals"          =

