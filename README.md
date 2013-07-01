Allen interval calculus
=======================

Allen interval calculus is a way to reason about temporal events. For example, given the following facts:

      A patient's fever immediately preceded her cough;
           her cough immediately preceded lab values showing hypokalemia;
           neutrophilia also immediately preceded the hypokalemia.

One can make queries of the relationships between intervals, for example:

    When did her neutrophila occur in relationship with her fever?

define (symptoms)
  
  (set-events '(fever neutrophilia hypokalemia cough))
  (set-temporal-relationship 'fever 'cough '(m) )
  (set-temporal-relationship 'cough 'hypokalemia '(m) )
  (set-temporal-relationship 'neutrophilia 'hypokalemia '(m) )
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

The results '(> mi di si oi) indicate that the episode of neutrophilia either came after the fever (but not immediately), came immediately after the fever, started concurrently with the fever, or was overlapped with the duration of the fever.
