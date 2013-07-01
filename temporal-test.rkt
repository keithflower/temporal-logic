(require "temporal.rkt")
(require srfi/19)         ;; use nice date functions
                
(define (test0)
  "a precedes b, b precedes c, therefore a precedes c"
  (set-events '(a b c))
  (set-temporal-relationship 'a 'b '(<) )
  (print-all-temporal-relationships)
  (set-temporal-relationship 'b 'c '(<) )
  (print-all-temporal-relationships)
  )

(define (test1)
  "a precedes b, b overlaps c, therefore a precedes c"
  (set-events '(a b c))
  (set-temporal-relationship 'a 'b '(<) )
  (set-temporal-relationship 'b 'c '(o) )
  (get-temporal-relationship 'a 'c)
  )

(define (test2)
  "This is the little example given in Allen's original paper."
  (set-events '(owner-armed-security-system alarm-armed robber-in-store))
  (set-temporal-relationship 'owner-armed-security-system
            'alarm-armed
            '(m o) )
  (print-all-temporal-relationships)
  (set-temporal-relationship 'owner-armed-security-system
            'robber-in-store
            '(< m mi >) )
  (print-all-temporal-relationships)
  (set-temporal-relationship 'alarm-armed
            'robber-in-store
            '(o s d) )
  (print-all-temporal-relationships)
  )

(define (test3)
  "This is the little example given in Allen's original paper."
  (set-events '(R-time-John-in-room S-time-light-switch-touched L-time-light-was-on))
  (set-temporal-relationship 'R-time-John-in-room
            'S-time-light-switch-touched
            '(< m mi >) )
  (print-all-temporal-relationships)
  (set-temporal-relationship 'S-time-light-switch-touched
            'L-time-light-was-on
            '(o m) )
  (print-all-temporal-relationships)
  (set-temporal-relationship 'L-time-light-was-on
            'R-time-John-in-room
            '(o s) )
  (print-all-temporal-relationships)
  )

(define (test5)
  "Here's how we might perform simple clinical reasoning using Allen calculus"
  "Diagnosis of schizoaffective disorder requires concurrent mood symptoms"
  "with symptoms that meet 'criteria A' for schizophrenia. There must have been"
  "however, 2 weeks of symptoms of psychosis in the absense of mood symptoms"
  
  (set-events '(major-depressive-episode meets-criteria-A hallucinations))
  (set-temporal-relationship 'a 'b '(<) )
  (set-temporal-relationship 'b 'c '(o) )
  (get-temporal-relationship 'a 'c)
  )

(define (symptoms)
  
  (set-events '(fever neutrophilia hypokalemia cough))
  (set-temporal-relationship 'fever 'cough '(m) )
  (set-temporal-relationship 'cough 'hypokalemia '(m) )
  (set-temporal-relationship 'neutrophilia 'hypokalemia '(m) )
  (get-temporal-relationship 'neutrophilia 'fever)
)


