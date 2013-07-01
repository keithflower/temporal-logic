;;; temporal.rkt

;;; Temporal (Allen interval calculus) logic using constraint propagation,
;;; in Racket 
;;; by Keith Flower, Copyright 2013.
;;;
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This is based on Common Lisp code originally written by Steven L.
;;; Tanimoto (Copyright 1995).
;;; This program is described in Chapter 8 ("Commonsense Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.

;;; The 13 possible relationships between two intervals are encoded as follows:
;;; "before" <
;;; "after" >
;;; "during" d
;;; "contains" di
;;; "overlaps" o
;;; "overlapped-by" oi
;;; "meets" m
;;; "met-by" mi
;;; "starts" s
;;; "started-by" si
;;; "finishes" f
;;; "finished-by" fi
;;; "equals" =

(module temporal racket
  
(provide set-events)
(provide set-temporal-relationship)
(provide get-temporal-relationship)
(provide print-all-temporal-relationships)

(require scheme/dict) 
(require scheme/set)

;;; ------- Data structures ------

;;; The following global variable will contain a list of the
;;; intervals of time involved in the problem.
;;; (define *intervals* '())

(define table-of-current-relns (make-hash))
(define transitivity-table (make-hash))
(define queue null)
(define *intervals* '())

(define (set-events events)
  (set! *intervals* events)
  (init-combining-table)
  (init-current-relationships)
  (init-queue))

(define (get-temporal-relationship i j)
  (get-temporal-reln i j))

(define-syntax-rule (pop l)
  (let ((result (car l)))
    (set! l (cdr l))
    result))
  
;;; We represent several structures using hash tables.
;;; First, we represent the network of temporal relationships using
;;; a one table.  Then we store the transitivity table as a hash table.
;;; We also establish a queue here for use by the main inference
;;; procedure, via the functions INIT-QUEUE, etc.

(define (get-temporal-reln i j)
    "Returns the list of possible temporal relationships
     that might hold between intervals I and J."
    (hash-ref table-of-current-relns (list i j)) )
(define (set-temporal-reln i j relationships)
    "Stores the list of possible temporal relationships
     that might hold between intervals I and J."
    (hash-set! table-of-current-relns (list i j) relationships))
(define (combine-relation r1 r2)
    "Looks up the transitivity entry in the table."
    (hash-ref transitivity-table (list r1 r2)) )
(define (store-combine-relation r1 r2 relationships)
    "Stores the transitivity entry in the table."
    (hash-set! transitivity-table (list r1 r2) relationships) )
(define (init-queue)
    "Initializes the QUEUE."
    (set! queue null) )
(define (insert-into-queue elt)
    "Puts ELT at end of QUEUE."
    (set! queue (append queue (list elt))) )
  (define (get-from-queue)
    (pop queue) )
  (define (queue-empty-p)
    (null? queue) )
  
;;; The following constant represents a list of the 13
;;; primitive temporal relations.
(define primitive-relations
  '(< > d di o oi m mi s si f fi =) )

;;; Next is a variant of this list that omits the EQUALS relation;
;;; it is used by the function that initializes the transitivity table.
(define primitive-relations12
  '(< > d di o oi m mi s si f fi) )

;;; ------- Initialization ------

;;; The following data is used to set up the "Transitivity Table" 
;;; used to propagate constraints using Allen's method.
;;; By putting this information into a hash table, the process of
;;; constraint propagation is made to run faster than it would
;;; if the list structures had to be continually traversed.
;;; One could alternatively implement the table as an array,
;;; in which case each interval would need to be assigned an
;;; integer index.
(define entries '( 
("before" ( < ) (< > d di o oi m mi s si f fi =)
  ( < o m d s ) ( < ) ( < )
  ( < o m d s ) ( < ) ( < o m d s )
  ( < ) ( < ) ( < o m d s ) ( < ))
("after"  (< > d di o oi m mi s si f fi =) ( > )
  ( > oi mi d f ) ( > ) ( > oi mi d f )
  (>) ( > oi mi d f ) ( > )
  ( > oi mi d f ) ( > ) ( > ) ( > ))
("during" ( < ) ( > )
  ( d ) (< > d di o oi m mi s si f fi =) ( < o m d s )
  ( > oi mi d f ) ( < ) ( > )
  ( d ) ( > oi mi d f ) ( d ) ( < o m d s ))
("contains" ( < o m di fi ) ( > oi di mi si )
  ( o oi d s f di si fi = ) ( di ) ( o di fi )
  ( oi di si ) ( o di fi ) ( o di si )
  ( di fi o ) ( di ) ( di si oi ) ( di ))
("overlaps" ( < ) ( > oi di mi si )
  ( o d s ) ( < o m di fi ) ( < o m )
  ( o oi d s f di si fi = ) ( < ) ( oi di si )
  ( o ) ( di fi o ) ( d s o ) ( < o m ))
("overlapped-by" ( < o m di fi ) ( > )
  ( oi d f ) ( > oi mi di si ) ( o oi d s f di si fi = )
  ( > oi mi ) ( o di fi ) ( > )
  ( oi d f ) ( oi > mi ) ( oi ) ( oi di si))
("meets" ( < ) ( > oi mi di si )
  ( o d s ) ( < ) ( < )
  ( o d s ) ( < ) ( f fi = )
  ( m ) ( m ) ( d s o ) ( < ))
("met-by" ( < o m di fi ) ( > )
  ( oi d f ) ( > ) ( oi d f )
  ( > ) ( s si = ) ( > )
  ( d f oi ) ( > ) ( mi ) ( mi ))
("starts" ( < ) ( > )
  ( d ) ( < o m di fi ) ( < o m )
  ( oi d f ) ( < ) ( mi )
  ( s ) ( s si = ) ( d ) ( < m o ))
("started-by" ( < o m di fi ) ( > )
  ( oi d f ) ( di ) ( o di fi )
  ( oi ) ( o di fi ) ( mi )
  ( s si = ) ( si ) ( oi ) ( di ))
("finishes" ( < ) ( > )
  ( d ) ( > oi mi di si ) ( o d s )
  ( > oi mi ) ( m ) ( > )
  ( d ) ( > oi mi ) ( f ) ( f fi = ))
("finished-by" ( < ) ( > oi mi di si )
  ( o d s ) ( di ) ( o )
  ( oi di si ) ( m ) ( si oi di )
  ( o ) ( di ) ( f fi = ) ( fi ))
))

(define r '())
(define (init-combining-table)
    "Stores James Allen's transitivity table entries in
     a hash table."
    (let ((pr-left primitive-relations12)
          (ps-left r))
      (for ((row entries))
        (pop row)
        (set! r (pop pr-left))
        (set! ps-left primitive-relations12)
        (for ((entry row))
          (store-combine-relation r (pop ps-left) entry)
          )
        (store-combine-relation r '= (list r))
        )
     (for ((c primitive-relations))
        (store-combine-relation '= c (list c)) )
 ) )

;;; The next function simply prints out the entries of
;;; the combining table, and it is used in debugging.      
(define (print-combining-table)
  "Prints the table (for debugging)."
  (printf "~%The Transitivity Table:")
    (for ((row primitive-relations))
      (printf "~%")
      (for ((col primitive-relations))
        (printf "~a\n" (combine-relation row col))
        (printf " ")
      ) ) )

;;; The function below gives initial values to all of the
;;; hashtable entries for temporal relationships on the intervals.
;;; For each pair of intervals I and J, the entry is either
;;; (=) if I = J, or it is the value of PRIMITIVE-RELATIONS,
;;; which means "nothing is known yet" or "anything is possible."
(define (init-current-relationships)
  "Sets up all the relationships as unconstrained."
  (for ((i *intervals*))
    (printf "interval ~A\n" i)
    (for ((j *intervals*))
      (if (eq? i j)
          (set-temporal-reln i j '(=))
          (set-temporal-reln i j primitive-relations) )
    ) ) )
;;; ------- Updating functions ------

(define (constraints relationships1 relationships2)
  "Returns the set of constraints on A and C implied by
   this pair of relationships, where A can be related to B
   by RELATIONSHIPS1 and B can be related to C by RELATIONSHIPS2."
  (let ((c null))
    (for ((r1 relationships1))
      (for ((r2 relationships2))
        (set! c (union c (combine-relation r1 r2))) ) )
    c) )

(define (add i j relationships)
  "Takes the new list of possible RELATIONSHIPS between
   interval I and interval J and propagates this constraint
   to the other stored relationships."
  (let* ((old (get-temporal-reln i j))
         (new (intersection relationships old)) )
    (when (not (set-equalx new old))
      (set-temporal-reln i j new)
      (insert-into-queue (list i j)) ) ) 
  (let loop ()
    (when (not (queue-empty-p))
       (let* ((pair (get-from-queue))
              (i (first pair))
              (j (second pair)) )
      (for ((k *intervals*))
        (try-to-update i j k)
        (try-to-update k i j)
       )
    (loop)) ) ) )

(define  (try-to-update x y z)
  "Handles updating tests and action for ADD."
  (let ((temp
        (intersection (get-temporal-reln x z)
                      (constraints (get-temporal-reln x y)
                                   (get-temporal-reln y z) ) )))
    (when (null? temp) (signal-inconsistent x y z))
    (when (not (set-equalx temp (get-temporal-reln x z)))
        (insert-into-queue (list x z))
        (set-temporal-reln x z temp) ) ) )
 
(define (set-temporal-relationship i j relationships)
  "Registers the constraints on I and J given by
   RELATIONSHIPS, including the implied inverse
   relationships."
  (add i j relationships)
  (add j i (invert-relationships relationships)) )

(define (set-equalx set1 set2)
  "Returns T if the sets contain the same elements."
  (equal? (list->set set1) (list->set set2)))
(define (invert-primitive p)
  "Returns the inverse relation for P."
  (second (assoc p
    '((< >)(> <)(d di)(di d)(o oi)(oi o)(m mi)(mi m)
      (s si)(si s)(f fi)(fi f)(= =) ) )) )

(define (invert-relationships r)
  "Returns the set of inverse relationships of elements of R."
  (map invert-primitive r) )

(define (signal-inconsistent x y z)
  "Prints a message about a temporal inconsistency."
  (printf "~%NOTE: A CONTRADICTION HAS BEEN FOUND ")
  (printf "~%among intervals ~S, ~S, and ~S.~%"
          x y z) )

(define (print-all-temporal-relationships)
  "Prints out for every pair of intervals what is currently
   explicity represented about their temporal relationship."
  (printf "~%The current interval relationships are:")
  (for ((i *intervals*))
    (for ((j *intervals*))
    (printf "~%")
      (printf "Interval ~S and interval ~S: ~S"
         i j (get-temporal-reln i j)) ) ) )

(define (intersection a b)
  (set->list (set-intersect (list->set a) (list->set b))))

(define (union a b)
  (set->list (set-union (list->set a) (list->set b))))

)
