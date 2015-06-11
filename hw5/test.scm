#lang racket

(define match-junk
  (lambda (k frag)
    (or (call/cc (lambda (cc)
      (cons (lambda () (cc #f)) frag)))
    (and (< 0 k)
      (pair? frag)
      (match-junk (- k 1) (cdr frag))))))

(define match-*
  (lambda (matcher frag)
    (or (call/cc (lambda (cc) 
      (cons (lambda () (cc #f)) frag))) ; empty match
    (let ((head (matcher frag)))
      (if head
        (and (not (eq? (car head) frag)) ; check not empty match
          (or (match-* matcher (cdr head))
            ((car head)))) ; backtrack
      #f )))))

(define make-matcher
  (lambda (pat)
    (cond
      ; pat is a symbol
      ((symbol? pat)
        (lambda (frag)
          (and (pair? frag)
            (eq? pat (car frag))
            (call/cc (lambda (cc) (cons (lambda () (cc #f)) (cdr frag)))))))
      ; (or pat..)
      ((eq? 'or (car pat))
        (let make-or-matcher ((pats (cdr pat)))
          (if (null? pats)
            (lambda (frag) (call/cc (lambda (cc) (cons (cc #f) frag)))) ; return false      
            (let ((head-matcher (make-matcher (car pats)))
              (tail-matcher (make-or-matcher (cdr pats))))
              (lambda (frag) ; match 1st pat or try the rest
                (let ((head (head-matcher frag)))
                  (if head
                    (or (call/cc (lambda (cc)
                      (cons (lambda () (cc #f)) (cdr head)))) ; return
                      (tail-matcher frag)) ; continue and try tail
                    (tail-matcher frag)))))))) ; try tail
      ; (list pat...)
      ((eq? 'list (car pat))
        (let make-list-matcher ((pats (cdr pat)))
          (if (null? pats)
            (lambda (frag) 
              (call/cc (lambda (cc)
                (cons (lambda () (cc #f)) frag))))  ; return false 
            (let ((head-matcher (make-matcher (car pats)))
              (tail-matcher (make-list-matcher (cdr pats))))
            (lambda (frag)
                (let ((head (head-matcher frag)))
                  (if head
                    (let ((tail (tail-matcher (cdr head)))) ; match tail
                      (if tail
                        tail ; return tail's match
                        ((car head)))) ; backtrack into head's cc
                    #f )))))))
      ; (junk k)
      ((eq? 'junk (car pat))
        (let ((k (cadr pat)))
          (lambda (frag)
            (match-junk k frag))))
      ; (* pat)
      ((eq? '* (car pat))
        (let ((matcher (make-matcher (cadr pat))))
          (lambda (frag)
            (match-* matcher frag)))))))



;
; Test cases: 
(define frag0 '())
(define frag1 '(a t g c t a))
;
; Scheme does not care about the newlines in the definition of frag2.
; From Scheme's point of view, they are merely extra white space that
; is ignored.  The newlines are present only to help humans understand
; how the patterns defined below are matched against frag2.
(define frag2 '(c c c g a t a a a a a a g t g t c g t
                a
                a g t a t a t g g a t a
                t a
                a g t a t a t g g a t a
                c g a t c c c t c g a t c t a))

; Most of the uses of "list" in the following pattern definitions
; are as a symbol that is part of a pattern, not as a procedure.
; However, there are two exceptions, one when defining pat3
; and the other when defining pat4.
(define pat1 '(list a t g c))
(define pat2 '(or
               (list a g t a t a t g g a t a)
               (list g t a g g c c g t)
               (list c c c g a t a a a a a a g t g t c g t)
               (list c g a t c c c (junk 1) c g a t c t a)))
(define pat3 (list 'list pat2 '(junk 2)))
(define pat4 (list '* pat3))

; For each pattern defined above, use "make-matcher" to create a
; matcher that matches the pattern.
(define matcher1 (make-matcher pat1))
(define matcher2 (make-matcher pat2))
(define matcher3 (make-matcher pat3))
(define matcher4 (make-matcher pat4))

; Return the first solution acceptable to ACCEPT.
(define (acceptable-match matcher frag accept)
  (let ((r (matcher frag)))
    (and r
         (or (accept (cdr r))
             ((car r))))))

; Return the first match.
(define (first-match matcher frag)
  (acceptable-match matcher frag (lambda (frag1) frag1)))

; Return true if the matcher matches all of the fragment.
(define (match-all? matcher frag)
  (acceptable-match matcher frag null?))

; Output all solutions.
(define (write-then-fail matcher frag)
  (let ((m (matcher frag)))
     (if m
         (begin
	   (write (cdr m))
           (newline)
           ((car m)))
         (void))))

; Some test cases.
(first-match matcher1 frag0) ; ⇒ #f

; A match must always match an entire prefix of a fragment.
; So, even though matcher1 finds a match in frag1,
; it does not find the match in (cons 'a frag1).
(first-match matcher1 frag1) ; ⇒ (t a)
(first-match matcher1 (cons 'a frag1)) ; ⇒ #f

(first-match matcher2 frag1) ; ⇒ #f
(first-match matcher2 frag2) ; ⇒ (a
;                                 a g t a t a t g g a t a
;                                 t a
;                                 a g t a t a t g g a t a
;                                 c g a t c c c t c g a t c t a)

; These matcher calls match the same prefix,
; so they return unmatched suffixes that are eq?.
(eq? (first-match matcher2 frag2)
     (first-match matcher3 frag2)) ; ⇒ #t

; matcher4 is lazy: it matches the empty fragment first,
; but you can force it to backtrack by insisting on progress.
(eq? (first-match matcher4 frag2)
     frag2) ; ⇒ #t
(eq? (first-match matcher2 frag2)
     (acceptable-match matcher4
           frag2
           (lambda (frag) (if (eq? frag frag2) #f frag))))
; ⇒ #t

; Here null? is being used as an acceptor.
; It accepts only the empty unmatched suffix,
; so it forces matcher4 to backtrack until all of frag2 is matched.
(match-all? matcher1 frag2) ; ⇒ #f
(match-all? matcher4 frag2) ; ⇒ #t