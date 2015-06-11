#lang racket

(define match-junk
  (lambda (k frag)
    (or (call/cc (lambda (b)
      (cons (lambda () (b #f)) frag)))
    (and (< 0 k)
      (pair? frag)
      (match-junk (- k 1) (cdr frag))))))

(define match-*
  (lambda (matcher frag)
    (or (call/cc (lambda (b) 
      (cons (lambda () (b #f)) frag)))
    (let ((head (matcher frag)))
      (if head
        (and (not (eq? (car head) frag))
          (or (match-* matcher (cdr head))
            ((car head))))
      #f )))))

(define make-matcher
  (lambda (pat)
    (cond

      ((symbol? pat)
        (lambda (frag)
          (and (pair? frag)
            (eq? pat (car frag))
            (call/cc (lambda (b) (cons (lambda () (b #f)) (cdr frag)))))))

      ((eq? 'or (car pat))
        (let make-or-matcher ((pats (cdr pat)))
          (if (null? pats)
            (lambda (frag) (call/cc (lambda (b) (cons (b #f) frag))))     
            (let ((head-matcher (make-matcher (car pats)))
              (tail-matcher (make-or-matcher (cdr pats))))
              (lambda (frag)
                (let ((head-frag (head-matcher frag)))
                  (if head-frag
                    (or (call/cc (lambda (b) (cons (lambda () (b #f)) (cdr head-frag))))
                      (tail-matcher frag))
                    (tail-matcher frag))))))))
      
      ((eq? 'list (car pat))
        (let make-list-matcher ((pats (cdr pat)))
          (if (null? pats)
            (lambda (frag) 
              (call/cc (lambda (b) (cons (lambda () (b #f)) frag))))
            (let ((head-matcher (make-matcher (car pats)))
              (tail-matcher (make-list-matcher (cdr pats))))
            (lambda (frag)
                (let ((head-frag (head-matcher frag)))
                  (if head-frag
                    (let ((tail-frag (tail-matcher (cdr head-frag))))
                      (if tail-frag
                        tail-frag
                        ((car head-frag))))
                    #f )))))))

      ((eq? 'junk (car pat))
        (let ((k (cadr pat)))
          (lambda (frag)
            (match-junk k frag))))

      ((eq? '* (car pat))
        (let ((matcher (make-matcher (cadr pat))))
          (lambda (frag)
            (match-* matcher frag)))))))
