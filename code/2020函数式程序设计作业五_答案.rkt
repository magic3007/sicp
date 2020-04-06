****1: 补集与对称差
by 1700012751
#lang racket

(define (minus a b)
  (define in? (curryr member b))
  (filter (negate in?) a))

(define (symmetric-diff a b)
  (append (minus a b) (minus b a)))

(define (format x) (sort (remove-duplicates x) <))

(define (readcases)
  (let* ([a (read)]
         [b (read)])
    (if (eq? a eof)
        (void)
        (begin
          (display (format (minus a b)))
          (display (format (symmetric-diff a b)))
          (newline)
          (readcases)))))

(readcases)

by 1800013090
#lang racket

(define (delta A B)
  (define (notin? B)
    (lambda (x)
      (cond
        ((null? B) #t)
        ((= x (car B)) #f)
        (else ((notin? (cdr B)) x)))))
  (filter (notin? B) A))

(define (quicksort lst)
  (if (null? lst) '()
      (let ((x (car lst)))
        (append
         (quicksort (filter (lambda (a) (< a x)) (cdr lst)))
         (list x)
         (quicksort (filter (lambda (a) (> a x)) (cdr lst)))))))

(define (work)
  (let ((A (read)) (B (read)))
    (if (eq? A eof)
        (void)
        (begin
          (display (quicksort (delta A B)))
          (displayln (quicksort (append (delta A B) (delta B A))))
          (work)))))

(work)

by 1800012907
#lang racket
(define (cha lst x)
  (cond
    ((null? lst) (list x))
    ((= x (car lst)) lst)
    ((< x (car lst)) (cons x lst))
    (else (cons (car lst) (cha (cdr lst) x)))))
(define (sort lst)
  (if (null? lst)
      '()
      (cha (sort (cdr lst)) (car lst))))
(define (in? x B)
  (if (null? B)
      #f
      (or (= x (car B)) (in? x (cdr B)))))
(define (f A B)
  (if (null? A)
      '()
      (if (in? (car A) B)
          (f (cdr A) B)
          (cons (car A) (f (cdr A) B)))))
(define (doit)
  (define A (read))
  (define B (read))
  (if (eq? A eof)
      (void)
      (begin
        (display (sort (f A B)))
        (displayln (sort (append (f A B) (f B A))))
        (doit))))
(doit)

by 1800012751
#lang racket
(define (sort lst)
  (if (null? lst) '()
      (let ((m (car lst))
            (rst (cdr lst)))
        (append (sort (filter (lambda (x) (< x m)) rst))
                (list m)
                (sort (filter (lambda (x) (> x m)) rst))))))
                

(define (Find B x)
  (cond ((null? B) #f)
        ((< x (car B)) #f)
        ((= x (car B)) #t)
        (else (Find (cdr B) x))))

(define (Rc A B)
  (filter (lambda (x) (not (Find B x))) A))

(define (Print A B)
  (display (Rc A B))
  (displayln (sort (append (Rc A B) (Rc B A)))))

(define (Read)
  (let ((A (read))
        (B (read)))
    (if (eq? A eof)
        (void)
        (begin (Print (sort A) (sort B)) (Read)))))

(Read)

****2: 多叉树的生长
by 1800011789
#lang racket

(define (gen root a b c)
  (if (> b 0) (append (list root) (map (lambda (node) (gen node a (- b 1) c)) (map (lambda (x) (+ x (* root a))) c))) (list root)))

(do ((a (read)(read))(b (read)(read))(c (read)(read)))
((eq? eof a)(void))
  (displayln (gen 0 b c a))
  )

by 1700013005
#lang racket
(define (T i L n d)
  (define (G k)
    (if (= k (length L))
        '()
        (cons (T (+ (* i n) (list-ref L k)) L n (- d 1)) (G (+ k 1)))))
  (if (= d 0) (list i) (cons i (G 0))))

(define (Test)
  (let ((L (read)) (n (read)) (d (read)))
    (if (eq? L eof)
        (void)
        (begin (displayln (T 0 L n d)) (Test)))))

(Test)

by 1800012789
#lang racket

(define (grow root b d pattern)
  (if (= d 0)
      (list root)
      (append (list root)
              (map (lambda (node) (grow node b (- d 1) pattern)) (map (lambda (x) (+ x (* root b))) pattern)))))

(define (main pattern b d)
  (if (eq? pattern eof)
      (void)
      (begin (displayln (grow 0 b d pattern)) (main (read) (read) (read)))))

(main (read) (read) (read))

by 1600012732
#lang racket
(define (main l w h)
  (define id 0)
  (define (tree root height)
    (if (= height 0)
        (list root)
        (cons root (map (lambda (x) (tree (+ (* root w) x) (- height 1))) l))))
  (tree 0 h))
(define (myloop)
  (let ((l (read))
        (w (read))
        (h (read)))
    (if (eq? l eof)
        (void)
        (begin (displayln (main l w h))
               (myloop)))))
(myloop)

****3: SICP课本练习2.69 哈夫曼编码树
by 1700013005
#lang racket
(define (generate-huffman-tree L)
  (define (T st)
    (if (<= (length st) 1)
        (car st)
        (T (adjoin-set (make-code-tree (car st) (cadr st)) (cddr st)))))
  (T (make-leaf-set L)))
(define (encode-L s T)
  (define (E c T)
    (if (leaf? T)
        '()
        (if (memq c (symbols (left-branch T)))
            (cons 0 (E c (left-branch T)))
            (cons 1 (E c (right-branch T))))))
  (if (null? s)
      '()
      (append (E (string->symbol (string (car s))) T) (encode-L (cdr s) T))))
(define (encode s T)
  (encode-L (string->list (symbol->string s)) T))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

  
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
;  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)


(define huffman-tree '())
(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))

(myloop)

by 1800012802
#lang racket
(define (gen s)
  (if(null? (cdr s))s
     (gen (adjoin-set (make-code-tree (car s) (cadr s)) (cddr s)))))
(define (generate-huffman-tree s)
  (car(gen (make-leaf-set s))))
(define (find x l)
  
  (if(null? l)#f
     (if(equal? x (car l))#t
        (find x (cdr l)))))
(define (Find c t)
  (if(leaf? t)'()
     (if(find (string->symbol c) (symbols (left-branch t)))
        (cons 0 (Find c (left-branch t)))
        (cons 1 (Find c (right-branch t))))))
(define (Encode chars tree)
  (if(= 1 (string-length chars))(Find (substring chars 0 1) tree)
   (append (Find (substring chars 0 1) tree) (Encode (substring chars 1 (string-length chars)) tree))))
(define (encode chars tree)(Encode (symbol->string chars)tree))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

  
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
;  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)


(define huffman-tree '())
(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))

(myloop)

by 1800012945
#lang racket
(define (generate-huffman-tree items)
  (define (combine set)
    (if (null? (cdr set))
        (car set)
        (combine (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))
  (combine (make-leaf-set items)))

(define (encode item tree)
  (define (iter alpha branch)
    (if (leaf? branch)
        '()
        (let ((left (memq alpha (symbols (left-branch branch)))))
          (if left
              (cons 0 (iter alpha (left-branch branch)))
              (cons 1 (iter alpha (right-branch branch)))))))
  (define (aqua lst)
    (if (null? lst)
        '()
        (append (iter (car lst) tree) (aqua (cdr lst)))))
  (aqua (convert item)))

(define (convert item)
  (map string->symbol (map string (string->list (symbol->string item)))))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

  
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
;  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)


(define huffman-tree '())
(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))

(myloop)

by 1900012959(lny)
#lang racket
(define (generate-huffman-tree chs)
  (define st (make-leaf-set chs))
  (define (f1 nods)
    (if (null? (cdr nods)) (car nods)
        (f1 (adjoin-set (make-code-tree (car nods) (cadr nods)) (cddr nods)))
        )
    )
  (f1 st)
  )
(define (encode str tre)
  (define (dfs nd path)
    (if (leaf? nd) (list (cons (symbol->string (symbol-leaf nd)) path))
        (append (dfs (car nd) (append path '(0))) (dfs (cadr nd) (append path '(1))))
        )
    )
  (define leafs (dfs tre '()))
  (define (getbits x ls)
    (if (equal? x (caar ls)) (cdar ls)
        (getbits x (cdr ls))
        )
    )
  (define (f1 s)
    (if (null? s) '()
        (append (getbits (string (car s)) leafs) (f1 (cdr s)))
        )
    )
  (f1 (string->list (symbol->string str)))
  )
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

  
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
;  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)


(define huffman-tree '())
(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))

(myloop)

