;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #t)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
	#f
	(graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c) '(words for node b))
   (make-graph-element 'c '(a) '(words for node c)))))

; (find-graph-element test-graph 'b)
; (find-graph-element test-graph 'z)
; (find-node-children test-graph 'b)
; (find-node-children test-graph 'z)
; (find-node-contents test-graph 'b)
; (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))

(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))

;; test
; (DFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)

(define (BFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

;; test
;(BFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)

;;-----------why BFS-simple works?-----------
;;changing the append order brings the 
;;least recent tree level to the end of the
;;list to search in.
;; you will need to write a similar search procedure that handles cycles

;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search-with-cycles initial-state goal? successors merge graph)
  (define visited-nodes (list initial-state))
  
  (define (visited? node)       ; Node -> boolean
    (if (find (lambda (item) 
                (equal? node item))
              visited-nodes)
        #t
        #f))

  (define (filter-visited candidate-nodes)        ; list<Node> -> list<Node>|null
    (cond ((null? candidate-nodes) '())
          ((not (visited? (car candidate-nodes))) (append (list (car candidate-nodes))
                                                          (filter-visited (cdr candidate-nodes))))
          (else (filter-visited (cdr candidate-nodes)))))

  (define (search-inner still-to-do)
    ;(write-line still-to-do)
    (if (null? still-to-do)
	      #f
        (let ((current (car still-to-do)))
            (if *search-debug*
                (write-line (list 'now-at current)))
            (append! visited-nodes (list current))
            ;(write-line visited-nodes)
            (if (goal? current)
                #t
                (search-inner
                  (filter-visited (merge (successors graph current) (cdr still-to-do))))))))
  (search-inner (list initial-state)))

(define (DFS start goal? graph)
  (search-with-cycles start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))


; (DFS 'a
;      (lambda (node) (eq? node 'l))
;      test-cycle)

; (DFS 'http://sicp.csail.mit.edu/
;      (lambda (node) #f)
;      the-web)

(define (BFS start goal? graph)
  (search-with-cycles start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

;; test
;(BFS 'a
;     (lambda (node) (eq? node 'l))
;     test-cycle)

; (BFS 'http://sicp.csail.mit.edu/
;      (lambda (node) #f)
;      the-web)

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;;   list of Index-Entries.  Each Index-Entry associates
;;   a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;; 

(define (make-index-entry key)  ; Key -> Index-entry
  (list key))

(define (val-found-in-entry? index-entry val)   ; Index-Entry, Val -> boolean
  (if (find (lambda (item) 
              (equal? val item))
            (cadr index-entry))
      #t
      #f)
  )

(define (insert-val-to-entry! index-entry val)  ; Index-Entry, Val -> Index-Entry
  (if (null? (cdr index-entry))
      (append! index-entry (list (list val)))
      (let ((val-exists (val-found-in-entry? index-entry val)))
           (if (not val-exists)
               (append! (cadr index-entry) 
                        (list val)))))
  index-entry)
;; test
;(define test-entry (make-index-entry 'ethiopia))
;(insert-val-to-entry! test-entry 'addis)
;(insert-val-to-entry! test-entry 'axum)
;Value: (ethiopia (addis axum))


(define (make-index)            ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))
      
; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        #f)))

(define (insert-entry-to-index! index index-entry)   ; Index, Index-Entry -> Index
    (append! index
             (list index-entry))
  index)  

;; TO BE IMPLEMENTED
(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((index-entry (find-entry-in-index index key)))
      (if (null? index-entry)
        ;; no entry -- create and insert a new one...
        (insert-entry-to-index! index
                              (insert-val-to-entry! (make-index-entry key)
                                                    value))

        ;; entry exists -- insert value if not already there...
        (insert-val-to-entry! index-entry
                              value))
  index))

;; Testing
;; (define test-index (make-index))
;; (add-to-index! test-index 'key1 'value1)
;; (add-to-index! test-index 'key2 'value2)
;; (add-to-index! test-index 'key1 'another-value1)
;; 
;; (find-in-index test-index 'key1)
;; (find-in-index test-index 'key2)


;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))


;; The real definition of THE-WEB we'll use is in another file, 
;; including all of the words in the documents.

;;(define the-web
;;  (list
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/
;;    '(http://sicp.csail.mit.edu/SchemeImplementations/
;;      http://sicp.csail.mit.edu/projects/)
;;    '(... words extracted from http://sicp.csail.mit.edu/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/projects/
;;    '(http://sicp.csail.mit.edu/collaborative-work.html
;;      http://sicp.csail.mit.edu/getting-help.html)
;;    '(... words extracted from http://sicp.csail.mit.edu/SchemeImplementations/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/getting-help.html
;;    '(http://sicp.csail.mit.edu/
;;      http://sicp.csail.mit.edu/SchemeImplementations/)
;;    '(... words extracted from http://sicp.csail.mit.edu/getting-help.html))
;;   ...))


;;--------------------
;; Searching the Web

;; you need to write expressions to search the web using different search
;; strategies


;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; TO BE IMPLEMENTED
;; add-document-to-index!: Index, Web, URL
(define (add-document-to-index! index web url)
  (let ((words-list (find-URL-text web url))
        (add-word-index-entry (lambda (word)
                                (add-to-index! index word url))))
       (for-each add-word-index-entry words-list)))


;; Example use
;; 
;; (define the-web-index (make-index))
;; 
;; (add-document-to-index! the-web-index
;;                         the-web 
;;                         'http://sicp.csail.mit.edu/)
;; 
;; (find-in-index the-web-index 'help)
;; ;Value: (http://sicp.csail.mit.edu/)
;; 
;; (find-in-index the-web-index '*magic*)
;; ;Value: #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Crawler that build index of the words found 
;; in all urls found in the given web in full-text mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-web-index web start-url)
  (define the-web-index (make-index))  ; creat index for the whole web
  
  ;; procedure that add contents of a URL into the-web-index
  (define add-url-content-to-web-index
    (lambda (url)
      (add-document-to-index! the-web-index web url)))
  
  ;; search procedure that accept procedure to be applied on every node traversed
  (define (search initial-state goal? successors merge graph proc-to-apply)
    (define visited-nodes (list initial-state))
    
    (define (visited? node)       ; Node -> boolean
      (if (find (lambda (item) 
                  (equal? node item))
                visited-nodes)
          #t
          #f))

    (define (filter-visited candidate-nodes)        ; list<Node> -> list<Node>|null
      (cond ((null? candidate-nodes) '())
            ((not (visited? (car candidate-nodes))) (append (list (car candidate-nodes))
                                                            (filter-visited (cdr candidate-nodes))))
            (else (filter-visited (cdr candidate-nodes)))))

    (define (search-inner still-to-do)
      ;(write-line still-to-do)
      (if (null? still-to-do)
          #f
          (let ((current (car still-to-do)))
              ;(if *search-debug*
              ;    (write-line (list 'now-at current)))
              (append! visited-nodes (list current))
              ;(write-line visited-nodes)
              (proc-to-apply current)  ;; add contents of current URL to the-web-index
              (if (goal? current)
                  #t
                  (search-inner
                    (filter-visited (merge (successors graph current) (cdr still-to-do))))))))
    (search-inner (list initial-state)))

  ;; go over all the urls using BFS strategy
  (define bfs 
    (search start-url
            (lambda (node) #f)
	          find-node-children
	          (lambda (new old) (append old new))
	          web
            add-url-content-to-web-index))
  
  ;; define procedure that return values of a given index-entry from the-web-index
  (define find-key-word
    (lambda (key)
      (find-in-index the-web-index key)))
  find-key-word)

(define (find-docs web start-node) (make-web-index web start-node))
;; test
; (define find-documents (find-docs the-web 'http://sicp.csail.mit.edu/)) ;build the-web-index
; (find-documents 'collaborative)  ;search in the-web-index
;Value: (http://sicp.csail.mit.edu/ http://sicp.csail.mit.edu/psets)

;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Crawler that searches
;; a word in the web dynamically
;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-search web start-node word *search-all*)
  (define nodes-containing-word (list '()))
  (define visited-nodes (list start-node))
  (define merge 
    (lambda (new old)
     (append old new)))
  (define (word-found-in-node? node)    ; Node -> boolean
    (if (find (lambda (item)
                (equal? item word))
              (find-node-contents web node))
        #t
        #f))

  (define (visited? node)       ; Node -> boolean
    (if (find (lambda (item) 
                (equal? node item))
              visited-nodes)
        #t
        #f))

  (define (filter-visited candidate-nodes)        ; list<Node> -> list<Node>|null
    (cond ((null? candidate-nodes) '())
          ((not (visited? (car candidate-nodes))) (append (list (car candidate-nodes))
                                                          (filter-visited (cdr candidate-nodes))))
          (else (filter-visited (cdr candidate-nodes)))))

  (define (search-inner still-to-do)
    ;(write-line still-to-do)
    (if (null? still-to-do)
        (let ((result (cdr nodes-containing-word)))
             (if (null? result)
                #f
                result))

        (let ((current (car still-to-do)))
            ;(if *search-debug*
            ;    (write-line (list 'now-at current)))
            (append! visited-nodes (list current))
            ;(write-line visited-nodes)
            ;(write-line nodes-containing-word)
            (if *search-all*
              (let ()
                  (if (word-found-in-node? current)
                      (append! nodes-containing-word (list current)))
                  (search-inner
                    (filter-visited (merge (find-node-children web current) (cdr still-to-do)))))
              
              (if (word-found-in-node? current)
                current
                (search-inner
                  (filter-visited (merge (find-node-children web current) (cdr still-to-do)))))))))  
  (search-inner (list start-node)))

(define (search-any web start-node word)    ; Graph, Node, Key -> Node|#f
  (dynamic-search web start-node word #f))
;; test
;(search-any the-web 'http://sicp.csail.mit.edu/ 'collaborative)
;Value: http://sicp.csail.mit.edu/

(define (search-all web start-node word)    ; Graph, Node, Key -> list<Node>|#f
  (dynamic-search web start-node word #t))

;; test
;(search-all the-web 'http://sicp.csail.mit.edu/ 'collaborative)
;Value: (http://sicp.csail.mit.edu/ http://sicp.csail.mit.edu/psets)

;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      (newline)
      val)))

;;-----------------------------------------
;; Comparison Web Index Vs Dynamic Search
;;-----------------------------------------      
;(define time-test
;  (lambda ()
;    (let  ;; create test webs and their corresponding find-docs procedures
;          ((t-web1 (generate-random-web 75))
;          (t-web2 (generate-random-web 100))
;          (t-web3 (generate-random-web 150)))
;
;          ;; time test for each test-web
;          (newline)
;          (display (list 'search-any 'on 't-web1 'for '"help"))
;          (timed search-any t-web1 '*start* 'help)
;
;          (newline)
;          (display (list 'search-any 'on 't-web2 'for '"help"))
;          (timed search-any t-web2 '*start* 'help)
;
;          (newline)
;          (display (list 'search-any 'on 't-web3 'for '"help"))
;          (timed search-any t-web3 '*start* 'help)
;
;          ;search word not found in web
;          (newline)
;          (display (list 'search-any 'on 't-web1 'for '"Kirubel"))
;          (timed search-any t-web1 '*start* 'Kirubel)
;
;          (newline)
;          (display (list 'search-any 'on 't-web2 'for '"Kirubel"))
;          (timed search-any t-web2 '*start* 'Kirubel)
;
;          (newline)
;          (display (list 'search-any 'on 't-web3 'for '"Kirubel"))
;          (timed search-any t-web3 '*start* 'Kirubel)
;
;          (newline)
;          (display (list 'search-all 'on 't-web1 'for '"help"))
;          (timed search-all t-web1 '*start* 'help)
;
;          (newline)
;          (display (list 'search-all 'on 't-web2 'for '"help"))
;          (timed search-all t-web2 '*start* 'help)
;
;          (newline)
;          (display (list 'search-all 'on 't-web3 'for '"help"))
;          (timed search-all t-web3 '*start* 'help)
;
;          ;make-web-index
;          (newline)
;          (display (list 'build-web-index 'for 't-web1))
;          (timed find-docs t-web1 '*start*)
;          (define find-docs-tw1 (find-docs t-web1 '*start*))  ;build Web-index for t-web1
;
;          (newline)
;          (display (list 'build-web-index 'for 't-web2))
;          (timed find-docs t-web2 '*start*)
;          (define find-docs-tw2 (find-docs t-web2 '*start*))  ;build Web-index for t-web2
;
;          (newline)
;          (display (list 'build-web-index 'for 't-web3))
;          (timed find-docs t-web3 '*start*)
;          (define find-docs-tw3 (find-docs t-web3 '*start*))  ;build Web-index for t-web3
;
;          ;find documents containing the word 'help
;          (newline)
;          (display (list 'find-docs-tw1 'for 'the 'word '"help"))
;          (timed find-docs-tw1 'help)
;
;          (newline)
;          (display (list 'find-docs-tw2 'for 'the 'word '"help"))
;          (timed find-docs-tw2 'help)
;
;          (newline)
;          (display (list 'find-docs-tw3 'for 'the 'word '"help"))
;          (timed find-docs-tw3 'help)
;
;          ;find documents containing the word 'Kirubel, which is not found
;          (newline)
;          (display (list 'find-docs-tw1 'for 'the 'word '"Kirubel"))
;          (timed find-docs-tw1 'Kirubel)
;
;          (newline)
;          (display (list 'find-docs-tw2 'for 'the 'word '"Kirubel"))
;          (timed find-docs-tw2 'Kirubel)
;
;          (newline)
;          (display (list 'find-docs-tw3 'for 'the 'word '"Kirubel"))
;          (timed find-docs-tw3 'Kirubel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create an optimized index by converting the 
;; list of index entries into a vector sorted
;; using thier key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (optimized-index ind)
  (define opt-ind (make-vector 2 'index))
  (vector-set! opt-ind 1 (sort! (list->vector (cdr ind))
                                (lambda (ind-entry1 ind-entry2)
                                  (symbol<? (car ind-entry1)
                                            (car ind-entry2)))))
  opt-ind)

;;-------------------------
;; search the optimized index and return 
;; the index-entry with matching entry key
;;-------------------------
(define (find-entry-in-opt-index opt-index key)   ; optimized-index, key -> Index-entry
  (vector-binary-search (vector-ref opt-index 1)
                              symbol<?  car  key))

;; test
; (define the-web-index (make-index))
; (add-document-to-index! the-web-index
;                         the-web
;                         'http://sicp.csail.mit.edu/)
; (add-document-to-index! the-web-index
;                         the-web
;                         'http://sicp.csail.mit.edu/psets)

; (define opt-ind (optimized-index the-web-index))
; (equal? (find-entry-in-index the-web-index 'help)
;         (find-entry-in-opt-index opt-ind 'help))
;Value: #t

; (equal? (find-entry-in-index the-web-index 'how)
;         (find-entry-in-opt-index opt-ind 'how))
;Value: #t

;;-------------------------
;; search the optimized index and return 
;; the list of values for an index-entry
;; with matching entry key using 
;; binary-search algorithm
;;-------------------------
(define (find-entry-in-optimized-index optind key)   ; optimized-index, key -> list<Val>
  (define (index-binary-search vector-entry key)
    (if (= (vector-length vector-entry) 1)
        (let ((entry (vector-ref vector-entry 0)))
             (if (equal? key (car entry))
                 entry
                 #f)))
    (let ((mid-entry (vector-ref vector-entry
                                 (ceiling (/ (vector-length vector-entry) 2))))
          (mid-index (ceiling (/ (vector-length vector-entry) 2))))
         (cond ((symbol<? key (car mid-entry)) 
                (index-binary-search (subvector vector-entry
                                               0
                                               mid-index)
                                     key))
               ((symbol>? key (car mid-entry)) 
                (index-binary-search (subvector vector-entry
                                                (+ mid-index 1)
                                                (vector-length vector-entry))
                                     key))
               (else mid-entry))))
    (let ((entry (index-binary-search (vector-ref optind 1) key)))
         (if entry
             (cadr entry)
             #f)))

;; test
; (equal? (find-in-index the-web-index 'help)
;         (find-entry-in-optimized-index opt-ind 'help))
;Value: #t

;; run-time test
;-----------------------------------------------
;build and retun a web index for the entire web
(define (build-web-index web start-url)
  (define the-web-index (make-index))  ; creat index for the whole web
  
  ;; procedure that add contents of a URL into the-web-index
  (define add-url-content-to-web-index
    (lambda (url)
      (add-document-to-index! the-web-index web url)))
  
  ;; search procedure that accept procedure to be applied on every node traversed
  (define (search initial-state goal? successors merge graph proc-to-apply)
    (define visited-nodes (list initial-state))
    
    (define (visited? node)       ; Node -> boolean
      (if (find (lambda (item) 
                  (equal? node item))
                visited-nodes)
          #t
          #f))

    (define (filter-visited candidate-nodes)        ; list<Node> -> list<Node>|null
      (cond ((null? candidate-nodes) '())
            ((not (visited? (car candidate-nodes))) (append (list (car candidate-nodes))
                                                            (filter-visited (cdr candidate-nodes))))
            (else (filter-visited (cdr candidate-nodes)))))

    (define (search-inner still-to-do)
      ;(write-line still-to-do)
      (if (null? still-to-do)
          #f
          (let ((current (car still-to-do)))
              ;(if *search-debug*
              ;    (write-line (list 'now-at current)))
              (append! visited-nodes (list current))
              ;(write-line visited-nodes)
              (proc-to-apply current)  ;; add contents of current URL to the-web-index
              (if (goal? current)
                  #t
                  (search-inner
                    (filter-visited (merge (successors graph current) (cdr still-to-do))))))))
    (search-inner (list initial-state)))

  ;; go over all the urls using BFS strategy
  (define bfs 
    (search start-url
            (lambda (node) #f)
	          find-node-children
	          (lambda (new old) (append old new))
	          web
            add-url-content-to-web-index))
  
  ;; define procedure that return values of a given index-entry from the-web-index
  the-web-index)

;------------------------------------------------------------
; apply a search procedure for a key on a given web index where
; the key will be searched num-iter times
(define (search-run-time proc key num-iter)
  (define apply-proc
    (lambda () 
      (map (lambda (item)
            (proc item)) 
          (make-list num-iter key))
      'done))
  (timed apply-proc))

;; begin run time test
;(define web-index (build-web-index the-web 'http://sicp.csail.mit.edu/)) ;index of an entire web
;(define opt-web-index (optimized-index web-index))
;
;(display (list 'run 'time 'of 'find-in-index))
;(search-run-time (lambda (key)
;                    (find-in-index web-index key))
;                 'help 
;                 10000)
;                
;(newline)
;(display (list 'run 'time 'of 'find-entry-in-optimized-index))
;(search-run-time (lambda (key)
;                    (find-entry-in-optimized-index opt-web-index key))
;                 'help 
;                 10000)
;
;(newline)
;(display (list 'run 'time 'of 'search-all))
;(search-run-time (lambda (key)
;                    (search-all the-web 'http://sicp.csail.mit.edu/ key))
;                 'help 
;                 10000)

;;-----------run time result description-------------
;; find-entry-in-optimized-index is faster than search all.
;; this is due to the fact that it uses binary search and unlike the
;; search all procedure which uses sequential search. But when compared to 
;; find-in-index procedure , it's slower and the reason for this is that 
;; find-in-index uses the assv pre-built searching procedure which is 
;; implemented in an optimal way and find-entry-in-optimized-index uses a
;; custom implementation of binary search where i used several built in procedures
;; and recursive calls to localy defined procedure.