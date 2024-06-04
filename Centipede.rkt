;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Centipede) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Benjamin Clauss and Nicholas Lailler
;; Problem Set 5

;; ------------------------------     Libraries     ----------------------------

(require 2htdp/universe)
(require 2htdp/image)

;; ------------------------------     Constants     ----------------------------

(define GRID-WIDTH 25)
(define GRID-HEIGHT 40)
(define CELL-SIZE 15)
(define BG (empty-scene (* CELL-SIZE GRID-WIDTH) (* CELL-SIZE GRID-HEIGHT)))

(define LEFT-BOUND 0)
(define RIGHT-BOUND (- GRID-WIDTH 1))
(define TOP (- GRID-HEIGHT 1))
(define BOTTOM 0)

(define PLAYER (square CELL-SIZE 'solid 'black))
(define BULLET (rectangle 3 8 'solid 'orange))
(define CENTIPEDE-CELL (square CELL-SIZE 'solid 'green))
(define TONGUE (triangle 5 'solid 'red))
(define LEFT-HEAD
  (overlay/align "left" "middle" (rotate 90 TONGUE) CENTIPEDE-CELL))
(define RIGHT-HEAD
  (overlay/align "right" "middle" (rotate 30 TONGUE) CENTIPEDE-CELL))

(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C (circle MUSHROOM-RADIUS "solid" 'LightSalmon))
(define MUSHROOM-2-C (circle MUSHROOM-RADIUS "solid" 'Salmon))
(define MUSHROOM-3-C (circle MUSHROOM-RADIUS "solid" 'OrangeRed))
(define MUSHROOM-4-C (circle MUSHROOM-RADIUS "solid" 'DarkRed))

(define WINNER (text "WINNER" 72 'black))
(define LOSER (text "LOSER" 72 'black))

;; ------------------------------     Data Definitions     ---------------------

;; A Mushroom is a one of:
;; - (make-mushroom Posn Number)
;; - 'empty
(define-struct mushroom (posn state))

;; mushroom-temp : Mushroom -> ??
(define (mushroom-temp m)
  (... (mushroom-posn m) ... (mushroom-state y) ...))

;; A LoM [Listof Mushroom] is one of
;; - empty
;; - (cons Mushroom LoM)

;; lom-temp : [Listof Mushroom] -> ??
#;(define (lom-temp lom)
  (cond
    [(empty? lom) ...]
    [else ... (first lom) ... (lom-temp (rest lom))]))

;; a CentipedeBody [Listof Posn] is one of:
;; - empty
;; - (cons Posn CentipedeBody)

;; centipede-body-temp : CentipedeBody -> ??
#;(define (centipede-body-temp cb)
  (cond
    [(empty? cb) ...]
    [else ... (first cb) ... (centipede-body-temp (rest cb))]))

;; A Centipede is a
;; (make-centipede VerticalDirection HorizontalDirection CentipedeBody)
;; A Direction is one of
;; - VerticalDirection
;; - HorizontalDirection
;; A VerticalDirection is one of
;; - 'up
;; - 'down
;; A HorizontalDirection is one of
;; - 'left
;; - 'right
(define-struct centipede (v-direction h-direction body))

;; centipede-temp : Centipede -> ??
(define (centipede-temp c)
  (... (centipede-v-direction c)
       ... (centipede-h-direction c) ... (centipede-body c) ...))

;; a LoC [Listof Centipede] is one of:
;; - empty
;; - (cons Centipede CentipedeList)

;; loc-temp : [Listof Centipede] -> ??
#;(define (loc-temp loc)
  (cond
    [(empty? loc) ...]
    [else ... (first loc) ... (loc-temp loc)]))

;; A Shooter is a Posn

;; A Bullet is one of:
;; - Posn
;; - 'none

;; bullet-temp : bullet -> ??
(define (bullet-temp b)
  (cond
    [(symbol? b) ...]
    [else ...]))

(define-struct world (LoM LoC bullet shooter move))
;; A world is a (make-world [Listof Mushroom] [Listof Centipede] Bullet Shooter)
;; -A LoM is a List of Mushrooms
;; -A LoC is a List of a Centipede
;; -A bullet is a Bullet
;; -A shooter is a Shooter
;; -A move is a Number

;; world-temp : World -> ??
(define (world-temp w)
  (... (world-LoM w) ...
       (world-LoC w) ...
       (world-bullet w) ...
       (world-shooter w) ...
       (world-move w) ...))

;; ------------------------------     General Functions     --------------------

;; posn=? : Posn Posn -> Boolean
(define (posn=? posn1 posn2)
  (and (= (posn-x posn1) (posn-x posn2)) (= (posn-y posn1) (posn-y posn2))))

(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) true)
(check-expect (posn=? (make-posn 1 1) (make-posn 1 2)) false)

;; ------------------------------     Mushroom Generation     ------------------

;; contains : [Listof Numbers] Number -> Boolean
;; This function takes a list of numbers and returns true if the
;; given number is in that list
(define (contains? l val)
  (cond
    [(empty? l) false]
    [(cons? l) (or (= (first l) val) (contains? (rest l) val))]))

(check-expect (contains? (cons 1 empty) 1) true)
(check-expect (contains? (cons 1 empty) 6) false)
(check-expect (contains? (cons 5 (cons 6(cons 1 empty))) 6) true)
(check-expect (contains? (cons 5 (cons 6(cons 1 empty))) 7) false)

;; count : List -> Number
;; This function takes a list and returns the number of items in it.
(define (count list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ 1 (count(rest list)))]))

(check-expect (count (cons 1 (cons 2 empty))) 2)

;; rand-mushroom : Number Number [Listof Number] [Listof Number] -> Mushroom
;; This functions takes in two lists of number and two random numbers and makes
;; sure those numbers aren't on the lists. If they aren't, it creates a
;; mushroom out of the numbers. If they are, it returns 'empty.
(define (rand-mushroom x y countedx countedy)
     (if (and (contains? countedx x) (contains? countedy y))
         'empty (make-mushroom (make-posn x y) 4)))

(define MUSHROOM-EX0 (make-mushroom (make-posn 0 0) 4))
(define MUSHROOM-EX1 (make-mushroom (make-posn 0 1) 4))

(check-expect (rand-mushroom 0 0 (cons 1 empty) (cons 1 empty)) MUSHROOM-EX0)
(check-expect (rand-mushroom 0 1 (cons 1 empty) (cons 1 empty)) MUSHROOM-EX1)
(check-expect (rand-mushroom 1 1 (cons 1 empty) (cons 1 empty)) 'empty)

;; generate-initial-field :
;; [Listof Mushroom] Number [Listof Number] [Listof Number] Mushroom ->
;; [Listof Mushroom]
;; This function takes in a list of mushrooms, a counter, two lists of
;; numbers and a mushroom. If the mushroom's position is not on the list,
;; it adds the mushroom to the list. Otherwise it adds 'empty.
(define (generate-initial-field LoM num countedx countedy mushroom)
  (cond
    [(> num 0) (cond
                  [(mushroom? mushroom)
                   (generate-initial-field
                    (cons mushroom LoM)
                    (sub1 num)
                    (cons (posn-x (mushroom-posn mushroom)) countedx)
                    (cons (posn-y (mushroom-posn mushroom)) countedy)
                    (rand-mushroom (random (+ 1 (- GRID-WIDTH 1)))
                                   (+ (random (- GRID-HEIGHT 2)) 1)
                                   countedx
                                   countedy))]
                  [else
                   (generate-initial-field
                    LoM num countedx countedy
                    (rand-mushroom
                     (random (+ 1 (- GRID-WIDTH 1)))
                     (+ (random (- GRID-HEIGHT 2)) 1) countedx countedy))])]
    [else LoM]))

(check-expect (count (generate-initial-field empty 1 empty empty 'empty)) 1)
(check-expect (count (generate-initial-field empty 40 empty empty 'empty)) 40)

;; ------------------------------     Tick Functions     -----------------------

;; update-all : World -> World
;; takes a world, checks a series of conditions, and returns
;; a new world based on the results
(define (update-all world)
  (cond
    [(= (modulo (/ (world-move world) 0.25) 8) 0) (move-all world)]
    [else (move-some world)]))

;; move-all : World -> World
;; moves all parts of world
(define (move-all w)
  (cond
    [(mushroom? (bullet-mushroom-collision (world-bullet w) (world-LoM w)))
     (make-world (update-mushroom-list
                  (bullet-mushroom-collision
                   (world-bullet w) (world-LoM w)) (world-LoM w))
                 (move-centipedes (world-LoC w) (world-LoM w))
                 'none
                 (world-shooter w)
                 (+ 0.25 (world-move w)))]
    [(bullet-centipedes-collision? (world-LoC w) (world-bullet w))
     (make-world (add-mushroom (world-LoM w) (world-bullet w))
                 (delete-empty-centipedes
                  (split-centipedes (world-LoC w) (world-bullet w)))
                 'none
                 (world-shooter w)
                 (+ 0.25 (world-move w)))]  
    [else (make-world (world-LoM w)
                      (move-centipedes (world-LoC w) (world-LoM w))
                      (move-bullet (world-bullet w))
                      (world-shooter w)
                      (+ 0.25 (world-move w)))]))

;; move-some : World -> World
;; moves all parts of world except Centipede
(define (move-some w)
  (cond
    [(mushroom? (bullet-mushroom-collision (world-bullet w) (world-LoM w)))
     (make-world (update-mushroom-list
                  (bullet-mushroom-collision
                   (world-bullet w) (world-LoM w)) (world-LoM w))
                 (world-LoC w)
                 'none
                 (world-shooter w)
                 (+ 0.25 (world-move w)))]
    [(bullet-centipedes-collision? (world-LoC w) (world-bullet w))
     (make-world (add-mushroom (world-LoM w) (world-bullet w))
                 (delete-empty-centipedes
                  (split-centipedes (world-LoC w) (world-bullet w)))
                 'none
                 (world-shooter w)
                 (+ 0.25 (world-move w)))]
    [else
     (make-world (world-LoM w)
                 (world-LoC w)
                 (move-bullet (world-bullet w))
                 (world-shooter w)
                 (+ 0.25 (world-move w)))]))

;; bullet-mushroom-collision : Bullet [Listof Mushroom] -> Mushroom
;; takes a bullet and a list of mushrooms and returns a
;; hit mushroom if the posn of the bullet is the same as the posn as one of the
;; mushrooms. Otherwise return 'empty.
(define (bullet-mushroom-collision bullet LoM)
  (cond
    [(symbol? bullet) 'empty]
    [(empty? LoM) 'empty]
    [(cons? LoM) (if (posn=? bullet (mushroom-posn (first LoM)))
                      (first LoM)
                     (bullet-mushroom-collision bullet (rest LoM)))]))

(define BULLET0 (make-posn 0 0))
(define BULLET-NONE 'none)

(check-expect (bullet-mushroom-collision BULLET0 empty) 'empty)
(check-expect
 (bullet-mushroom-collision
  BULLET-NONE (cons MUSHROOM-EX1 (cons MUSHROOM-EX0 empty))) 'empty)
(check-expect
 (bullet-mushroom-collision BULLET0 (cons MUSHROOM-EX0 empty)) MUSHROOM-EX0)
(check-expect
 (bullet-mushroom-collision
  BULLET0 (cons MUSHROOM-EX1 (cons MUSHROOM-EX0 empty))) MUSHROOM-EX0)

;; increment-mushroom : Mushroom -> Mushroom
;; takes a mushroom, decreases its state and returns it
;; unless the initial state was 1 in which case it return 'empty
(define (increment-mushroom mushroom)
  (cond
    [(= 1 (mushroom-state mushroom)) 'empty]
    [else (make-mushroom (mushroom-posn mushroom)
                         (- (mushroom-state mushroom) 1))]))
(define MUSHROOM-EX0b (make-mushroom (make-posn 0 0) 3))
(define MUSHROOM-EX0c (make-mushroom (make-posn 0 0) 2))
(define MUSHROOM-EX0d (make-mushroom (make-posn 0 0) 1))

(check-expect (increment-mushroom MUSHROOM-EX0) MUSHROOM-EX0b)
(check-expect (increment-mushroom MUSHROOM-EX0b) MUSHROOM-EX0c)
(check-expect (increment-mushroom MUSHROOM-EX0c) MUSHROOM-EX0d)
(check-expect (increment-mushroom MUSHROOM-EX0d) 'empty)

;; update-mushroom-list : Mushroom [Listof Mushroom] -> [Listof Mushroom]
;; takes in a mushroom and a list of mushrooms, increments
;; the mushroom's state and adds the new mushroom, if it exists, to the list.
(define (update-mushroom-list mushroom LoM)
  (cond
    [(empty? LoM) empty]
    [(cons? LoM) (cond
                   [(posn=?(mushroom-posn mushroom) (mushroom-posn (first LoM)))
                    (if (symbol? (increment-mushroom (first LoM)))
                        (rest LoM)
                        (cons (increment-mushroom (first LoM)) (rest LoM)))]
                   [else
                    (cons
                     (first LoM)
                     (update-mushroom-list mushroom  (rest LoM)))])]))
(define MUSHROOM-EX0-LIST (cons MUSHROOM-EX0 empty))
(define MUSHROOM-EX0b-LIST (cons MUSHROOM-EX0b empty))
(define MUSHROOM-EX0c-LIST (cons MUSHROOM-EX0c empty))
(define MUSHROOM-EX0d-LIST (cons MUSHROOM-EX0d empty))
(check-expect (update-mushroom-list MUSHROOM-EX0 empty) empty)
(check-expect
 (update-mushroom-list MUSHROOM-EX0 MUSHROOM-EX0-LIST) MUSHROOM-EX0b-LIST)
(check-expect
 (update-mushroom-list MUSHROOM-EX0 MUSHROOM-EX0b-LIST) MUSHROOM-EX0c-LIST)
(check-expect
 (update-mushroom-list MUSHROOM-EX0 MUSHROOM-EX0c-LIST) MUSHROOM-EX0d-LIST)
(check-expect
 (update-mushroom-list
  MUSHROOM-EX0
  (cons MUSHROOM-EX1 MUSHROOM-EX0d-LIST)) (cons MUSHROOM-EX1 empty))

;; Number -> [Listof Segment]
;; This function takes a number less than the GRID-WIDTH and generates a list of
;; segments containg the given number of elements at a given start and height
(define (make-initial-centipede-body length start height)
  (cond
    [(= length 0) empty]
    [else
     (cons
      (make-posn (sub1 (+ length start)) height)
      (make-initial-centipede-body (sub1 length) start height))]))
(define CBODY (cons (make-posn 3 39)
                    (cons (make-posn 2 39)
                          (cons (make-posn 1 39)
                                (cons (make-posn 0 39) empty)))))

(check-expect (make-initial-centipede-body 0 0 39) empty)
(check-expect (make-initial-centipede-body 4 0 39) CBODY)
(check-expect
 (make-initial-centipede-body 4 1 39)
 (cons (make-posn 4 39)
       (cons (make-posn 3 39)
             (cons (make-posn 2 39) (cons (make-posn 1 39) empty)))))

;; bullet-centipedes-collision? [Listof Centipede] Bullet -> Boolean
(define (bullet-centipedes-collision? loc b)
  (cond
    [(symbol? b) false]
    [(empty? loc) false]
    [else (or
           (bullet-centipede-collision? (centipede-body (first loc)) b)
           (bullet-centipedes-collision? (rest loc) b))]))
(define BULLET-TOP0 (make-posn 0 39))
(define BULLET-TOP5 (make-posn 5 39))
(check-expect (bullet-centipedes-collision? centipede-list 'none) false)
(check-expect (bullet-centipedes-collision? centipede-list BULLET0) false)
(check-expect (bullet-centipedes-collision? centipede-list BULLET-TOP0) true)

;; bullet-centipede-collision? CentipedeBody Bullet -> Boolean
(define (bullet-centipede-collision? body b)
  (cond
    [(symbol? b) false]
    [(empty? body) false]
    [else (or
           (posn=? (first body) b)
           (bullet-centipede-collision? (rest body) b))]))
(define INITIAL-FOUR-CB-LIST (make-initial-centipede-body 4 0 39))
(define INITIAL-15-CB-LIST (make-initial-centipede-body 15 0 39))
(check-expect (bullet-centipede-collision? INITIAL-FOUR-CB-LIST 'none) false)
(check-expect
 (bullet-centipede-collision? INITIAL-FOUR-CB-LIST BULLET-TOP5) false)
(check-expect (bullet-centipede-collision? INITIAL-15-CB-LIST BULLET-TOP0) true)
(check-expect (bullet-centipede-collision? INITIAL-15-CB-LIST BULLET-TOP5) true)

;; add-mushroom : [Listof Mushroom] Bullet -> [Listof Mushroom]
;; This function takes a list of mushrooms and a bullet and adds a
;; mushroom at the bullet's position to the list
(define (add-mushroom lom b)
  (cons (make-mushroom b 4) lom))
(check-expect
 (add-mushroom empty BULLET-TOP0)
 (cons (make-mushroom (make-posn 0 39) 4) empty))
(check-expect
 (add-mushroom (cons (make-mushroom (make-posn 0 39) 4) empty) BULLET-TOP5)
 (cons
  (make-mushroom (make-posn 5 39) 4)
  (cons (make-mushroom (make-posn 0 39) 4) empty)))

;; make-initial-centipede : CentipedeBody -> Centipedes
;; This function takes a centipede body and creates a centipede facing
;; right and going down
(define (make-initial-centipede cb)
   (make-centipede 'down 'right cb))

(check-expect
 (make-initial-centipede INITIAL-FOUR-CB-LIST)
 (make-centipede 'down 'right INITIAL-FOUR-CB-LIST))
(check-expect
 (make-initial-centipede INITIAL-15-CB-LIST)
 (make-centipede 'down 'right INITIAL-15-CB-LIST))

;; split-centipedes : [Listof Centipede] Bullet -> [Listof Centipede]
;; splits every Centipede in the given list of Centipede that has to be split
(define (split-centipedes loc b)
  (cond
    [(empty? loc) empty]
    [else (cond
            [(bullet-centipede-collision? (centipede-body (first loc)) b)
              (cons (make-centipede
                     (centipede-v-direction (first loc))
                     (centipede-h-direction (first loc))
                     (split-top (centipede-body (first loc)) b))
                    (cons (make-centipede
                     (centipede-v-direction (first loc))
                     (centipede-h-direction (first loc))
                     (split-bottom (centipede-body (first loc)) b))
                          (split-centipedes (rest loc) b)))]
            [else
             (cons (first loc) (split-centipedes (rest loc) b))])]))

(check-expect
 (split-centipedes
  (cons (make-initial-centipede INITIAL-FOUR-CB-LIST) empty)
  (make-posn 3 39)) (cons (make-initial-centipede empty)
                          (cons (make-initial-centipede
                                 (make-initial-centipede-body 3 0 39)) empty)))
(check-expect
 (split-centipedes
  (cons (make-initial-centipede INITIAL-FOUR-CB-LIST) empty)
  (make-posn 2 39)) (cons (make-initial-centipede
                           (make-initial-centipede-body 1 3 39))
                          (cons
                           (make-initial-centipede
                            (make-initial-centipede-body 2 0 39)) empty))) 
(check-expect
 (split-centipedes
  (cons (make-initial-centipede INITIAL-FOUR-CB-LIST) empty)
  (make-posn 0 39)) (cons (make-initial-centipede
                           (make-initial-centipede-body 3 1 39))
                          (cons (make-initial-centipede empty) empty))) 

;; split-top : CentipedeBody Bullet -> CentipedeBody
;; gets body up to bullet
(define (split-top body b)
  (cond
    [(empty? body) empty]
    [else (cond
       [(posn=? (first body) b) empty]
       [else (cons (first body) (split-top (rest body) b))])]))

;; split-bottom : CentipedeBody Bullet -> CentipedeBody
;; gets body behind bullet
(define (split-bottom body b)
  (cond
    [(empty? body) empty]
    [else (cond
       [(posn=? (first body) b) (rest body)]
       [else (split-bottom (rest body) b)])]))

;; delete-empty-centipedes : [Listof Centipede] -> [Listof Centipede]
(define (delete-empty-centipedes loc)
  (cond
    [(empty? loc) empty]
    [else
     (cond
       [(empty? (centipede-body (first loc)))
        (delete-empty-centipedes (rest loc))]
       [else
        (cons (first loc) (delete-empty-centipedes (rest loc)))])]))
       
;; ------------------------------     Move Functions     -----------------------

;; move-centipedes : [Listof Centipede] [Listof Mushroom] -> [Listof Centipede]
(define (move-centipedes loc lom)
  (cond
    [(empty? loc) loc]
    [else
     (cons (move-centipede (first loc) lom) (move-centipedes (rest loc) lom))]))

;; move-centipede : Centipede [Listof Mushroom] -> Centipede
;; moves centipede to next position
(define (move-centipede c lom)
  (cond
    [(hits-wall? c) (move-row c)]
    [(hits-mushroom? c lom) (move-row c)]
    [else
      (make-centipede
       (centipede-v-direction c)
       (centipede-h-direction c)
       (move-centipede-body (centipede-body c) (centipede-h-direction c)))]))

;; hits-wall? : Centipede -> Boolean
;; checks if centipede will hit wall
(define (hits-wall? c)
  (or
   (and (= LEFT-BOUND (posn-x (first (centipede-body c))))
        (symbol=? 'left (centipede-h-direction c)))
   (and (= RIGHT-BOUND (posn-x (first (centipede-body c))))
        (symbol=? 'right (centipede-h-direction c)))))


;; hits-mushroom? : Centipede [Listof Mushroom] -> Boolean
;; checks if centipede will hit mushroom
(define (hits-mushroom? c lom)
  (cond
    [(empty? lom) false]
    [(symbol=? (centipede-h-direction c) 'right)
     (or (posn=? (mushroom-posn (first lom))
                  (make-posn
                   (+ (posn-x (first (centipede-body c))) 1)
                   (posn-y (first (centipede-body c)))))
          (hits-mushroom? c (rest lom)))]
    [(symbol=? (centipede-h-direction c) 'left)
     (or (posn=? (mushroom-posn (first lom))
                  (make-posn
                   (- (posn-x (first (centipede-body c))) 1)
                   (posn-y (first (centipede-body c)))))
          (hits-mushroom? c (rest lom)))]))

;; move-row : Centipede -> Centipede
;; moves centipede to next row
(define (move-row c)
  (cond
    [(or
      (and (= TOP (posn-y (first (centipede-body c))))
           (symbol=? 'up (centipede-v-direction c)))
      (and (= BOTTOM (posn-y (first (centipede-body c))))
           (symbol=? 'down (centipede-v-direction c))))
     (make-centipede
      (flip (centipede-v-direction c))
      (flip (centipede-h-direction c))
      (cons (new-head (first (centipede-body c))
                      (flip (centipede-v-direction c)))
            (all-but-last-seg (centipede-body c))))]
    [else
     (make-centipede
      (centipede-v-direction c)
      (flip (centipede-h-direction c))
      (cons (new-head (first (centipede-body c))
                      (centipede-v-direction c))
            (all-but-last-seg (centipede-body c))))]))

;; flip : Direction -> Direction
(define (flip dir)
  (cond
    [(symbol=? dir 'left) 'right]
    [(symbol=? dir 'right) 'left]
    [(symbol=? dir 'up) 'down]
    [(symbol=? dir 'down) 'up]))

;; move-centipede-body : CentipedeBody Direction -> CentipedeBody
;; moves centipede body in given direction
(define (move-centipede-body body dir)
  (cons (new-head (first body) dir) (all-but-last-seg body)))

;; new-head : Segment Direction -> Segment
(define (new-head seg dir)
  (cond
    [(symbol=? dir 'left) (make-posn (sub1 (posn-x seg)) (posn-y seg))]
    [(symbol=? dir 'right) (make-posn (add1 (posn-x seg)) (posn-y seg))]
    [(symbol=? dir 'up) (make-posn (posn-x seg) (add1 (posn-y seg)))]
    [(symbol=? dir 'down) (make-posn (posn-x seg) (sub1 (posn-y seg)))]))

;; all-but-last-seg : CentipedeBody -> CentipedeBody
;; Chops the last segment off the given non-empty list of CentipedeBody
(define (all-but-last-seg cb)
  (cond
    [(empty? (rest cb)) empty]
    [(cons? (rest cb)) 
     (cons (first cb) (all-but-last-seg (rest cb)))]))

;; move-bullet : Bullet -> Bullet
;; This function takes a bullet and moves it up .25. If the bullet
;; is past the top of the scene, it sets the bullet to 'none instead.
(define (move-bullet b)
  (cond
    [(symbol? b) b]
    [else (cond
            [(> (posn-y b) TOP) 'none]
            [else (make-posn (posn-x b) (+ 1 (posn-y b)))])]))

(check-expect (move-bullet 'none) 'none)
(check-expect (move-bullet (make-posn 0 0)) (make-posn 0 1))
(check-expect (move-bullet (make-posn 0 1)) (make-posn 0 2))
(check-expect (move-bullet (make-posn 0 (+ TOP 1))) 'none)

;; ------------------------------     Key Functions     ------------------------
;; move-shooter : Shooter Key -> Shooter
;; This function takes a shooter and a key and returns shooter in a new position
(define (move-shooter s key)
  (cond
    [(key=? "left" key) (cond
                          [(< (- (posn-x s) 1) LEFT-BOUND) s]
                          [else (make-posn (- (posn-x s) 1) (posn-y s))])]
    [(key=? "right" key) (cond
                          [(> (+ (posn-x s) 1) RIGHT-BOUND) s]
                          [else (make-posn (+ (posn-x s) 1) (posn-y s))])]))
(define SHOOTER11 (make-posn 11 0))
(define SHOOTER12 (make-posn 12 0))
(define SHOOTER13 (make-posn 13 0))
(define SHOOTER-LEFT (make-posn 0 0))
(define SHOOTER-RIGHT (make-posn 39 0))

(check-expect (move-shooter SHOOTER12 "left") SHOOTER11)
(check-expect (move-shooter SHOOTER12 "right") SHOOTER13)
(check-expect (move-shooter SHOOTER-RIGHT "right") SHOOTER-RIGHT)
(check-expect (move-shooter SHOOTER-LEFT "left") SHOOTER-LEFT)

;; make-world-shooter : Shooter -> World
;; This function makes an empty world except for the given shooter
(define (make-world-shooter shooter)
  (make-world empty empty 'none shooter  3))
(check-expect
 (make-world-shooter SHOOTER11) (make-world empty empty 'none SHOOTER11 3))
(check-expect
 (make-world-shooter SHOOTER12) (make-world empty empty 'none SHOOTER12 3))

;; shoot-or-move : World Key -> World
;; This function takes a world and a key and returns a new world with
;; a moved shooter and/or created bullet
(define (shoot-or-move world key)
  (cond
    [(or (key=? key "left")
         (key=? key "right"))
     (make-world
      (world-LoM world)
      (world-LoC world)
      (world-bullet world)
      (move-shooter (world-shooter world) key)
      (world-move world))]
    [(and (key=? key " ")
          (symbol? (world-bullet world)))
     (make-world
      (world-LoM world)
      (world-LoC world)
      (world-shooter world) ;; Bullet is Shooter (Posn of player)
      (world-shooter world)
      (world-move world))]
    [else world]))

(check-expect
 (shoot-or-move
  (make-world-shooter SHOOTER12) "right") (make-world-shooter SHOOTER13))
(check-expect
 (shoot-or-move
  (make-world-shooter SHOOTER12) "left") (make-world-shooter SHOOTER11))
(check-expect
 (shoot-or-move
  (make-world-shooter SHOOTER-LEFT) "left") (make-world-shooter SHOOTER-LEFT))
(check-expect
 (shoot-or-move
  (make-world-shooter SHOOTER-RIGHT) "right")
 (make-world-shooter SHOOTER-RIGHT))
(check-expect
 (shoot-or-move
  (make-world-shooter SHOOTER11) " ")
 (make-world empty empty SHOOTER11 SHOOTER11 3))
(check-expect
 (shoot-or-move (make-world empty empty SHOOTER11 SHOOTER11 3) " ")
 (make-world empty empty SHOOTER11 SHOOTER11 3))



;; ------------------------------     Draw Functions     -----------------------

;; place-image-grid : Image Number Number Image -> Image
;; Just like place-image, except position is in grid coordinates
;; instead of pixels
(define (place-image-grid img x y bkg)
  (place-image img 
               (* (+ x 0.5) CELL-SIZE)
               (- (* GRID-HEIGHT CELL-SIZE) (* (+ y 0.5) CELL-SIZE))
               bkg))

;; world->image : World -> Image
;; takes a world and produces an image from it
(define (world->image w)
  (draw-mushrooms (world-LoM w)
  (draw-centipedes (world-LoC w)
  (draw-shooter (world-shooter w)
  (draw-bullet (world-bullet w) BG)))))

;; draw-centipedes : [Listof Centipede] Image -> Image
;; draws all centipedes on grid
(define (draw-centipedes loc img)
  (cond
    [(empty? loc) img]
    [else (draw-centipede (first loc) (draw-centipedes (rest loc) img))]))

;; draw-centipede : Centipede Image -> Image
;; draws centipede on grid
(define (draw-centipede c img)
  (draw-centipede-head c (draw-centipede-body (rest (centipede-body c)) img)))

;; draw-centipede-head : Centipede Image -> Image
;; draws centipede head on grid
(define (draw-centipede-head c img)
  (place-image-grid
           (get-centipede-head (centipede-h-direction c))
           (posn-x (first (centipede-body c)))
           (posn-y (first (centipede-body c)))
           img))

;; get-centipede-head : HorizontalDirection -> Image
;; gets respective centipede head Image from Direction
(define (get-centipede-head dir)
  (cond
    [(symbol=? 'right dir) RIGHT-HEAD]
    [(symbol=? 'left dir) LEFT-HEAD]))

;; draw-centipede-body : CentipedeBody Image -> Image
;; draws centipede body on grid
(define (draw-centipede-body cb img)
  (cond
    [(empty? cb) img]
    [(cons? cb)
     (place-image-grid
           CENTIPEDE-CELL
           (posn-x (first cb))
           (posn-y (first cb))
           (draw-centipede-body (rest cb) img))]))

;; draw-mushrooms : [Listof Mushroom] Image -> Image
;; takes a list of mushroom and an image and puts the mushrooms on the image
(define (draw-mushrooms lom img)
  (cond
    [(empty? lom) img]
    [else (draw-mushroom
            (first lom)
            (draw-mushrooms (rest lom) img))]))

;; draw-mushroom : Mushroom Image -> Image
;; takes a mushroom and an image and places
;; the mushroom on the image
(define (draw-mushroom m img)
  (place-image-grid
           (mushroom-image m)
           (posn-x (mushroom-posn m))
           (posn-y (mushroom-posn m))
           img))

;; mushroom-image : Mushroom -> Image
;; takes a Mushroom and returns the Mushroom image based
;; on the given mushroom's state
(define (mushroom-image m)
  (cond
    [(= (mushroom-state m) 1) MUSHROOM-1-C]
    [(= (mushroom-state m) 2) MUSHROOM-2-C]
    [(= (mushroom-state m) 3) MUSHROOM-3-C]
    [(= (mushroom-state m) 4) MUSHROOM-4-C]))
(check-expect (mushroom-image MUSHROOM-EX0d) MUSHROOM-1-C)
(check-expect (mushroom-image MUSHROOM-EX0c) MUSHROOM-2-C)
(check-expect (mushroom-image MUSHROOM-EX0b) MUSHROOM-3-C)
(check-expect (mushroom-image MUSHROOM-EX0) MUSHROOM-4-C)

;; draw-bullet : Bullet Image -> Image
;; takes a bullet and an image returns the bullet placed
;; on the image.
(define (draw-bullet b img)
  (cond
    [(symbol? b) img]
    [else (place-image-grid BULLET (posn-x b) (posn-y b) img)]))

;; draw-shooter : Shooter Image -> Image
;; take a shooter and an image and puts the shooter on the image.
(define (draw-shooter s img)
  (place-image-grid PLAYER (posn-x s) (posn-y s) img))

;; ------------------------------     Game End     -----------------------------
;; get-centipede-body : Centipede -> CentipedeBody
;; This function takes in a centipede and returns its body.
(define (get-centipede-body centipede)
  (cond
    [(centipede? centipede) (centipede-body centipede)]
    [else empty]))
(check-expect (get-centipede-body empty) empty)
(check-expect
 (get-centipede-body
  (make-initial-centipede
   (make-initial-centipede-body 10 0 39)))
 (make-initial-centipede-body 10 0 39))

;; shooter-centipede-collision? : Shooter Centipede -> Boolean
;; This function takes a shooter and a centipede and returns true
;; if there is a segment in the same position as the shooter
(define (shooter-centipede-collision? shooter centipede)
  (cond
    [(empty? (get-centipede-body centipede)) false]
    [(cons? (get-centipede-body centipede))
         (or (posn=?
              shooter (first (get-centipede-body centipede)))
             (shooter-centipede-collision?
              shooter (make-centipede (centipede-v-direction centipede)
                                      (centipede-h-direction centipede)
                                      (rest
                                       (centipede-body centipede)))))]))

(check-expect (shooter-centipede-collision? SHOOTER12 empty) false)
(check-expect (shooter-centipede-collision?
               SHOOTER12 (make-initial-centipede
                          (make-initial-centipede-body 10 1 0))) false)
(check-expect (shooter-centipede-collision?
               SHOOTER12 (make-initial-centipede
                          (make-initial-centipede-body 10 8 0))) true)



;; shooter-centipede-list-collision? : Shooter [Listof Centipede] -> Boolean
;; takes a shooter and a list of centipedes and returns true if
;; there is a centipede from the list that is in the same spot as the shooter
(define (shooter-centipede-list-collision? shooter LoC)
  (cond
    [(empty? LoC) false]
    [(cons? LoC)
         (or
          (shooter-centipede-collision? shooter (first LoC))
          (shooter-centipede-list-collision? shooter (rest LoC)))]))
(check-expect (shooter-centipede-list-collision? SHOOTER12 empty) false)
(check-expect
 (shooter-centipede-list-collision?
  SHOOTER12 (cons
             (make-initial-centipede (make-initial-centipede-body 10 1 0))
             empty)) false)
(check-expect
 (shooter-centipede-list-collision?
  SHOOTER12 (cons
             (make-initial-centipede (make-initial-centipede-body 10 1 0))
             (cons (make-initial-centipede (make-initial-centipede-body 5 8 0))
                   empty))) true)
;; world-end? : World -> Boolean
;; This function takes in a world and returns true if certain conditions are met
(define (world-end? world)
  (or (shooter-centipede-list-collision?
       (world-shooter world) (world-LoC world))
      (empty? (world-LoC world))))

;; game-end :  World -> Image
(define (game-end world)
  (cond
    [(empty? (world-LoC world)) WINNER]
    [(shooter-centipede-list-collision?
      (world-shooter world) (world-LoC world)) LOSER]))

;; ------------------------------     Game Launch     --------------------------

;; TEST STUFF
(define SHOOTER (make-posn 12 0))
(define INITIAL-MUSHROOM-FIELD (generate-initial-field empty 40 empty empty
     (rand-mushroom
                     (random (+ 1 (- GRID-WIDTH 1)))
                     (+ (random (- GRID-HEIGHT 2)) 1) empty empty)))

(define CENT (make-initial-centipede INITIAL-15-CB-LIST))
(define centipede-list (cons CENT empty))
(define WORLD0
  (make-world INITIAL-MUSHROOM-FIELD centipede-list 'none SHOOTER 0))

(big-bang WORLD0
          [on-tick update-all .005]
          [on-key shoot-or-move]
          [to-draw world->image]
          [stop-when world-end? game-end])
