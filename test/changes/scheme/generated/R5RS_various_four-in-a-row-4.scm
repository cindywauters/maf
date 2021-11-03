; Changes:
; * removed: 1
; * added: 5
; * swaps: 1
; * negated predicates: 1
(letrec ((down-key 0)
         (left-key 1)
         (right-key 2)
         (pi 3.141500e+00)
         (MAX_LOOP_ITERATIONS 10)
         (global-min #f)
         (global-max #f)
         (global-draw-adt #f)
         (global-current 0)
         (remq (lambda (x l)
                 (if (pair? l)
                    (if (eq? x (car l))
                       (remq x (cdr l))
                       (cons x (remq x (cdr l))))
                    l)))
         (enter-keyboard-input (lambda ()
                                 (random 3)))
         (make-window (lambda (w h title)
                        (let* ((show-fps #t)
                               (fps 0)
                               (delta-time 0)
                               (previous-time 0)
                               (keyboard-callback (lambda (ev)
                                                    0))
                               (update-callback (lambda ()
                                                  0))
                               (layers ())
                               (closed #f))
                           (letrec ((paint-callback (lambda ()
                                                      (update-callback)
                                                      (set! fps (calculate-fps delta-time))))
                                    (calculate-fps (lambda (delta-time)
                                                     (if (not (= delta-time 0))
                                                        (/ 1000 delta-time)
                                                        fps)))
                                    (add-layer (lambda ()
                                                 (letrec ((layer (make-layer w h)))
                                                    (set! layers (append layers (cons layer ())))
                                                    layer)))
                                    (set-background! (lambda (colorstring)
                                                       0))
                                    (game-loop (lambda ()
                                                 (keyboard-callback (enter-keyboard-input))
                                                 (paint-callback)))
                                    (window-dispatch (lambda (msg)
                                                       (if (eq? msg 'make-layer)
                                                          (add-layer)
                                                          (if (eq? msg 'set-background!)
                                                             (lambda (mw-sb)
                                                                (set-background! mw-sb))
                                                             (if (eq? msg 'set-key-callback!)
                                                                (lambda (eh)
                                                                   (set! keyboard-callback eh))
                                                                (if (eq? msg 'set-update-callback!)
                                                                   (lambda (gl)
                                                                      (set! update-callback gl))
                                                                   (if (eq? msg 'game-loop)
                                                                      (lambda ()
                                                                         (game-loop))
                                                                      (error msg)))))))))
                              window-dispatch))))
         (get-bitmap (lambda (file)
                       0))
         (get-bitmap-section (lambda (tilebitmap x y width height)
                               0))
         (generate-mask (lambda (bitmappath background-color)
                          0))
         (make-bitmap-tile (lambda (bitmappath mask)
                             (let ((x 0)
                                   (y 0))
                                (letrec ((bitmap-tile-dispatch (lambda (msg)
                                                                 (if (eq? msg 'set-x!)
                                                                    (lambda (new-x)
                                                                       (set! x new-x))
                                                                    (if (eq? msg 'set-y!)
                                                                       (lambda (new-y)
                                                                          (set! y new-y))
                                                                       (if (eq? msg 'get-x)
                                                                          (lambda ()
                                                                             (<change>
                                                                                ()
                                                                                (display x))
                                                                             x)
                                                                          (if (eq? msg 'get-y) (lambda () y) (error msg))))))))
                                   bitmap-tile-dispatch))))
         (make-tile (lambda (w h bitmap mask)
                      (if (string? bitmap)
                         (set! bitmap (get-bitmap bitmap))
                         #f)
                      (if (string? mask)
                         (set! mask (get-bitmap mask))
                         #f)
                      (let* ((x 0)
                             (y 0)
                             (update-callback (lambda ()
                                                #t))
                             (rotation 0))
                         (letrec ((clear (lambda ()
                                           (update-callback)))
                                  (draw-rectangle (lambda (x y w h color)
                                                    (if (string? color) (set! color "") #f)
                                                    (update-callback)))
                                  (draw-ellipse (lambda (x y w h color)
                                                  (if (string? color) "" #f)
                                                  (update-callback)))
                                  (draw-line (lambda (x y w h color)
                                               (if (string? color) "" #f)
                                               (update-callback)))
                                  (draw-text (lambda (text fontsize x y color)
                                               (if (string? color) (set! color "") #f)
                                               (update-callback)))
                                  (rotate-clockwise (lambda ()
                                                      (set! rotation (modulo (+ rotation 90) 360))
                                                      (rotate rotation)))
                                  (rotate-counterclockwise (lambda ()
                                                             (set! rotation (modulo (- rotation 90) 360))
                                                             (if (< rotation 0) (set! rotation 270) #f)
                                                             (rotate rotation)))
                                  (rotate (lambda (tempr)
                                            (set! rotation tempr)
                                            (if (not (let ((__or_res (eq? 90 tempr))) (if __or_res __or_res (let ((__or_res (eq? 0 tempr))) (if __or_res __or_res (let ((__or_res (eq? 180 tempr))) (if __or_res __or_res (eq? 270 tempr))))))))
                                               (begin
                                                  (display "ERROR ::: illegal rotation given, only 0,90,180,270 is allowed: ")
                                                  (newline))
                                               #f)
                                            (update-callback)))
                                  (set-x! (lambda (new_x)
                                            (set! x new_x)
                                            (update-callback)))
                                  (set-y! (lambda (new_y)
                                            (set! y new_y)
                                            (update-callback)))
                                  (draw (lambda (dc)
                                          0))
                                  (set-on-update! (lambda (new_callback)
                                                    (set! update-callback new_callback)))
                                  (tile-dispatch (lambda (msg)
                                                   (if (eq? msg 'draw)
                                                      (lambda (x-graphics-1)
                                                         (draw x-graphics-1))
                                                      (if (eq? msg 'set-on-update!)
                                                         (lambda (x-graphics-2)
                                                            (set-on-update! x-graphics-2))
                                                         (if (eq? msg 'set-x!)
                                                            (lambda (x-graphics-3)
                                                               (set-x! x-graphics-3))
                                                            (if (eq? msg 'set-y!)
                                                               (lambda (x-graphics-4)
                                                                  (set-y! x-graphics-4))
                                                               (if (eq? msg 'get-x)
                                                                  (lambda ()
                                                                     x)
                                                                  (if (eq? msg 'get-y)
                                                                     (lambda ()
                                                                        y)
                                                                     (if (eq? msg 'get-w)
                                                                        (lambda ()
                                                                           w)
                                                                        (if (eq? msg 'get-h)
                                                                           (lambda ()
                                                                              h)
                                                                           (if (eq? msg 'rotate-clockwise)
                                                                              (lambda ()
                                                                                 (rotate-clockwise))
                                                                              (if (eq? msg 'rotate-counterclockwise)
                                                                                 (lambda ()
                                                                                    (rotate-counterclockwise))
                                                                                 (if (eq? msg 'clear)
                                                                                    (lambda ()
                                                                                       (clear))
                                                                                    (if (eq? msg 'draw-rectangle)
                                                                                       (lambda (x-5 x-6 x-7 x-8 x-9)
                                                                                          (draw-rectangle x-5 x-6 x-7 x-8 x-9))
                                                                                       (if (eq? msg 'draw-line)
                                                                                          (lambda (x-10 x-11 x-12 x-13 x-14)
                                                                                             (draw-line x-10 x-11 x-12 x-13 x-14))
                                                                                          (if (eq? msg 'draw-ellipse)
                                                                                             (lambda (x-15 x-16 x-17 x-18 x-19)
                                                                                                (draw-ellipse x-15 x-16 x-17 x-18 x-19))
                                                                                             (if (eq? msg 'draw-text)
                                                                                                (lambda (x-20 x-21 x-22 x-23 x-24)
                                                                                                   (draw-text x-20 x-21 x-22 x-23 x-24))
                                                                                                (error msg)))))))))))))))))))
                            tile-dispatch))))
         (make-tile-sequence (lambda (tiles)
                               (<change>
                                  ()
                                  width)
                               (let ((index 0)
                                     (update-callback (lambda ()
                                                        #t)))
                                  (letrec ((set-x! (lambda (new_x)
                                                     (for-each (lambda (tile) ((tile 'set-x!) new_x)) tiles)
                                                     (update-callback)))
                                           (set-y! (lambda (new_y)
                                                     (for-each (lambda (tile) ((tile 'set-y!) new_y)) tiles)
                                                     (update-callback)))
                                           (set-current! (lambda (new_index)
                                                           (if (let ((__or_res (>= new_index (length tiles)))) (if __or_res __or_res (< new_index 0)))
                                                              (begin
                                                                 (display "ERROR ::: illegal index given for tile-sequence: ")
                                                                 (display new_index)
                                                                 (newline))
                                                              (begin
                                                                 (set! index new_index)
                                                                 (update-callback)))))
                                           (set-previous! (lambda ()
                                                            (set! index (modulo (- index 1) (length tiles)))
                                                            (if (< index 0)
                                                               (set! index (- (length tiles) 1))
                                                               #f)
                                                            (update-callback)))
                                           (set-next! (lambda ()
                                                        (set! index (modulo (+ 1 index) (length tiles)))
                                                        (update-callback)))
                                           (rotate-clockwise (lambda ()
                                                               (for-each (lambda (tile) (tile 'rotate-clockwise)) tiles)
                                                               (update-callback)))
                                           (rotate-counterclockwise (lambda ()
                                                                      (for-each (lambda (tile) (tile 'rotate-counterclockwise)) tiles)
                                                                      (update-callback)))
                                           (draw-rectangle (lambda (x y w h color)
                                                             (for-each (lambda (tile) ((tile 'draw-rectangle) x y w h color)) tiles)
                                                             (update-callback)))
                                           (draw-ellipse (lambda (x y w h color)
                                                           (for-each (lambda (tile) ((tile 'draw-ellipse) x y w h color)) tiles)
                                                           (update-callback)))
                                           (draw-text (lambda (text fontsize x y color)
                                                        (for-each (lambda (tile) ((tile 'draw-text) text fontsize x y color)) tiles)
                                                        (update-callback)))
                                           (draw-line (lambda (x y w h width color)
                                                        (for-each (lambda (tile) ((tile 'draw-line) x y w h width color)) tiles)
                                                        (update-callback)))
                                           (clear (lambda ()
                                                    (for-each (lambda (tile) (tile 'clear)) tiles)
                                                    (update-callback)))
                                           (draw (lambda (dc)
                                                   (((current) 'draw) dc)))
                                           (set-on-update! (lambda (new_callback)
                                                             (set! update-callback new_callback)))
                                           (current (lambda ()
                                                      (list-ref tiles index)))
                                           (tile-sequence-dispatch (lambda (msg)
                                                                     (if (eq? msg 'draw)
                                                                        draw
                                                                        (if (eq? msg 'set-on-update!)
                                                                           set-on-update!
                                                                           (if (eq? msg 'set-x!)
                                                                              set-x!
                                                                              (if (eq? msg 'set-y!)
                                                                                 set-y!
                                                                                 (if (eq? msg 'get-x)
                                                                                    ((current) 'get-x)
                                                                                    (if (eq? msg 'get-y)
                                                                                       ((current) 'get-y)
                                                                                       (if (eq? msg 'get-w)
                                                                                          ((current) 'get-w)
                                                                                          (if (eq? msg 'get-h)
                                                                                             ((current) 'get-h)
                                                                                             (if (eq? msg 'set-current!)
                                                                                                set-current!
                                                                                                (if (eq? msg 'set-next!)
                                                                                                   (set-next!)
                                                                                                   (if (eq? msg 'set-previous!)
                                                                                                      (set-previous!)
                                                                                                      (if (eq? msg 'rotate-clockwise)
                                                                                                         (rotate-clockwise)
                                                                                                         (if (eq? msg 'rotate-counterclockwise)
                                                                                                            (rotate-counterclockwise)
                                                                                                            (if (eq? msg 'clear)
                                                                                                               (clear)
                                                                                                               (if (eq? msg 'draw-rectangle)
                                                                                                                  draw-rectangle
                                                                                                                  (if (eq? msg 'draw-ellipse)
                                                                                                                     draw-ellipse
                                                                                                                     (if (eq? msg 'draw-line)
                                                                                                                        draw-line
                                                                                                                        (if (eq? msg 'draw-text) draw-text (error msg))))))))))))))))))))))
                                     tile-sequence-dispatch))))
         (make-layer (lambda (w h)
                       (let* ((drawables ())
                              (needs-update #t))
                          (letrec ((redraw (lambda ()
                                             0))
                                   (draw (lambda (dc)
                                           (if (<change> needs-update (not needs-update))
                                              (begin
                                                 (redraw)
                                                 (set! needs-update #f))
                                              #f)))
                                   (add-drawable (lambda (drawable)
                                                   (<change>
                                                      (set! drawables (cons drawable drawables))
                                                      ())
                                                   (redraw)))
                                   (remove-drawable (lambda (drawable)
                                                      ((drawable 'set-on-update!) (lambda () #t))
                                                      (set! drawables (remq drawable drawables))
                                                      (<change>
                                                         ()
                                                         drawable)
                                                      (<change>
                                                         ()
                                                         remq)
                                                      (redraw)))
                                   (layer-dispatch (lambda (msg)
                                                     (if (eq? msg 'add-drawable)
                                                        add-drawable
                                                        (if (eq? msg 'remove-drawable)
                                                           remove-drawable
                                                           (if (eq? msg 'draw) draw (error msg)))))))
                             layer-dispatch))))
         (add1 (lambda (add1-x)
                 (+ add1-x 1)))
         (sub1 (lambda (sub1-x)
                 (- sub1-x 1)))
         (fill-vector! (lambda (lambda-fun vector)
                         (letrec ((fill-vector-iter! (lambda (i vector)
                                                       (if (= i (vector-length vector))
                                                          vector
                                                          (begin
                                                             (vector-set! vector i (lambda-fun i))
                                                             (fill-vector-iter! (+ i 1) vector))))))
                            (fill-vector-iter! 0 vector))))
         (make-matrix (lambda (x y l)
                        (let ((matrix-data (make-vector x (make-vector y #f))))
                           (letrec ((set-matrix! (lambda (x y value)
                                                   (vector-set! (vector-ref matrix-data x) y value)))
                                    (get-matrix (lambda (x y)
                                                  (vector-ref (vector-ref matrix-data x) y)))
                                    (matrix-dispatch (lambda (msg)
                                                       (if (eq? msg 'set!)
                                                          set-matrix!
                                                          (if (eq? msg 'get)
                                                             get-matrix
                                                             (if (eq? msg 'show) (display matrix-data) #f))))))
                              (fill-vector! (lambda (i) (make-vector y l)) matrix-data)
                              matrix-dispatch))))
         (arrow (lambda (min max draw-adt)
                  (set! global-min min)
                  (<change>
                     (set! global-max max)
                     (set! global-draw-adt draw-adt))
                  (<change>
                     (set! global-draw-adt draw-adt)
                     (set! global-max max))
                  (set! global-current min)))
         (arrow-draw (lambda (arrow-draw-arrow)
                       ((global-draw-adt 'draw-pointer) #f)))
         (arrow-min (lambda (arrow-min-arrow)
                      global-min))
         (arrow-max (lambda (arrow-max-arrow)
                      global-max))
         (arrow-current (lambda (arrow-current-arrow)
                          global-current))
         (arrow-left (lambda (arrow-left-arrow)
                       (if (> (arrow-current arrow-left-arrow) (arrow-min arrow-left-arrow))
                          (set! global-current (sub1 (arrow-current arrow-left-arrow)))
                          #f)))
         (arrow-right (lambda (arrow-right-arrow)
                        (if (< (arrow-current arrow-right-arrow) (arrow-max arrow-right-arrow))
                           (set! global-current (add1 (arrow-current arrow-right-arrow)))
                           (display "current coordinate above max"))))
         (make-coin (lambda (x y bitmap draw-adt)
                      (letrec ((draw (lambda ()
                                       ((draw-adt 'draw-coin) coin-dispatch)))
                               (coin-dispatch (lambda (msg)
                                                (if (eq? msg 'bitmap)
                                                   bitmap
                                                   (if (eq? msg 'draw)
                                                      (draw)
                                                      (if (eq? msg 'get-x)
                                                         x
                                                         (if (eq? msg 'get-y) y #f)))))))
                         coin-dispatch)))
         (make-graphics (lambda (window width height rows cols)
                          (let ((background-layer (window 'make-layer))
                                (layer (window 'make-layer))
                                (coin-tile-list ())
                                (pointer-tile (make-bitmap-tile "arrow.png" #f)))
                             (letrec ((c2p-w (lambda (x)
                                               (* x (/ width cols))))
                                      (draw-pointer (lambda (pointer)
                                                      ((pointer-tile 'set-x!) (c2p-w (arrow-current pointer)))))
                                      (update-coin! (lambda (coin tile)
                                                      ((tile 'set-x!) (c2p-w (coin 'get-x)))
                                                      ((tile 'set-y!) (c2p-w (add1 (coin 'get-y))))))
                                      (new-coin-tile! (lambda (coin)
                                                        (let ((coin-tile (make-bitmap-tile (coin 'bitmap) #f)))
                                                           (set! coin-tile-list (cons (list coin coin-tile) coin-tile-list))
                                                           (update-coin! coin coin-tile)
                                                           ((layer 'add-drawable) coin-tile))))
                                      (draw-coin (lambda (coin)
                                                   (let ((asc (assq coin coin-tile-list)))
                                                      (if asc
                                                         (update-coin! coin (cadr asc))
                                                         (new-coin-tile! coin)))))
                                      (graphics-dispatch (lambda (msg)
                                                           (if (eq? msg 'draw-pointer)
                                                              draw-pointer
                                                              (if (eq? msg 'draw-coin) draw-coin #f))))
                                      (make-background-row (lambda (x y)
                                                             (if (< y (add1 rows))
                                                                (let ((empty-tile (make-bitmap-tile "empty.png" #f)))
                                                                   ((background-layer 'add-drawable) empty-tile)
                                                                   ((empty-tile 'set-x!) (c2p-w x))
                                                                   ((empty-tile 'set-y!) (c2p-w y))
                                                                   (make-background-row x (add1 y)))
                                                                #f)))
                                      (make-background (lambda (x y)
                                                         (if (< x cols)
                                                            (begin
                                                               (make-background-row x 1)
                                                               (make-background (add1 x) y))
                                                            #f))))
                                ((layer 'add-drawable) pointer-tile)
                                (make-background 0 0)
                                graphics-dispatch))))
         (four-in-a-row (lambda (width height rows cols title)
                          (let* ((window (make-window width height title))
                                 (game-board (make-matrix rows cols 0))
                                 (current-coin "blue-tile.png")
                                 (draw (make-graphics window width height rows cols))
                                 (pointer (arrow 0 (sub1 cols) draw)))
                             (letrec ((switch (lambda ()
                                                (if (eq? current-coin "blue-tile.png")
                                                   (set! current-coin "red-tile.png")
                                                   (set! current-coin "blue-tile.png"))))
                                      (find-first (lambda (col)
                                                    (letrec ((find-first-iter (lambda (col r)
                                                                                (let ((tile ((game-board 'get) r col)))
                                                                                   (if (not (eq? tile 0))
                                                                                      (sub1 r)
                                                                                      (if (= r (sub1 rows))
                                                                                         r
                                                                                         (find-first-iter col (add1 r))))))))
                                                       (find-first-iter col 0))))
                                      (drop-coin (lambda ()
                                                   (if (if (not (eq? ((game-board 'get) 0 3) 0)) (not (eq? ((game-board 'get) 1 3) 0)) #f)
                                                      (error "arbitrary error")
                                                      #f)
                                                   (let ((row (find-first (arrow-current pointer))))
                                                      (if (>= row 0)
                                                         (let ((new-coin (make-coin (arrow-current pointer) row current-coin draw)))
                                                            (switch)
                                                            (new-coin 'draw)
                                                            ((game-board 'set!) row (arrow-current pointer) new-coin))
                                                         #f))))
                                      (on-key (lambda (rt)
                                                (if (= rt down-key)
                                                   (drop-coin)
                                                   (if (= rt left-key)
                                                      (arrow-left pointer)
                                                      (if (= rt right-key)
                                                         (arrow-right pointer)
                                                         (error rt))))))
                                      (start (lambda ()
                                               (letrec ((main-loop (lambda (main-loop-n)
                                                                     (if (> main-loop-n 0)
                                                                        (begin
                                                                           ((window 'game-loop))
                                                                           (main-loop (- main-loop-n 1)))
                                                                        #f))))
                                                  ((window 'set-key-callback!) on-key)
                                                  ((window 'set-background!) "white")
                                                  (main-loop MAX_LOOP_ITERATIONS))))
                                      (dispatch (lambda (msg)
                                                  (if (eq? msg 'start!) (start) #f))))
                                dispatch))))
         (game (four-in-a-row 700 700 6 7 "four in a row")))
   (<change>
      ()
      (game 'start!))
   (game 'start!))