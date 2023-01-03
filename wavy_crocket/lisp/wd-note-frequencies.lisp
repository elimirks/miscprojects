(require "lisp/common")

;; Used to extarpolate computation of other notes
(setg 'wd-base-frequencies
      '(16.35 ; c0
        32.70 ; c1
        65.41 ; ...
        130.81
        261.63
        523.25
        1046.50
        2093.00
        4186.01)) ; c8

;; https://pages.mtu.edu/~suits/NoteFreqCalcs.html
(defun wd-extrapolate-note (base n) 
  (set 'a (pow 2.0 (/ 1.0 12.0)))
  (* base (pow a n)))

;; Oh, woe is me.. if only we had macro support
;; ... at least I have vim macros :}

(setg 'c0 (nth 0 wd-base-frequencies))
(setg 'cs0 (wd-extrapolate-note c0 1))
(setg 'd0 (wd-extrapolate-note c0 2))
(setg 'ds0 (wd-extrapolate-note c0 3))
(setg 'e0 (wd-extrapolate-note c0 4))
(setg 'f0 (wd-extrapolate-note c0 5))
(setg 'fs0 (wd-extrapolate-note c0 6))
(setg 'g0 (wd-extrapolate-note c0 7))
(setg 'gs0 (wd-extrapolate-note c0 8))
(setg 'a0 (wd-extrapolate-note c0 9))
(setg 'as0 (wd-extrapolate-note c0 10))
(setg 'b0 (wd-extrapolate-note c0 11))

(setg 'c1 (nth 1 wd-base-frequencies))
(setg 'cs1 (wd-extrapolate-note c1 1))
(setg 'd1 (wd-extrapolate-note c1 2))
(setg 'ds1 (wd-extrapolate-note c1 3))
(setg 'e1 (wd-extrapolate-note c1 4))
(setg 'f1 (wd-extrapolate-note c1 5))
(setg 'fs1 (wd-extrapolate-note c1 6))
(setg 'g1 (wd-extrapolate-note c1 7))
(setg 'gs1 (wd-extrapolate-note c1 8))
(setg 'a1 (wd-extrapolate-note c1 9))
(setg 'as1 (wd-extrapolate-note c1 10))
(setg 'b1 (wd-extrapolate-note c1 11))

(setg 'c2 (nth 2 wd-base-frequencies))
(setg 'cs2 (wd-extrapolate-note c2 1))
(setg 'd2 (wd-extrapolate-note c2 2))
(setg 'ds2 (wd-extrapolate-note c2 3))
(setg 'e2 (wd-extrapolate-note c2 4))
(setg 'f2 (wd-extrapolate-note c2 5))
(setg 'fs2 (wd-extrapolate-note c2 6))
(setg 'g2 (wd-extrapolate-note c2 7))
(setg 'gs2 (wd-extrapolate-note c2 8))
(setg 'a2 (wd-extrapolate-note c2 9))
(setg 'as2 (wd-extrapolate-note c2 10))
(setg 'b2 (wd-extrapolate-note c2 11))

(setg 'c3 (nth 3 wd-base-frequencies))
(setg 'cs3 (wd-extrapolate-note c3 1))
(setg 'd3 (wd-extrapolate-note c3 2))
(setg 'ds3 (wd-extrapolate-note c3 3))
(setg 'e3 (wd-extrapolate-note c3 4))
(setg 'f3 (wd-extrapolate-note c3 5))
(setg 'fs3 (wd-extrapolate-note c3 6))
(setg 'g3 (wd-extrapolate-note c3 7))
(setg 'gs3 (wd-extrapolate-note c3 8))
(setg 'a3 (wd-extrapolate-note c3 9))
(setg 'as3 (wd-extrapolate-note c3 10))
(setg 'b3 (wd-extrapolate-note c3 11))

(setg 'c4 (nth 4 wd-base-frequencies))
(setg 'cs4 (wd-extrapolate-note c4 1))
(setg 'd4 (wd-extrapolate-note c4 2))
(setg 'ds4 (wd-extrapolate-note c4 3))
(setg 'e4 (wd-extrapolate-note c4 4))
(setg 'f4 (wd-extrapolate-note c4 5))
(setg 'fs4 (wd-extrapolate-note c4 6))
(setg 'g4 (wd-extrapolate-note c4 7))
(setg 'gs4 (wd-extrapolate-note c4 8))
(setg 'a4 (wd-extrapolate-note c4 9))
(setg 'as4 (wd-extrapolate-note c4 10))
(setg 'b4 (wd-extrapolate-note c4 11))

(setg 'c5 (nth 5 wd-base-frequencies))
(setg 'cs5 (wd-extrapolate-note c5 1))
(setg 'd5 (wd-extrapolate-note c5 2))
(setg 'ds5 (wd-extrapolate-note c5 3))
(setg 'e5 (wd-extrapolate-note c5 4))
(setg 'f5 (wd-extrapolate-note c5 5))
(setg 'fs5 (wd-extrapolate-note c5 6))
(setg 'g5 (wd-extrapolate-note c5 7))
(setg 'gs5 (wd-extrapolate-note c5 8))
(setg 'a5 (wd-extrapolate-note c5 9))
(setg 'as5 (wd-extrapolate-note c5 10))
(setg 'b5 (wd-extrapolate-note c5 11))

(setg 'c6 (nth 6 wd-base-frequencies))
(setg 'cs6 (wd-extrapolate-note c6 1))
(setg 'd6 (wd-extrapolate-note c6 2))
(setg 'ds6 (wd-extrapolate-note c6 3))
(setg 'e6 (wd-extrapolate-note c6 4))
(setg 'f6 (wd-extrapolate-note c6 5))
(setg 'fs6 (wd-extrapolate-note c6 6))
(setg 'g6 (wd-extrapolate-note c6 7))
(setg 'gs6 (wd-extrapolate-note c6 8))
(setg 'a6 (wd-extrapolate-note c6 9))
(setg 'as6 (wd-extrapolate-note c6 10))
(setg 'b6 (wd-extrapolate-note c6 11))

(setg 'c7 (nth 7 wd-base-frequencies))
(setg 'cs7 (wd-extrapolate-note c7 1))
(setg 'd7 (wd-extrapolate-note c7 2))
(setg 'ds7 (wd-extrapolate-note c7 3))
(setg 'e7 (wd-extrapolate-note c7 4))
(setg 'f7 (wd-extrapolate-note c7 5))
(setg 'fs7 (wd-extrapolate-note c7 6))
(setg 'g7 (wd-extrapolate-note c7 7))
(setg 'gs7 (wd-extrapolate-note c7 8))
(setg 'a7 (wd-extrapolate-note c7 9))
(setg 'as7 (wd-extrapolate-note c7 10))
(setg 'b7 (wd-extrapolate-note c7 11))

(setg 'c8 (nth 8 wd-base-frequencies))
(setg 'cs8 (wd-extrapolate-note c8 1))
(setg 'd8 (wd-extrapolate-note c8 2))
(setg 'ds8 (wd-extrapolate-note c8 3))
(setg 'e8 (wd-extrapolate-note c8 4))
(setg 'f8 (wd-extrapolate-note c8 5))
(setg 'fs8 (wd-extrapolate-note c8 6))
(setg 'g8 (wd-extrapolate-note c8 7))
(setg 'gs8 (wd-extrapolate-note c8 8))
(setg 'a8 (wd-extrapolate-note c8 9))
(setg 'as8 (wd-extrapolate-note c8 10))
(setg 'b8 (wd-extrapolate-note c8 11))