(require "lisp/wd-note-frequencies")
;; File required to generate music / wavedata

;; Default sample rate is CD quality
;; NOTE: It _must_ be set before you start manipulating any wave data!
;; Otherwise.. strange things will happen
(setg 'wd-sample-rate 44100)

(defun wd-set-bpm (bpm)
  (set 'full-samples (* (/ 60.0 bpm) wd-sample-rate))
  (setg 'wd-full-note-duration (to-int full-samples))
  (setg 'wd-half-note-duration (to-int (/ full-samples 2.0)))
  (setg 'wd-quarter-note-duration (to-int (/ full-samples 4.0)))
  (setg 'wd-eighth-note-duration (to-int (/ full-samples 8.0)))
  (setg 'wd-sixteenth-note-duration (to-int (/ full-samples 16.0))))
;; BPM can be dynamic. Visions of "In the Hall of the Mountain King"
(wd-set-bpm 180.0)

(defun wd-amplify (amplitude data)
  (wd-multiply data (wd-flat-amplitude amplitude (wd-len data))))

;; Inverts the amplitude of the given data
(defun wd-invert (data)
  (wd-amplify -1.0 data))

(defun wd-slope-down (duration) 
  (wd-reverse (wd-slope-up duration)))

;; Shifts the given data by some amount
(defun wd-shift (amount data)
  (wd-superimpose (wd-flat-amplitude amount (wd-len data)) data))

;; Envelopes wavedata using ADSR
;; attack:  Sample count of the attack slope
;; decay:   Sample count of the decay slope
;; sustain: Sustain amplitude (as a percentage)
;; release: Sample count of release slope
;; See https://support.apple.com/en-ca/guide/logicpro/lgsife419620/mac
(defun wd-adsr (attack decay sustain release data)
  (set 'hold-time (- (wd-len data) (+ attack (+ decay release))))
  (if (ge? hold-time 0) nil
    (progn
      (println "wd-asdr hold time must positive")
      (exit 1)))
  (set 'attack-slope (wd-slope-up attack))
  (set 'decay-drop (- 1.0 sustain))
  (set 'decay-slope (wd-shift sustain (wd-amplify decay-drop (wd-slope-down decay))))
  (set 'hold-plateau (wd-flat-amplitude sustain hold-time))
  (set 'release-slope (wd-amplify sustain (wd-slope-down release)))
  (set 'envelope (reduce wd-concat (list attack-slope decay-slope hold-plateau release-slope)))
  (wd-multiply envelope data))

(defun wd-decay (data)
  (set 'decay-envelope (wd-slope-down (wd-len data)))
  (wd-multiply decay-envelope data))

(defun wd-pad-to-full-note (data)
  (set 'padding-len (% (wd-len data) wd-full-note-duration))
  (wd-concat data (wd-flat-amplitude 0.0 padding-len)))

;; Superimposes every wavedata in the given list
(defun wd-superimpose-all (l)
  (reduce wd-superimpose l))

(defun wd-delay (data delay-duration)
  (wd-concat (wd-flat-amplitude 0.0 delay-duration) data))

;; Accepts a list of lists
;; Each element in the list is a list of values to superimpose
;; The index of the sub-list indicates what note index to play at (indexed on quarter notes)
(defun wd-build-track (track)
  (set 'subtracks-with-offsets
       (filter (lambda (elem) (true? (cdr elem)))
               (map (lambda (subtrack)
                      (cons (* (car subtrack) wd-quarter-note-duration) (cdr subtrack)))
                    (enumerate (map wd-superimpose-all track)))))
  (fold (lambda (acc it)
          (wd-superimpose-insert (cdr it) (car it) acc))
        (cdr (car subtracks-with-offsets))
        (cdr subtracks-with-offsets)))

(defun wd-zero (duration)
  (wd-flat-amplitude 0.0 duration))

(defun envelope-soft-full (data)
  (set 'duration wd-full-note-duration)
  (set 'attack (/ duration 16))
  (set 'decay (/ duration 4))
  (set 'sustain 0.5)
  (set 'release (/ duration 16))
  (wd-pad-to-full-note (wd-adsr attack decay sustain release data)))

(defun envelope-soft-half (data)
  (set 'duration wd-half-note-duration)
  (set 'attack (/ duration 16))
  (set 'decay (/ duration 4))
  (set 'sustain 0.5)
  (set 'release (/ duration 16))
  (wd-pad-to-full-note (wd-adsr attack decay sustain release data)))

(defun envelope-soft-quarter (data)
  (set 'duration wd-quarter-note-duration)
  (set 'data (wd-subsample 0 duration data))
  (set 'attack (/ duration 16))
  (set 'decay (/ duration 4))
  (set 'sustain 0.5)
  (set 'release (/ duration 16))
  (wd-adsr attack decay sustain release data))

(defun envelope-soft-eighth (data)
  (set 'duration wd-eighth-note-duration)
  (set 'data (wd-subsample 0 duration data))
  (set 'attack (/ duration 16))
  (set 'decay (/ duration 4))
  (set 'sustain 0.5)
  (set 'release (/ duration 16))
  (wd-adsr attack decay sustain release data))

(defun envelope-soft-sixteenth (data)
  (set 'duration wd-sixteenth-note-duration)
  (set 'data (wd-subsample 0 duration data))
  (set 'attack (/ duration 16))
  (set 'decay (/ duration 4))
  (set 'sustain 0.5)
  (set 'release (/ duration 16))
  (wd-adsr attack decay sustain release data))

;; A simple pure tone synth
(defun synth-pure (frequency)
  (wd-pure-tone frequency wd-full-note-duration))

;; Idk why this sound reminds me of a frog
;; The basic idea is to envelope a pure tone with some harmonic frequencies
(defun synth-frog (frequency)
  (set 'base (synth-pure frequency))
  (set 'frog1 (wd-shift 0.5 (wd-amplify 0.5 (synth-pure (/ frequency 2.0)))))
  (set 'frog2 (wd-shift 0.5 (wd-amplify 0.5 (synth-pure (/ frequency 4.0)))))
  (set 'frog (wd-multiply frog1 frog2))
  (wd-multiply frog base))

(defun synth-harmonic-overtones (frequency base-synth overtone-pairs)
  (set 'base (base-synth frequency))
  (set 'overtones (map
                    (lambda (pair)
                      (wd-amplify (car pair) (base-synth (* frequency (cdr pair)))))
                    overtone-pairs))
  (reduce wd-superimpose (cons base overtones)))

(defun synth-organ (frequency)
  (set 'overtones
       '((0.50 . 2.0)
         (0.50 . 4.0)
         (0.10 . 8.0)
         (0.05 . 16.0)))
  (wd-amplify 0.5 (synth-harmonic-overtones frequency synth-pure-full overtones)))

;; FIXME: Why do we need to pad these?
(defun synth-noise-hat ()
  (set 'duration wd-half-note-duration)
  (set 'noise (wd-amplify 0.2 (wd-noise duration)))
  (set 'attack 0)
  (set 'decay (to-int (/ duration 2.0)))
  (set 'sustain 0.2)
  (set 'release (to-int (/ duration 2.0)))
  (wd-pad-to-full-note (wd-adsr attack decay sustain release noise)))

(defun synth-noise-hat-muted ()
  (set 'duration wd-half-note-duration)
  (set 'noise (wd-amplify 0.2 (wd-noise duration)))
  (set 'attack 0)
  (set 'decay (to-int (/ duration 2.0)))
  (set 'sustain 0.0)
  (set 'release 0)
  (wd-pad-to-full-note (wd-adsr attack decay sustain release noise)))

;; https://output.com/blog/get-perfect-kick-drum
(defun synth-kick ()
  (set 'duration wd-full-note-duration)
  (set 'base (wd-shifting-pure-tone d2 0.0 duration))
  ;(set 'overtones (wd-decay (wd-shifting-pure-tone d3 0.0 wd-quarter-note-duration)))
  (set 'overtones (wd-decay (wd-shifting-pure-tone d3 c3 0)))
  (set 'attack (/ duration 16))
  (set 'decay (- duration (/ duration 8)))
  (set 'sustain 0.0)
  (set 'release 0)
  (wd-amplify 0.5 (wd-adsr attack decay sustain release (wd-superimpose base overtones))))
