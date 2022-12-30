;; File required to generate music / wavedata

;; Default sample rate is CD quality
;; NOTE: It _must_ be set before you start manipulating any wave data!
;; Otherwise.. strange things will happen
(setg 'wd-sample-rate 44100)
;; BPM can be dynamic, though. Visions of "In the Hall of the Mountain King"
(setg 'wd-bpm 80.0)

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

;; A simple pure tone synth
(defun synth-round (frequency)
  ;; 1 quarter second length
  (set 'duration (/ wd-sample-rate 4))
  (set 'attack (/ duration 16))
  (set 'decay (/ duration 4))
  (set 'sustain 0.5)
  (set 'release (/ duration 16))
  (wd-adsr attack decay sustain release (wd-pure-tone frequency duration)))

(setg 'c4-freq 261.63)
(setg 'cs4-freq 277.18)
(setg 'd4-freq 293.66)
(setg 'ds4-freq 311.13)
(setg 'e4-freq 329.63)
(setg 'f4-freq 329.63)
(setg 'g4-freq 392.0)
(setg 'a4-freq 440.0)
(setg 'b4-freq 493.88)

(setg 'c4-major (list c4-freq e4-freq g4-freq))
