;; File required to generate music / wavedata

;; Default sample rate is CD quality
;; NOTE: It _must_ be set before you start manipulating any wave data!
;; Otherwise.. strange things will happen
(setg 'wd-sample-rate 44100)
;; BPM can be dynamic, though. Visions of "In the Hall of the Mountain King"
(setg 'wd-bpm 80.0)

(defun wd-amplify (amplitude data)
  (wd-multiply data (wd-flat-amplitude amplitude (wd-len data))))

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
