(require "lisp/common")
(require "lisp/wd")

(set 'duration (* wd-sample-rate 1))
(set 'segments
     (map
       (lambda (frequency) (wd-amplify 0.3 (wd-pure-tone frequency duration)))
       c4-major))

(set 'result (reduce wd-superimpose segments))

(wd-save (wd-adsr 1000 5000 0.3 3000 result) "out.wav")
