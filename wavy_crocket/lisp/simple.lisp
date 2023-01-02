(require "lisp/common")
(require "lisp/wd")

(set 'result
     (wd-build-track
       (list
         (list (synth-noise-hat) (envelope-soft-half (synth-pure e4)) (envelope-soft-half (synth-frog c4)))
         (list (envelope-soft-half (synth-frog c4)))
         (list (synth-noise-hat-muted) (envelope-soft-half (synth-pure e4)) (envelope-soft-half (synth-frog c4)))
         (list (envelope-soft-half (synth-pure e4))))))

(wd-play (wd-amplify 0.5 result))
