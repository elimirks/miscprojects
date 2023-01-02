(require "lisp/common")
(require "lisp/wd")

(set 'result (reduce wd-concat
        (map synth-frog
             (list
               e4 d4 c4 d4 e4 e4 e4
               d4 d4 d4 e4 e4 e4
               e4 d4 c4 d4 e4 e4 e4
               e4 d4 d4 e4 d4 c4))))

(wd-play (wd-amplify 0.5 result))
;(wd-save (wd-amplify 0.5 result) "out.wav")
