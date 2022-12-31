(require "lisp/common")
(require "lisp/wd")

(set 'result (reduce wd-concat
        (map synth-organ
             (list
               e4-freq
               d4-freq
               c4-freq
               d4-freq
               e4-freq
               e4-freq
               e4-freq

               d4-freq
               d4-freq
               d4-freq
               e4-freq
               e4-freq
               e4-freq

               e4-freq
               d4-freq
               c4-freq
               d4-freq
               e4-freq
               e4-freq
               e4-freq

               e4-freq
               d4-freq
               d4-freq
               e4-freq
               d4-freq
               c4-freq))))

(wd-play (wd-amplify 0.5 result))
;(wd-save (wd-amplify 0.5 result) "out.wav")
