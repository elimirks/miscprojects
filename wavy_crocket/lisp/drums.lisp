(require "lisp/common")
(require "lisp/wd")

(set 'result (synth-kick))

;(wd-plot (wd-amplify 0.5 result))
(wd-play (wd-amplify 1.0 result))
