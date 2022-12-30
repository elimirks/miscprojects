(require "lisp/common")
(require "lisp/wd")

(wd-pure-tone frequency wave-count)

(debug (wavedata))
(println (to-string wd-bpm))
(debug wd-sample-rate)
