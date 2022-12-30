(require "lisp/common")
(require "lisp/wd")

(set 'data (wd-pure-tone 256.0 10.0))
(debug data)
(wd-save data "out.wav")
