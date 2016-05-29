(ns apcustom.app)

(defprotocol App
  (init [this apc])
  (activate [this apc])
  (deactivate [this apc])
  (close [this apc])
  (tick [this apc])
  (display [this apc])
  (message [this apc msg]))
