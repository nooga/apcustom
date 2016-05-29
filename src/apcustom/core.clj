(ns apcustom.core
  (:import [javax.sound.midi MidiSystem ShortMessage Receiver])
  (:require [clojure.core.async :refer [chan >! >!! <!! <! close! go mix pub sub admix unmix unmix-all unsub-all]]
            [apcustom.music :refer :all]
            [apcustom.midi :as midi]
            [apcustom.apc :as apc]
            [apcustom.keyboard-app :as kbd-app]
            [taoensso.timbre :as log]))


(comment

  (def devices (midi/midi-devs))

  (def kbd (kbd-app/kbd-app))

  (def apc (apc/apc-mini
            (midi/midi-in-device (get devices 2))
                     (midi/midi-out-device (get devices 4))
                     (midi/midi-out-device (get devices 3))
                     nil;;(midi/midi-in-device (get devices 1))
                     ))

  (apc/load-app apc kbd)

  (do
    (midi/close-device (:apc-in apc))
    (midi/close-device (:apc-out apc))
    (midi/close-device (:midi-in apc))
    (midi/close-device (:midi-out apc))
    (close! (:worker apc)))


  (def outdev (midi-out-device (get devices 4)))

  (def outch1 (midi-output outdev 0))

(write-lights outch1 (scale-display (count (SCALE :minor))))

  (def indev (midi-in-device (get devices 1)))

  (def inch1 (midi-input indev 0))

  (def huj (go (loop [m (<! inch1)]
                 (>! outch1 (Msg. :note-on 0 (:d1 m) 3))
                 (recur (<! inch1)))))

  (close-device outdev)
  (close-device indev)

  (midi-devs)

  (map (fn [d] (-> d .getDeviceInfo .getName)) (vals (midi-devs)))

(close! huj)
  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
