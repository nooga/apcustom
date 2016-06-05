(ns apcustom.push
  (:require [clojure.core.async :refer [chan >! >!! <!! <! close! go mix pub sub admix unmix unmix-all unsub-all] :as async]
            [apcustom.app :refer :all]
            [apcustom.midi :as midi]
            [apcustom.ui :as ui]
            [apcustom.app :as app]
            [taoensso.timbre :as log]
            [apcustom.sequencer :as s]))

(defrecord Controller [controller-in controller-out midi-out midi-in state])

(defn push-test-seq [push]
  (let [out (midi/output (:controller-out push) 0)
        state (atom {:bpm 40 :tick-res 10})
        _ (s/sequencer state)]
    (async/go-loop [msg (<! (get-in @state [:sequencer :seq-out]))]
      (println "push-tick")
      (>!! out (midi/msg :note-on 0 (:pitch msg) 64))
      (recur (<! (get-in @state [:sequencer :seq-out]))))))

(defn push [push-in push-out midi-out midi-in]
  (Controller. push-in push-out midi-out midi-in nil))

(def push-c
  (let [devices (midi/midi-devs)]
    (push (midi/midi-in-device (get devices 3))
          (midi/midi-out-device (get devices 6))
          (midi/midi-out-device (get devices 4))
          nil;(midi/midi-in-device (get devices 1))
          )))
