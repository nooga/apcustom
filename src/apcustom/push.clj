(ns apcustom.push
  (:require [clojure.core.async :refer [chan >! >!! <!! <! close! go mix pub sub admix unmix unmix-all unsub-all] :as async]
            [apcustom.app :refer :all]
            [apcustom.midi :as midi]
            [apcustom.ui :as ui]
            [apcustom.app :as app]
            [taoensso.timbre :as log]
            [apcustom.sequencer :as s]))

(defrecord Controller [controller-in controller-out midi-out midi-in state])

(defn push-test-seq [push state]
  (let [out (midi/output (:controller-out push) 0)
        _ (s/sequencer state)]
    (sequencer-display push state)
    (async/go-loop [msg (<! (get-in @state [:sequencer :seq-out]))]
      (>!! out (midi/msg (:type msg) 0 (:pitch msg) (if (= (:type msg) :note-on) 64 0)))
      (recur (<! (get-in @state [:sequencer :seq-out]))))))

(defn push [push-in push-out midi-out midi-in]
  (Controller. push-in push-out midi-out midi-in nil))

(defn stop [state]
  (async/put! (get-in @state [:sequencer :control-ch]) :stop))

(defn push-c []
  (let [devices (midi/midi-devs)]
    (push (midi/midi-in-device (get devices 3))
          (midi/midi-out-device (get devices 6))
          (midi/midi-out-device (get devices 4))
          nil;(midi/midi-in-device (get devices 1))
          )))

(def s (atom {:bpm 40 :tick-res 128}))
(def p (push-c))

(defn column [step]
  (range step 80 8))

(defn tick->column [tick tick-res]
  (if (= tick 0)
    0
    (/ tick 8)))

(defn clear-display [out]
  (doseq [pitch (flatten (map #(column %) (range 8)))]
    (>!! out (midi/msg :note-off 0 pitch 0))))

(defn init-seq-display [push base state]
  (let [out (midi/output (:controller-out push) 0)]
    (clear-display out)
    (doseq [[t v] (get-in @state [:sequencer :steps])]
      (let [col (+ base (tick->column t (:tick-res @state)))
            pitch (+ col (* 8 (- (:pitch v) base)))]
        ;(println t v col pitch)
        (when (and (= :note-on (:type v)) (integer? col) (< (- col base) 8))
          (println "putting " col pitch v)
          (>!! out (midi/msg :note-on 0 pitch 64)))))))

(defn pitch->tick [tick-res]
  )

(defn sequencer-display [push state]
  (let [in (midi/input (:controller-in push) 0)
        base-c 36
        base-r 36]
    (init-seq-display push base-c state)
    (async/go-loop [msg (<! in)]
      )))
