(ns apcustom.sequencer
  (:require [clojure.core.async :as async]
            [apcustom.scales :as scales]
                        [taoensso.timbre :as log]))

;control-ch->clock->seq-ch->seq->seq-out

(defn tick [state]
  (let [seq (:sequencer @state)
        {:keys [:steps :length :tick]} seq
        step-num (mod tick length)
        steps-curr (get steps step-num)]
    (when steps-curr
      (async/put! (:seq-out seq) steps-curr))
    (swap! state update-in [:sequencer :tick] inc)))

(defn sequencer-logic [state]
  #(loop [msg (async/<!! (get-in @state [:sequencer :seq-ch]))]
    (condp = msg
      :stop nil
      :tick (do (tick state)
                (recur (async/<!! (get-in @state [:sequencer :seq-ch])))))))

(defn note-off-step [step tick tick-step]
  (vector (+ tick (/ tick-step 2)) (assoc step :type :note-off)))

(defn init-seq [length tick-res tick-step]
  (let [steps-num (* length 8)
        steps (map #(vector (* tick-step %) {:pitch 37;(scales/random-weighted :phrygian-dominant)
                                             :type :note-on}) (range steps-num))
        note-off-steps (map (fn [[k v]] (vector (+ k (/ tick-step 2))
                                               {:pitch (:pitch v)
                                                :type :note-off})) steps)]
    (into {} (concat steps note-off-steps))))

(defn clock-logic [state]
  #(async/go-loop [[m p] (async/alts! [(get-in @state [:sequencer :control-ch])
                                       (async/timeout (get-in @state [:sequencer :tick-time]))])]
     (let [seq (:sequencer @state)]
       (if (= m :stop)
         (async/put! (:seq-ch seq) :stop)
         (do
           (async/put! (:seq-ch seq) :tick)
           (recur (async/alts! [(:control-ch seq)
                                (async/timeout (:tick-time seq))])))))))

(defn add-step [note column tick-step steps]
  (let [note-on {:pitch note :type :note-on}
        note-on-tick (* column tick-step)
        [off-tick off] (note-off-step note-on note-on-tick tick-step)]
    (-> steps (assoc note-on-tick note-on) (assoc off-tick off))))

(defn update-steps [note column state]
  (swap! state update-in [:sequencer :steps] #(add-step note column
                                                   (get-in @state [:sequencer :tick-step])
                                                   %)))

(defn sequencer [state]
  (let [bpm (:bpm @state)
        tick-res (:tick-res @state)
        tick-step (/ tick-res 8)
        tick-time (/ 60000 (/ bpm 4) tick-res)
        seq-ch (async/chan)
        seq-out (async/chan)
        control-ch (async/chan)
        seq-len 1
        init-seq (init-seq seq-len tick-res tick-step)
        seq {:tick-time tick-time :bpm bpm :control-ch control-ch :seq-out seq-out :steps init-seq :seq-ch seq-ch :length (* tick-res seq-len) :tick 0 :tick-step tick-step}]
    (swap! state assoc :sequencer seq)
    (.start (Thread. (sequencer-logic state)))
    (.start (Thread. (clock-logic state)))))
