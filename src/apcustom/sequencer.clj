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
    (println steps-curr step-num)
    (swap! state update-in [:sequencer :tick] inc)))

(defn sequencer-logic [state]
  #(loop [msg (async/<!! (get-in @state [:sequencer :seq-ch]))]
    (condp = msg
      :stop nil
      :tick (do (tick state)
                (recur (async/<!! (get-in @state [:sequencer :seq-ch])))))))

(defn init-seq [length tick-res bpm]
  (let [steps-num 16
        tick-step 10 ;FIXIT: kurwa
        steps (map #(vector (* tick-step %) {:pitch (scales/random-weighted :phrygian-dominant)}) (range steps-num))]
    (into {} steps)))

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


(defn sequencer [state]
  (let [bpm (:bpm @state)
        tick-time (/ 60000 bpm (:tick-res @state))
        seq-ch (async/chan)
        seq-out (async/chan)
        control-ch (async/chan)
        init-seq (init-seq 1 (:tick-res @state) bpm)
        seq {:tick-time tick-time :bpm bpm :control-ch control-ch :seq-out seq-out :steps init-seq :seq-ch seq-ch :length 160 :tick 0}] ;FIXIT calculate length kurwa
    (swap! state assoc :sequencer seq)
    (.start (Thread. (sequencer-logic state)))
      (.start (Thread. (clock-logic state)))))
