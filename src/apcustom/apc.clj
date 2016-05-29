(ns apcustom.apc
  (:require [clojure.core.async :refer [chan >! >!! <!! <! close! go mix pub sub admix unmix unmix-all unsub-all]]
            [apcustom.app :refer :all]

            [apcustom.midi :as midi]
            [apcustom.ui :as ui]
            [apcustom.app :as app]
            [taoensso.timbre :as log]))

(defrecord APCMini [apc-in apc-out midi-out midi-in state worker])

(defn apc-write-screen [apcd new-screen]
  (log/info "write screen")
  (let [old-screen (:screen @(:state apcd))
        out (midi/output (:apc-out apcd) 0)]
    (swap! (:state apcd) assoc :screen new-screen)

    (doseq [[i o n] (map vector (range 64) old-screen new-screen)]
      (when-not (= o n)
        (>!! out (midi/msg :note-on 0 i n))))))

(defn apc-mini [apc-in apc-out midi-out midi-in]
  (let [apcin0  (midi/input apc-in 0)
        apcout0 (midi/output apc-out 0)
        midiout0 (midi/output midi-out 0)
        midiin0 nil;;(midi/input midi-in 0)
        state (atom {:screen (take 64 (repeat nil))
                     :app nil
                     :buttons-on #{}})
        apc (APCMini. apc-in apc-out midi-out midi-in state nil)
        worker (go (loop [m (<! apcin0)]
                     (log/info "apc -->" m)
                     (if-not (nil? m)
                       (do

                         ;;(apc-mini-process apc m apcin0 apcout0 midiin0 midiout0)
                         (when (:app @state)
                           (swap! state update :app app/message apc m)

                           ;;(println (:app @state))
                           (apc-write-screen apc (ui/layers-merge (app/display (:app @state) apc)))
                           )


                         (recur (<! apcin0)))
                       (log/info "end worker"))))]

    (apc-write-screen apc (take 64 (repeat 0)))
    (assoc apc :worker worker)))

(defn load-app [{:keys [state] :as apc} app]
  (swap! state assoc :app (app/init app apc))
  (apc-write-screen apc (ui/layers-merge (app/display (:app @state) apc)))
  nil)

(defn send-out [apc kind ch d1 d2]
  (log/info "--> MIDI OUT" kind ch d1 d2)
  (>!! (-> apc :midi-out (midi/output ch)) (midi/msg kind ch d1 d2)))
