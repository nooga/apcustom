(ns apcustom.midi
  (:import [javax.sound.midi MidiSystem ShortMessage Receiver])
  (:require [clojure.core.async :refer [chan >! >!! <!! <! close! go mix pub sub admix unmix unmix-all unsub-all]]

            [taoensso.timbre :as log]))

(defrecord Msg [kind channel d1 d2])

(def cmd->kw
  {0xf1 :midi-time-code
   0xf2 :song-position-pointer
   0xf3 :song-select
   0xf6 :tune-request
   0xf7 :end-of-exclusive
   0xf8 :timing-clock
   0xfa :start
   0xfb :continue
   0xfc :stop
   0xfe :active-sensing
   0xff :system-reset
   0x80 :note-off
   0x90 :note-on
   0xa0 :poly-pressure
   0xb0 :control-change
   0xc0 :program-change
   0xd0 :channel-pressure
   0xe0 :pitch-bend})

(def kw->cmd (clojure.set/map-invert cmd->kw))

(defn ShortMsg->Msg [m]
  (Msg. (cmd->kw (.getCommand m)) (.getChannel m) (.getData1 m) (.getData2 m)))

(defn Msg->ShortMsg [m]
  (ShortMessage. (-> m :kind kw->cmd) (:channel m) (:d1 m) (:d2 m)))

(defrecord MidiDevice [dir in-hub out-hub worker receiver raw-dev])

(defn midi-in-device [dev]
  (let [_ (.open dev)
        inbound (chan 64)
        receiver (reify Receiver
                   (close [this]
                     (log/info "midi inbound channel" dev  "closed")
                     (close! inbound))
                   (send [this msg time]
                     (let [r (ShortMsg->Msg msg)]
                       (log/info "-->" r)
                       (go (>! inbound r)
                           (log/info "wrote")))
                     ))]

    (.setReceiver (.getTransmitter dev) receiver)

    (MidiDevice. :in
                 (pub inbound :channel)
                 nil
                 nil
                 receiver
                 dev)))


(defn midi-out-device [dev]
  (let [_ (.open dev)

        outbound (chan 64)

        sender (.getReceiver dev)
        worker (go
                 (loop [m (<! outbound)]
                   (log/info "sending" m )
                   (log/info "ret" (.send sender (Msg->ShortMsg m) (+ 1000 (.getMicrosecondPosition dev))))
                   (recur (<! outbound))))]

    (MidiDevice. :out
                 nil
                 (mix outbound)
                 worker
                 nil
                 dev)))

(defn close-device [dev]
  (when-let [x (:in-hub dev)] (unsub-all x))
  (when-let [x (:out-hub dev)] (unmix-all x))
  (when-let [x (:worker dev)] (close! x))
  (when-let [x (:receiver dev)] (.close x))
  (when-let [x (:raw-dev dev)] (.close x)))

(defn input [midi-device midi-chan]
  (if (= :in (:dir midi-device))
    (let [c (chan)]
      (sub (-> midi-device :in-hub) midi-chan c)
      c)
    (log/info "error: cannot output to input device")))

(defn output [midi-device midi-chan]
  (if (= :out (:dir midi-device))
    (let [c (chan)]
      (admix (-> midi-device :out-hub) c)
      c)
    (log/info "error: cannot input from output device")))

(defn midi-devs []
  (let [midi-infos (MidiSystem/getMidiDeviceInfo)]
    (zipmap (range (alength midi-infos))
            (map #(MidiSystem/getMidiDevice %) midi-infos))))

(defn kind? [m t]
  (= (:kind m) t))

(defn msg [k c d1 d2]
  (Msg. k c d1 d2))
