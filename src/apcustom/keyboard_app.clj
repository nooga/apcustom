(ns apcustom.keyboard-app
  (:require [apcustom.app :as app]
            [apcustom.midi :as midi]
            [apcustom.apc :as apc]
            [apcustom.ui :as ui]
            [apcustom.music :as music]
            [taoensso.timbre :as log]
            ))



(defn map-scale [key scale slide]
  (let [sm (music/scale-field key scale)]
    (reduce
     (fn [acc n]
       (concat acc (->> sm
                        (drop (+ (* n (- slide)) (* n 8)))
                        (take 8))))
     '() (range 8))
    )
  )

(defn map-roots [scale slide]
  (let [c (-> (music/SCALE scale) count dec (take (repeat 0)) (conj (ui/color :yellow)))
        sm (cycle c)]
    (reduce
     (fn [acc n]
       (concat acc (->> sm
                        (drop (+ (* n (- slide)) (* n 8)))
                        (take 8))))
     '() (range 8))
    )
  )

(defn calc-highlight [sm pk]
  (vec (map (fn [x]
              (when (pk x) (ui/color :green))) sm)))

(defn octave-up [this apc]
  (let [n (update this :octave #(mod (inc %) 5))]
    (log/info "Octave +1 -> " (:octave n))
    n)
  )

(defn octave-down [this apc]
   (let [n (update this :octave #(mod (dec %) 5))]
    (log/info "Octave -1 -> " (:octave n))
    n)
  )

(def mkeys [:C :C# :D :D# :E :F :F# :G :G# :A :A# :B])
(def scales [:major :minor :major-pentatonic :minor-pentatonic
             :diatonic :ionian :dorian :phrygian :lydian :mixolydian
             :aeolian :locrian :pentatonic :melodic-minor :lydian-minor])

(defn coll-inc [cur coll]
  (second (drop-while #(not= % cur) (cycle coll))))

(defn coll-dec [cur coll]
  (last (take-while #(not= % cur) (cycle coll))))

(defn key-down [this apc]
  (let [n
        (app/init (update this :key coll-dec mkeys) apc)]
    (log/info "Key -1 -> " (:key n))
    n
    )
  )

(defn key-up [this apc]
  (let [n (app/init (update this :key coll-inc mkeys) apc)]
    (log/info "Key +1 -> " (:key n))
    n
    )
  )

(defn scale-cycle [this apc]
  (let [n (app/init (update this :scale coll-inc scales) apc)]
    (log/info "Scale +1 -> " (:scale n))
    n
    )

  )

(defn on-message [{:keys [scale-map layers pressed-keys octave] :as this} apc {:keys [kind d1 d2] :as msg}]

  (log/info "processing msg" msg)

  (cond
    (ui/matrix? msg)
    (do
      (if (midi/kind? msg :note-on)
        (let [note (nth scale-map d1)
              npk (conj pressed-keys note)]
          (log/info "note on" note)
          (apc/send-out apc :note-on 0 (+ octave note) 127)
          (-> this
              (assoc-in [:layers 1] (calc-highlight scale-map npk))
              (assoc :pressed-keys npk))
          )
        (let [note (nth scale-map d1)
              npk (disj pressed-keys note)]
          (apc/send-out apc :note-off 0 (+ octave note) 127)
          (log/info "note off")
          (-> this
              (assoc-in [:layers 1] (calc-highlight scale-map npk))
              (assoc :pressed-keys npk)))))


    (ui/bottom? msg)
    (if (midi/kind? msg :note-off)
      (({64 octave-up
         65 octave-down
         66 key-down
         67 key-up
         68 scale-cycle} d1) this apc)
      this)


    :else
    (do
      (log/info "ignore")
      this)
    ))


(defrecord KeyboardApp [key scale octave layers pressed-keys roots-map scale-map]
  app/App
  (init [this apc]
    (let [sc (or (:scale this) :major)
          ky (or (:key this) :C)
          scalem (map-scale ky sc 5)
          roots (map-roots sc 5)]
      (assoc this
             :key ky
             :scale sc
             :octave (or (:octave this) 2)
             :pressed-keys #{}
             :roots-map roots
             :scale-map scalem
             :layers [;;(ui/layer-clear (ui/color :off)) ;; root overlay
                      (vec roots)
                      (ui/layer-clear [])
                      ;;... button overlays go here
                      ])))

  (activate [this apc]
    this ;;nop
   )

  (deactivate [this apc]
    this ;;nop
    )

  (close [this apc]
    this ;;nop
    )

  (tick [this apc]
    this ;;nop
    )

  (display [this apc]
    layers)

  (message [this apc msg]
    (try
      (on-message this apc msg)
      (catch Exception e
        (log/error "error in handler" e)
        this))
    ;;this
    ))

(defn kbd-app []
  (map->KeyboardApp {}))
