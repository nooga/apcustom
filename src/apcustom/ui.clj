(ns apcustom.ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7 .  .  .  .  .  .  .  .   ;;
;; 6 .  .  .  .  .  .  .  .   ;;
;; 5 .  .  .  .  .  .  .  .   ;;
;; 4 .  .  .  .  .  .  .  .   ;;
;; 3 .  .  .  .  .  .  .  .   ;;
;; 2 .  .  .  .  .  .  .  .   ;;
;; 1 .  .  .  .  .  .  .  .   ;;
;; 0 .  .  .  .  .  .  .  .   ;;
;;   0  1  2  3  4  5  6  7   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lin->xy [addr]
  {:pre [(<= 0 addr 63)]}
  [(mod addr 8) (quot addr 8)])

(defn xy->lin [x y]
  {:pre [(<= 0 x 7) (<= 0 y 7)]}
  (+ x (* y 8)))

(defn color
  ([c] (color c false))
  ([c blink]
   {:pre [(#{:off :green :red :yellow :none} c)]}
   (let [k (get {:off 0 :green 1 :red 3 :yellow 5 :none nil} c 0)]
     (+ k
        (if (and (> k 0) blink) 1 0)))))

(color :green)

(defn layer-clear
  ([layer]     (layer-clear layer nil))
  ([layer col] (->> col repeat (take 64) vec)))

(defn layer-set [layer pos col]
  (assoc layer pos col))

(defn layer-merge-method [stack]
  (or (last (filter some? stack)) 0))

(defn layers-merge [layers]
  (->> (apply map list layers)
       (map layer-merge-method)))

(defn layer-kill [layers n]
  (vec (concat (take n layers)
               (drop (inc n) layers))))

(defn matrix? [{:keys [kind d1 d2] :as msg}]
  (when (<= 0 d1 63) msg))

(defn bottom? [{:keys [kind d1 d2] :as msg}]
  (when (<= 64 d1 71) msg))
