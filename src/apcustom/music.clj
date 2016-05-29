(ns apcustom.music)


(def NOTES {:C  0  :c  0  :b# 0  :B# 0
            :C# 1  :c# 1  :Db 1  :db 1  :DB 1  :dB 1
            :D  2  :d  2
            :D# 3  :d# 3  :Eb 3  :eb 3  :EB 3  :eB 3
            :E  4  :e  4
            :E# 5  :e# 5  :F  5  :f  5
            :F# 6  :f# 6  :Gb 6  :gb 6  :GB 6  :gB 6
            :G  7  :g  7
            :G# 8  :g# 8  :Ab 8  :ab 8  :AB 8  :aB 8
            :A  9  :a  9
            :A# 10 :a# 10 :Bb 10 :bb 10 :BB 10 :bB 10
            :B  11 :b  11 :Cb 11 :cb 11 :CB 11 :cB 11})

(def SCALE
  (let [ionian-sequence     [2 2 1 2 2 2 1]
        hex-sequence        [2 2 1 2 2 3]
        pentatonic-sequence [3 2 2 3 2]
        rotate (fn [scale-sequence offset]
                 (take (count scale-sequence)
                       (drop offset (cycle scale-sequence))))]
    {:diatonic           ionian-sequence
     :ionian             (rotate ionian-sequence 0)
     :major              (rotate ionian-sequence 0)
     :dorian             (rotate ionian-sequence 1)
     :phrygian           (rotate ionian-sequence 2)
     :lydian             (rotate ionian-sequence 3)
     :mixolydian         (rotate ionian-sequence 4)
     :aeolian            (rotate ionian-sequence 5)
     :minor              (rotate ionian-sequence 5)
     :locrian            (rotate ionian-sequence 6)
     :hex-major6         (rotate hex-sequence 0)
     :hex-dorian         (rotate hex-sequence 1)
     :hex-phrygian       (rotate hex-sequence 2)
     :hex-major7         (rotate hex-sequence 3)
     :hex-sus            (rotate hex-sequence 4)
     :hex-aeolian        (rotate hex-sequence 5)
     :minor-pentatonic   (rotate pentatonic-sequence 0)
     :yu                 (rotate pentatonic-sequence 0)
     :major-pentatonic   (rotate pentatonic-sequence 1)
     :gong               (rotate pentatonic-sequence 1)
     :egyptian           (rotate pentatonic-sequence 2)
     :shang              (rotate pentatonic-sequence 2)
     :jiao               (rotate pentatonic-sequence 3)
     :pentatonic         (rotate pentatonic-sequence 4) ;; historical match
     :zhi                (rotate pentatonic-sequence 4)
     :ritusen            (rotate pentatonic-sequence 4)
     :whole-tone         [2 2 2 2 2 2]
     :whole              [2 2 2 2 2 2]
     :chromatic          [1 1 1 1 1 1 1 1 1 1 1 1]
     :harmonic-minor     [2 1 2 2 1 3 1]
     :melodic-minor-asc  [2 1 2 2 2 2 1]
     :hungarian-minor    [2 1 3 1 1 3 1]
     :octatonic          [2 1 2 1 2 1 2 1]
     :messiaen1          [2 2 2 2 2 2]
     :messiaen2          [1 2 1 2 1 2 1 2]
     :messiaen3          [2 1 1 2 1 1 2 1 1]
     :messiaen4          [1 1 3 1 1 1 3 1]
     :messiaen5          [1 4 1 1 4 1]
     :messiaen6          [2 2 1 1 2 2 1 1]
     :messiaen7          [1 1 1 2 1 1 1 1 2 1]
     :super-locrian      [1 2 1 2 2 2 2]
     :hirajoshi          [2 1 4 1 4]
     :kumoi              [2 1 4 2 3]
     :neapolitan-major   [1 2 2 2 2 2 1]
     :bartok             [2 2 1 2 1 2 2]
     :bhairav            [1 3 1 2 1 3 1]
     :locrian-major      [2 2 1 1 2 2 2]
     :ahirbhairav        [1 3 1 2 2 1 2]
     :enigmatic          [1 3 2 2 2 1 1]
     :neapolitan-minor   [1 2 2 2 1 3 1]
     :pelog              [1 2 4 1 4]
     :augmented2         [1 3 1 3 1 3]
     :scriabin           [1 3 3 2 3]
     :harmonic-major     [2 2 1 2 1 3 1]
     :melodic-minor-desc [2 1 2 2 1 2 2]
     :romanian-minor     [2 1 3 1 2 1 2]
     :hindu              [2 2 1 2 1 2 2]
     :iwato              [1 4 1 4 2]
     :melodic-minor      [2 1 2 2 2 2 1]
     :diminished2        [2 1 2 1 2 1 2 1]
     :marva              [1 3 2 1 2 2 1]
     :melodic-major      [2 2 1 2 1 2 2]
     :indian             [4 1 2 3 2]
     :spanish            [1 3 1 2 1 2 2]
     :prometheus         [2 2 2 5 1]
     :diminished         [1 2 1 2 1 2 1 2]
     :todi               [1 2 3 1 1 3 1]
     :leading-whole      [2 2 2 2 2 1 1]
     :augmented          [3 1 3 1 3 1]
     :purvi              [1 3 2 1 1 3 1]
     :chinese            [4 2 1 4 1]
     :lydian-minor       [2 2 2 1 1 2 2]}))

(defn scale-field
  "Create the note field for a given scale.  Scales are specified with
  a keyword representing the key and an optional scale
  name (defaulting to :major):
  (scale-field :g)
  (scale-field :g :minor)"
  [skey & [sname]]
  (let [base (NOTES skey)
        sname (or sname :major)
        intervals (SCALE sname)]
    (reverse (next
      (reduce (fn [mem interval]
              (let [new-note (+ (first mem) interval)]
                (conj mem new-note)))
            (list base)
            (take (* 8 12) (cycle intervals)))))))
