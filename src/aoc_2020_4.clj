(ns aoc-2020-4
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;===========[Part 1]===========
(def field->code {:birth-year      "byr"
                  :issue-year      "iyr"
                  :expiration-year "eyr"
                  :height          "hgt"
                  :hair-color      "hcl"
                  :eye-color       "ecl"
                  :passport-id     "pid"
                  :country-id      "cid"})

(s/def :passport/birth-year      #(re-find (re-pattern (format "%s:" (field->code :birth-year))) %))
(s/def :passport/issue-year      #(re-find (re-pattern (format "%s:" (field->code :issue-year))) %))
(s/def :passport/expiration-year #(re-find (re-pattern (format "%s:" (field->code :expiration-year))) %))
(s/def :passport/height          #(re-find (re-pattern (format "%s:" (field->code :height))) %))
(s/def :passport/hair-color      #(re-find (re-pattern (format "%s:" (field->code :hair-color))) %))
(s/def :passport/eye-color       #(re-find (re-pattern (format "%s:" (field->code :eye-color))) %))
(s/def :passport/passport-id     #(re-find (re-pattern (format "%s:" (field->code :passport-id))) %))
(s/def :passport/country-id      #(re-find (re-pattern (format "%s:" (field->code :country-id))) %))

(s/def :passport/available
  (s/keys :req [:passport/birth-year
                :passport/issue-year
                :passport/expiration-year
                :passport/height
                :passport/hair-color
                :passport/eye-color
                :passport/passport-id]
          :opt [:passport/country-id]))


(defn valid-passport? [passport-text]
  (s/valid? :passport/available {:passport/birth-year      passport-text
                                 :passport/issue-year      passport-text
                                 :passport/expiration-year passport-text
                                 :passport/height          passport-text
                                 :passport/hair-color      passport-text
                                 :passport/eye-color       passport-text
                                 :passport/passport-id     passport-text}))

(def passport-texts (-> (slurp "resources/input_2020_4.txt")
                        (str/split #"\n\n")))

(comment
  (->> passport-texts
       (filter valid-passport?)
       count))


;===========[Part 2]===========

(defn str->year [str]
  (if (re-matches #"[0-9]{4}" str)
    (Integer/parseInt str)
    str))

(defn str->height [str]
  (if-let [[_ height type]
           (re-matches #"([0-9]+)(cm|in)" str)]
    {:height (Integer/parseInt height)
     :type (keyword type)}
    {:height 0 :type nil}))


(defn format-passport 
  "Parse
  {:iyr 2013,  :byr 1997,
  :hgt {:height 182, :type \"cm\"},
  :hcl \"#ceb3a1\",
  :eyr 2027,  :ecl :gry,
  :cid \"102\",
  :pid \"018128535\"}"
  [[key-str value-str]]
  (let [key (keyword key-str)]
    (case key
     :byr {key (str->year value-str)}
     :iyr {key (str->year value-str)}
     :eyr {key (str->year value-str)}
     :hgt {key (str->height value-str)}
     :hcl {key value-str}
     :ecl {key (keyword value-str)}
     :pid {key value-str}
     :cid {key value-str}
     nil)))

(defn parse-passport [passport-text]
  (->> (-> (str/replace passport-text #" |:|\n" " ")
           (str/split #" "))
       (partition 2)
       (keep format-passport)
       (apply merge)))

(defn valid-height? [height-and-type]
  (let [height (:height height-and-type)
        type (:type height-and-type)]
    (when (and height type)
      (cond (and (= type :cm)
                 (s/int-in-range? 150 194 height)) true
            (and (= type :in)
                 (s/int-in-range? 59 77 height)) true
            :else false))))

(defn valid-hair-color? [heir-color]
  (when heir-color
    (re-matches #"#[0-9|a-f]{6}" heir-color)))

(defn valid-passport-id? [passport-id]
  (when passport-id
    (re-matches #"[0-9]{9}" passport-id)))

(def eye-colors #{:amb :blu :brn :gry :grn :hzl :oth})

(s/def :complete-passport/birth-year      (s/int-in 1920 2003))
(s/def :complete-passport/issue-year      (s/int-in 2010 2021))
(s/def :complete-passport/expiration-year (s/int-in 2020 2031))
(s/def :complete-passport/height       #(valid-height? %))
(s/def :complete-passport/hair-color   #(valid-hair-color? %))
(s/def :complete-passport/eye-color    #(eye-colors %))
(s/def :complete-passport/passport-id  #(valid-passport-id? %))

(s/def :complete-passport/available
  (s/keys :req [:complete-passport/birth-year
                :complete-passport/issue-year
                :complete-passport/expiration-year
                :complete-passport/height
                :complete-passport/hair-color
                :complete-passport/eye-color
                :complete-passport/passport-id]))


(defn complete-valid-passport? [passport]
  (s/valid? :complete-passport/available
              {:complete-passport/birth-year      (passport :byr)
               :complete-passport/issue-year      (passport :iyr)
               :complete-passport/expiration-year (passport :eyr)
               :complete-passport/height          (passport :hgt)
               :complete-passport/hair-color      (passport :hcl)
               :complete-passport/eye-color       (passport :ecl)
               :complete-passport/passport-id     (passport :pid)}))

(comment
  (->> passport-texts
       (map parse-passport)
       (filter complete-valid-passport?)
       count))

