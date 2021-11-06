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

(defn parse-year-in-passport-words [passport-words]
  (->> passport-words
       (map str->year)))

;Parse
; "iyr:2013 byr:1997 hgt:182cm hcl:#ceb3a1\neyr:2027\necl:gry cid:102 pid:018128535"
; => ["iyr" "2013" "byr" "1997" "hgt" "182cm" "hcl" "#ceb3a1" "eyr" "2027" "ecl" "gry" "cid" "102" "pid" "018128535"]
; => {"hgt" "182cm", "pid" "018128535", "byr" 1997, "eyr" 2027, "iyr" 2013, "ecl" "gry", "cid" "102", "hcl" "#ceb3a1"}
(def passports (->> passport-texts
                    (keep (fn [text] (when (valid-passport? text) text)))
                    (map (fn [text]
                           (-> (str/replace text #" |:|\n" " ")
                               (str/split #" "))))
                    (map parse-year-in-passport-words)
                    (map (fn [words]
                           (apply hash-map words)))))

(s/def :complete-passport/birth-year      (s/int-in 1920 2003))
(s/def :complete-passport/issue-year      (s/int-in 2010 2021))
(s/def :complete-passport/expiration-year (s/int-in 2020 2031))
(s/def :complete-passport/height       #(re-matches #"1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in" %))
(s/def :complete-passport/hair-color   #(re-matches #"#[0-9|a-f]{6}" %))
(s/def :complete-passport/eye-color    #(re-matches #"amb|blu|brn|gry|grn|hzl|oth" %))
(s/def :complete-passport/passport-id  #(re-matches #"[0-9]{9}" %))

(s/def :complete-passport/available
  (s/keys :req [:complete-passport/birth-year
                :complete-passport/issue-year
                :complete-passport/expiration-year
                :complete-passport/height
                :complete-passport/hair-color
                :complete-passport/eye-color
                :complete-passport/passport-id]))


(defn complete-valid-passport? [passport]
  (s/valid? :complete-passport/available {:complete-passport/birth-year (passport (field->code :birth-year))
                                          :complete-passport/issue-year (passport (field->code :issue-year))
                                          :complete-passport/expiration-year (passport (field->code :expiration-year))
                                          :complete-passport/height (passport (field->code :height))
                                          :complete-passport/hair-color  (passport (field->code :hair-color))
                                          :complete-passport/eye-color (passport (field->code :eye-color))
                                          :complete-passport/passport-id (passport (field->code :passport-id))}))

(comment
  (->> passports
       (filter complete-valid-passport?)
       count))

