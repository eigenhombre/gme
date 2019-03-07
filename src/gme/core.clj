(ns gme.core
  (:gen-class)
  (:require [clojure.string :as s]))

{"Impossible" 0
 "Somewhat likely" 1
 "No way" 2
 "Very unlikely" 3
 "Unlikely" 4
 "50/50" 5
 "Likely" 6
 "Very likely" 7
 "Near sure thing" 8
 "A sure thing" 9
 "Has to be" 10}

(def fate-table-values
  [       9           8           7           6           5           4           3          2          1
   0  [10 50 91]  [5 25 86]   [3 15 84]   [2 10 83]   [1 5 82]    [1 5 82]    [0 0 81]   [0 0 81]   [0 -20 77]
   1  [15 75 96]  [10 50 91]  [7 35 88]   [5 25 86]   [3 15 84]   [2 10 83]   [1 5 82]   [1 5 82]   [0 0 81]
   2  [16 85 97]  [13 65 94]  [10 50 91]  [9 45 90]   [5 25 86]   [3 15 84]   [2 10 83]  [1 5 82]   [1 5 82]
   3  [18 90 99]  [15 75 96]  [11 55 92]  [10 50 91]  [7 35 88]   [4 20 85]   [3 15 84]  [2 10 83]  [1 5 82]
   4  [19 95 100] [16 85 97]  [15 75 96]  [13 65 94]  [10 50 91]  [7 35 88]   [5 25 86]  [3 15 84]  [2 10 83]
   5  [19 95 100] [18 90 99]  [16 85 97]  [16 80 97]  [13 65 94]  [10 50 91]  [9 45 90]  [5 25 86]  [4 20 85]
   6  [20 100 0]  [19 95 100] [18 90 99]  [16 85 97]  [15 75 96]  [11 55 92]  [10 50 91] [7 35 88]  [5 25 86]
   7  [21 105 0]  [19 95 100] [19 95 100] [18 90 99]  [16 85 97]  [15 75 96]  [13 65 94] [10 50 91] [9 45 90]
   8  [23 115 0]  [20 100 0]  [19 95 100] [19 95 100] [18 90 99]  [16 80 97]  [15 75 96] [11 55 92] [10 50 91]
   9  [25 125 0]  [22 110 0]  [19 95 100] [19 95 100] [18 90 99]  [16 85 97]  [16 80 97] [13 65 94] [11 55 92]
   10 [26 145 0]  [26 130 0]  [20 100 0]  [20 100 0]  [19 95 100] [19 95 100] [18 90 99] [16 85 97] [16 80 97]])

(def transformed-map
  (let [chaos-vals (take 9 fate-table-values)
        rows (->> fate-table-values
                  (drop 9)
                  (partition 10))]
    (into (sorted-map)
          (for [[likelihood & entries] rows]
            [likelihood (->> entries
                             (interleave chaos-vals)
                             (partition 2)
                             (map vec)
                             (into (sorted-map)))]))))

(defn d100
  ([]
   (inc (rand-int 100)))
  ([chaos]
   (let [r1 (rand-int 10)
         r2 (rand-int 10)
         r (+ (* r1 10) r2)
         r (if (= r 0) 100 r)
         equal? (= r1 r2)
         random-event? (and equal? (<= r1 chaos))]
     [r random-event?])))

(defn roll-yes-no [likelihood chaos]
  (let [[lo thresh hi] (get-in transformed-map [likelihood chaos])
        [roll random-event?] (d100 chaos)]
    [[lo thresh hi]
     roll
     (cond
       (<= roll lo) :exceptional-yes
       (<= roll thresh) :yes
       (> roll hi) :exceptional-no
       :else :no)
     random-event?]))

(def event-focus-table
  [[1 7 "Remote event"]
   [8 28 "NPC action"]
   [29 35 "Introduce a new NPC"]
   [35 45 "Move toward a thread"]
   [46 52 "Move away from a thread"]
   [53 55 "Close a thread"]
   [56 67 "PC negative"]
   [68 75 "PC positive"]
   [76 83 "Ambiguous event"]
   [84 92 "NPC negative"]
   [93 100 "NPC positive"]])

(defn event-focus []
  (let [roll (d100)]
    (->> event-focus-table
         (drop-while (fn [[min_ max_ desc]]
                       (< max_ roll)))
         first
         last)))

(def event-actions
  ["Attainment" "Starting" "Neglect" "Fight" "Recruit" "Triumph"
   "Violate" "Oppose" "Malice" "Communicate" "Persecute" "Increase"
   "Decrease" "Abandon" "Gratify" "Inquire" "Antagonise" "Move"
   "Waste" "Truce" "Release" "Befriend" "Judge" "Desert" "Dominate"
   "Procrastinate" "Praise" "Separate" "Take" "Break" "Heal" "Delay"
   "Stop" "Lie" "Return" "Imitate" "Struggle" "Inform" "Bestow"
   "Postpone" "Expose" "Haggle" "Imprison" "Release" "Celebrate"
   "Develop" "Travel" "Block" "Harm" "Debase" "Overindulge" "Adjourn"
   "Adversity" "Kill" "Disrupt" "Usurp" "Create" "Betray" "Agree"
   "Abuse" "Oppress" "Inspect" "Ambush" "Spy" "Attach" "Carry" "Open"
   "Carelessness" "Ruin" "Extravagance" "Trick" "Arrive" "Propose"
   "Divide" "Refuse" "Mistrust" "Deceive" "Cruelty" "Intolerance"
   "Trust" "Excitement" "Activity" "Assist" "Care" "Negligence"
   "Passion" "Work hard" "Control" "Attract" "Failure" "Pursue"
   "Vengeance" "Proceedings" "Dispute" "Punish" "Guide" "Transform"
   "Overthrow" "Oppress" "Change"])

(def event-subjects
  ["Goals" "Dreams" "Environment" "Outside" "Inside" "Reality"
   "Allies" "Enemies" "Evil" "Good" "Emotions" "Opposition" "War"
   "Peace" "The innocent" "Love" "The spiritual" "The intellectual"
   "New ideas" "Joy" "Messages" "Energy" "Balance" "Tension"
   "Friendship" "The physical" "A project" "Pleasures" "Pain"
   "Possessions" "Benefits" "Plans" "Lies" "Expectations" "Legal
   matters" "Bureaucracy" "Business" "A path" "News" "Exterior
   factors" "Advice" "A plot" "Competition" "Prison" "Illness" "Food"
   "Attention" "Success" "Failure" "Travel" "Jealousy" "Dispute"
   "Home" "Investment" "Suffering" "Wishes" "Tactics" "Stalemate"
   "Randomness" "Misfortune" "Death" "Disruption" "Power" "A burden"
   "Intrigues" "Fears" "Ambush" "Rumor" "Wounds" "Extravagance" "A
   representative" "Adversities" "Opulence" "Liberty" "Military" "The
   mundane" "Trials" "Masses" "Vehicle" "Art" "Victory" "Dispute"
   "Riches" "Status quo" "Technology" "Hope" "Magic" "Illusions"
   "Portals" "Danger" "Weapons" "Animals" "Weather" "Elements"
   "Nature" "The public" "Leadership" "Fame" "Anger" "Information"])

(defn event-action [] (rand-nth event-actions))
(defn event-subject [] (rand-nth event-subjects))

(defn answer-question [question-str likelihood chaos]
  (let [[_ _ result random-event?] (roll-yes-no likelihood chaos)]
    (merge {:question question-str
            :answer result
            :random-event? random-event?}
           (when random-event?
             {:event-focus (event-focus)
              :event-action (event-action)
              :event-subject (event-subject)}))))

(answer-question "Is the road blocked ahead?" 5 9)
;;=>
'{:question "Is the road blocked ahead?",
  :answer :yes,
  :random-event? false}

(answer-question "Do I see another ship?" 1 3)
;;=>
'{:question "Do I see another ship?",
  :answer :no,
  :random-event? false}

(answer-question "Should I stop for the night?" 5 5)
;;=>
'{:question "Should I stop for the night?",
  :answer :exceptional-no,
  :random-event? true,
  :event-focus "Move toward a thread",
  :event-action "Decrease",
  :event-subject "Dispute"}

(defn handle-input [chaos input]
  (let [{:keys [question answer random-event?
                event-focus event-action
                event-subject]}
        (answer-question input 5 chaos)]
    (println "Question: " question)
    (println "  Answer: " (name answer))
    (when random-event?
      (println "Something happened!")
      (println "  Event focus: " event-focus)
      (println "Event subject: " event-subject)
      (println " Event action: " event-action))))

(defn -main []
  (loop [chaos 5]
    (print "gme> ")
    (flush)
    (let [input (s/trim (read-line))]
      (if-not (.startsWith input "quit")
        (do
          (handle-input chaos input)
          (recur chaos))))))

"
Things that can happen:
- start a game
  - define a character
  - start a scene (name)
    - ask a question
      - get answer
      - determine if extra event occurs
  - conclude a scene
    - update characters
    - update threads
- pause a game
- resume a game
- review a game
- end a game
"

;;(-main)

