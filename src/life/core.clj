(ns life.core
  (:use [clojure.contrib.combinatorics :only [selections cartesian-product]])
  (:import [javax.swing JPanel JFrame Timer]
           [java.awt Color]
           [java.awt.event ActionListener MouseListener MouseAdapter MouseEvent MouseMotionListener MouseMotionAdapter])
  (:gen-class))

(def matrix (remove #{[0 0]} (selections [-1 0 1] 2)))

(defn create-world [x y]
  (-> (cartesian-product (range x) (range y))
      (zipmap (repeat false))
      (with-meta {:x x :y y})))

(def world (atom (create-world 40 40)))

(defn cell [world position]
  (get world position))

(defn neighbours [world position]
  (map (partial cell world)
       (map (partial map + position) matrix)))

(defn live-neighbours [world position]
  (count (filter true? (neighbours world position))))

(defn update-state [world position]
  (let [[state live] ((juxt cell live-neighbours) world position)]
    (cond (and state (or (> 2 live) (> live 3))) false    ; under-population/overcrowding
          (and state (<= 2 live 3))              true     ; continue life
          (and (not state) (== live 3))          true)))  ; reproduction

(defn toggle-position [world position]
  (update-in world [position] not))

(defn update-world [world]
  (with-meta 
    (into {} (for [[position] world] [position (update-state world position)]))
    (meta world)))

(defn draw-world [graphics world]
  (doseq [[[x y] state] world]
    (let [x (* x 10) y (* y 10)]
      (doto graphics
        (.setColor Color/BLACK)
        (.fillRect x y 10 10)   ; border
        (.setColor (if state Color/GREEN Color/WHITE))
        (.fillRect x y 9 9))))) ; cell

(defn panel []
  (proxy [JPanel ActionListener] []
    (paintComponent [g] (proxy-super paintComponent g) (draw-world g @world))
    (actionPerformed [_] (swap! world update-world) (.repaint this))))

(defn frame []
  (let [{:keys [x y]} (meta @world)]
    (doto (JFrame. "Conway's Game of Life")
      (.setSize (+ 17 (* x 10)) (+ 39 (* y 10)))
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))))

(def painted (atom #{}))

(defn -main [& args]
  (let [frame (frame)
        panel (panel)
        timer (Timer. 100 panel)]
    (doto panel
      (.addMouseListener
       (proxy [MouseAdapter] []
         (mouseClicked [e]
           (let [x (int (/ (.getX e) 10))
                 y (int (/ (.getY e) 10))
                 position [x y]]
             (condp == (.getButton e)
               (MouseEvent/BUTTON1)
               (do (.stop timer)
                   (swap! world toggle-position position)
                   (.repaint panel))
               (MouseEvent/BUTTON3)
               (if (.isRunning timer)
                 (.stop timer)
                 (.start timer))
               (MouseEvent/BUTTON2)
               (let [{:keys [x y]} (meta @world)]
                 (.stop timer)
                 (swap! world (constantly (create-world x y)))
                 (.repaint panel)))))
         (mouseReleased [_]
           (swap! painted (constantly #{})))))
      (.addMouseMotionListener
       (proxy [MouseMotionAdapter] []
         (mouseDragged [e]
           (let [x (int (/ (.getX e) 10))
                 y (int (/ (.getY e) 10))
                 position [x y]]
             (.stop timer)
             (when-not (@painted position)
               (swap! world toggle-position position)
               (swap! painted conj position))
             (.repaint panel))))))
    (.setFocusable panel true)
    (.add frame panel)))
