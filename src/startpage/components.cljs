(ns startpage.components
  (:require 
    [cljs-polys-etc.polys :as polys]))


(defn datesplode 
  "Get today's date info in a friendly hashmap."
  []
  (let [date                 (new js/Date)
        year                 (.getFullYear date)
        month                (inc (.getMonth date))
        day                  (.getDate date)
        dow                  (.getDay date)
        first-dow            (.getDay (new js/Date year (dec month) 1))
        days-in-month        (.getDate (new js/Date year month 0))]
    {:year year :month month :day day 
     :dow dow :first-dow first-dow :days-in-month days-in-month}))
;(datesplode)

(defn chevron-poly 
  "Returns a chevron shape for drawing the calendar months."
  [w h indent]
  (let [
        mx (* 0.5 w)
        tl [0 0]
        tm [mx indent]
        tr [w 0]
        br [w h]
        bm [mx (+ h indent)]
        bl [0 h]
        ]
    [tl tm tr br bm bl]))

(defn equilateral-triangle-poly 
  "Returns an equilateral triangle."
  [sidelen] 
  (let [
        ;sidelen        (equilateral-sidelen height)
        inradius       (polys/equilateral-inradius sidelen)
        circumradius   (* 2 inradius)
        p1             (polys/polar-to-cartesian 0 0 circumradius 0)
        p2             (polys/polar-to-cartesian 0 0 circumradius 120 )
        p3             (polys/polar-to-cartesian 0 0 circumradius 240 )
        ]
    [p1 p2 p3]))

;TODO: unused?
(defn poly-line [poly poly-width number gap]
  (let [xs (mapv #(* % (+ poly-width gap)) (range number))]
    (mapv #(polys/translate-poly poly % 0) xs)))

(defn ruler-poly 
  "Return a poly made of ruler-like lines.
  Pattern-vec contains numbers describing the relative length of each mark."
  [max-height max-value spacing pattern-vec]
  (for [[bar i] (map #(vector %1 %2) pattern-vec (range ))
        :let [y (* i  spacing)
              x (* max-height (/ bar max-value))
              ]]
    [[0 y] [x y]]))

(defn yearmonth-gauge-calc 
  "Returns a hashmap with data required to draw the year-month gauge."
  [{:keys [month-width month-height day-sidelen border gap]}]
  (let [x 0 y 0 ;TODO: refactor this out
        {:keys [day first-dow]} (datesplode)

        day-width            20
        ruler-spacing        6
        day-height           (* 31 ruler-spacing)
        day-indicator-y      (* (dec day) ruler-spacing)
        month-height-chevron (+ month-height (* 0.1 month-height))
        month-height-total   (+ (* 12 month-height-chevron) (* 11 gap))
        ; day-height-tall      (+ (* 17 day-sidelen) (* 16 gap))
        ; day-height-short     (+ (* 15 day-sidelen) (* 14 gap))

        tr [(+ x month-width day-width day-width border border border border ) y]
        ;tm [(+ x day-width day-width border border) y]
        ;tl [x (+ y day-sidelen day-sidelen border)]
        tl [x y]
        bl [x (+ y day-height border border)]
        ;bm [(+ x day-width day-width border border) (+ y day-height-tall border border)]
        bn [(+ x day-width day-width border border) (+ y month-height-total)]
        br [(+ x month-width day-width day-width border border border border ) (+ y month-height-total)]
        month-x (+ x day-width day-width border border border border )
        month-y (+ y border border)
        month-polys (for [i (range 0 12)]
                      (polys/translate-poly
                        (chevron-poly month-width month-height gap)
                        month-x
                        (+ month-y (* i (+ gap month-height)))))

        ruler-pattern (->> (range 1 31)
                       (map #(mod (+ first-dow (dec %)) 7))
                       (map #(if (or (= 0 %) (= 6 %) ) 3 1))
                       )
        ruler-polys (-> (ruler-poly day-width 3 ruler-spacing ruler-pattern)
                       (polys/translate-polys (+ x border) (+ y day-sidelen border) ))
        indicator-poly (-> (equilateral-triangle-poly day-width)
                           (polys/rotate-poly 180)
                           (polys/translate-poly (+ x day-width day-width ) 
                                           (+ y day-indicator-y day-sidelen border)))
        ]

    {:poly [tr #_tm tl bl #_bm bn br]
     :width (+ x month-width day-width day-width border border border border)
     :height (+ y month-height-total border border)
     :month-polys month-polys
     :ruler-polys ruler-polys
     :indicator-poly indicator-poly
     }))

(defn carveout-box 
  "Returns poly for the main outline of the SVG. A rectangle with carve-outs.
  blur? - when false, returns the poly with fill and no blur
        - when true, returns the poly outline with blur
  Use together to avoid Firefox rendering bugs."
  [blur? & carveout-polys]
  (let [w js/document.documentElement.clientWidth
        h js/document.documentElement.clientHeight
        l 10
        r (- w 10)
        t 10
        b (- h 10)
        nw [l t]
        ne [r t]
        se [r b]
        sw [l b]

        frame [nw ne se sw]
        carveout [[100 0] [400 0] [380 50] [120 50]]
        ;diff (difference frame carveout)
        diff (reduce polys/difference frame (conj carveout-polys carveout))
        pstr (apply str (interpose " " (flatten diff)))
        ]
    ;(cljs.pprint/pprint carveout-polys)
    ;(cljs.pprint/pprint (conj carveout-polys carveout))

    (if blur?
      [:<>
       [:polygon {:points pstr :class "glow transparent" :filter "url(#mainblur)" :stroke-width 2}]
       [:polygon {:points pstr :class "glow transparent" :stroke-width 2}]
       ]
      [:<>
       [:polygon {:points pstr :class "glow-fill" }]
       ]
      )))

(defn cyber-hexagon-poly 
  "Returns a poly hexagon with three long sides and three short sides, alternating.
  Looks like a triangle with blunted points."
  [height offset] 
  (let [
        tipsize        (* 0.1 height)
        sqrt3          (js/Math.sqrt 3)
        sidelen        (/ (* 2 height) sqrt3)
        inradius       (/ (* sqrt3 sidelen) 6)
        circumradius   (* 2 inradius)
        circumradius-  (- circumradius tipsize)
        p1             (polys/polar-to-cartesian 0 0 circumradius- (- offset))
        p2             (polys/polar-to-cartesian 0 0 circumradius- offset)
        p3             (polys/polar-to-cartesian 0 0 circumradius- (- 120 offset))
        p4             (polys/polar-to-cartesian 0 0 circumradius- (+ 120 offset))
        p5             (polys/polar-to-cartesian 0 0 circumradius- (- 240 offset))
        p6             (polys/polar-to-cartesian 0 0 circumradius- (+ 240 offset))
        ]
    [p1 p2 p3 p4 p5 p6]))

(defn page-triangle 
  "Returns an SVG group element using the hexagon outline.
  svgname - externally loaded svg that will be clipped into the triangle.
  url - for when the triangle is clicked."
  [svgname url x y degrees]
  (let [t1 (cyber-hexagon-poly 100 5)
        t2 (cyber-hexagon-poly 90 5)
        icon-url (str "img/" svgname ".svg")
        t1 (-> t1 
               (polys/rotate-poly degrees) 
               (polys/translate-poly x y))
        t2 (-> t2 
               (polys/rotate-poly degrees) 
               (polys/translate-poly x y))
        t1pts  (polys/poly2path t1)
        t2pts  (polys/poly2path t2)
        ]
  [:g { :class "page-triangle" }
   [:a {:href url :target "_top"}
     [:defs [:clipPath {:id (str "clip-" svgname)}
       [:polygon {:points t2pts }] ]]
     ;[:circle {:cx x :cy y :r 100 :fill :green}]
     [:polygon {:class "hover-target glow glow-fill" :points t1pts :filter "url(#mainblur)"}]
     [:polygon {:class "glow" :points t2pts :fill "#000"}]
     [:image {:x (- x 50) :y (- y 50) :width 100 :height 100 
             :clip-path (str "url(#clip-" svgname ")") 
             :href icon-url}]
     [:polygon {:class "glow" :points t2pts :fill "#0000"}]
  ]]))

;TODO: unused?
(defn pagebar [pages tall?]  
  (let [p      pages
        c      (count p)
        ymid   (* 0.5 js/document.documentElement.clientHeight)
        xmid   (* 0.5 js/document.documentElement.clientWidth)
        space  (* (dec c) 100);(* c 75)
        xstart (- xmid (* 0.5 space))
        ystart (- ymid (* 0.5 space))
        plist  (if tall?
                  (map #(conj %1 %2 %3) p (cycle [0 180]) (range)) ;add orientation and index to page info
                  (map #(conj %1 %2 %3) p (cycle [90 270]) (range)) ;add orientation and index to page info
                 )
        ] 

    [:<>
     ;[:line {:stroke "#F0F" :x1 0 :y1 ymid :x2 js/document.documentElement.clientWidth :y2 ymid}]
     ;[:line {:stroke "#F0F" :x1 xmid :y1 0 :x2 xmid :y2 js/document.documentElement.clientHeight}]
     (for [[pname purl prot px] plist] 
       (if tall?
         ^{:key (str "page-" pname)} [page-triangle pname purl xmid (+ ystart (* 100 px)) prot]
         ^{:key (str "page-" pname)} [page-triangle pname purl (+ xstart (* 100 px)) ymid prot]
         )
       )]
    ))

