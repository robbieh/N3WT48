(ns startpage.core
  (:require
    [cljs.reader]
    [reagent.dom :as rdom]
    [reagent.core :as r]
    [startpage.components :as c]
    [goog.string :refer [format]]
    [cljs-polys-etc.polys :as polys]))

(defonce pages (r/atom (js->clj js/jspages :keywordize-keys true)))
(defonce query-params (r/atom nil))

(defn setstyle [prop value]
  (js/document.body.style.setProperty prop value))


(defn set-colors [glow glow-fill glow-hover calendar calendar-alt]
  (setstyle "--glow-color" glow)
  (setstyle "--glow-fill-color" glow-fill)
  (setstyle "--glow-hover-color" glow-hover)
  (setstyle "--calendar-color" calendar)
  (setstyle "--calendar-alt" calendar-alt))

(defn set-hue [hue]
  (let [
        cal-hue    (-> hue (- 60) (mod 360 ))
        hover-hue  (-> hue (+ 10) (mod 360 ))
        glow       (format "hsl(%d,%d%%,%d%%)", hue 100 50)
        glow-fill  (format "hsl(%d,%d%%,%d%%)", hue 100 10)
        glow-hover (format "hsl(%d,%d%%,%d%%)", hover-hue 100 50)
        calendar   (format "hsl(%d,%d%%,%d%%)", cal-hue 100 50)
        cal-alt    (format "hsl(%d,%d%%,%d%%)", cal-hue 100 20)
        ]
    (set-colors glow glow-fill glow-hover calendar cal-alt)))

(let [params (-> js/window.location js/URL. .-searchParams 
                   js/Object.fromEntries (js->clj :keywordize-keys true))
      color  (:color params)
      hue    (:hue params)]
  (reset! query-params params)
  (when color
    (case color
      "red"         (set-hue 0)
      "orange"      (set-hue 30)
      "yellow"      (set-hue 60)
      "chartreuse"  (set-hue 90)
      "green"       (set-hue 120)
      "spring"      (set-hue 150)
      "cyan"        (set-hue 180)
      "azure"       (set-hue 210)
      "blue"        (set-hue 240)
      "indigo"      (set-hue 270)
      "magenta"     (set-hue 300)
      "rose"        (set-hue 330)))
  (when hue (set-hue (js/parseInt hue))))


(defn new-svg [w h e]
  [:svg {:width w :height h :viewBox (str "0 0 " w " " h ) } e])

(defn starter []
  (let [w    js/document.documentElement.clientWidth
        h    js/document.documentElement.clientHeight
        tall (if (> h w) true false)
        pageset (keyword (get @query-params :pages "default"))

        ymg  (c/yearmonth-gauge-calc {:month-width 20 
                                      :month-height 20 
                                      :day-sidelen 13 
                                      :border 5 
                                      :gap 2})
        date           (new js/Date)
        month          (inc (.getMonth date))
        ymwidth        (:width ymg)
        ympoly         (:poly ymg)
        ympoly         (polys/translate-poly ympoly (- w 10 ymwidth) 10)
        monthpolys     (:month-polys ymg)
        monthpolys     (polys/translate-polys monthpolys (- w 10 ymwidth) 10 )
        ruler-polys    (:ruler-polys ymg)
        ruler-polys    (polys/translate-polys ruler-polys (- w 10 ymwidth) 10 )
        indicator-poly (:indicator-poly ymg)
        indicator-poly (polys/translate-poly indicator-poly (- w 10 ymwidth) 10 )]

    (new-svg w h 
             [:<> 
              [:defs
               [:filter {:id "mainblur"}
                [:feGaussianBlur {:in "SourceGraphic" :stdDeviation 5 }]]
               [:filter {:id "smallblur"}
                [:feGaussianBlur {:in "SourceGraphic" :stdDeviation 2 }]]
               ]

              ;Putting a solid fill-color poly behind the main transparent poly eliminates Firefox's rendering bug
              ;that's why this is called twice - once for the flat poly, once for the glow poly
              [c/carveout-box false ympoly]
              [c/carveout-box true ympoly]

              [c/pagebar (get @pages pageset) tall]

              ;day ruler, indicator, and months
              (into [:<>] (for [p ruler-polys] 
                [:polyline {:class "calendar-outline" :points (polys/poly2path p)}]))
              [:polygon {:class "calendar-outline calendar-fill" 
                         :points (polys/poly2path indicator-poly) :filter "url(#mainblur)"}]
              [:polygon {:class "calendar-outline" 
                         :fill "#000" :points (polys/poly2path indicator-poly)}]
              (into [:<>] (for [p monthpolys]
                [:polygon {:class "calendar-outline" :fill "#000" :points (polys/poly2path p)}]))
              (into [:<>] (for [p (take month monthpolys)]
                [:polygon {:class "calendar" :points (polys/poly2path p)}]))

              ])))

(defn ^:dev/after-load mount-root []
   (let [root-el (.getElementById js/document "app")]
     (rdom/unmount-component-at-node root-el)
     (rdom/render [starter] root-el)))

(defn init []
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [starter] root-el)))

(js/window.addEventListener "resize" mount-root)

