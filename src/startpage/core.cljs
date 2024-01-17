(ns startpage.core
  (:require
    [cljs.reader]
    [reagent.dom :as rdom]
    [reagent.core :as r]
    [startpage.components :as c]
    [startpage.polys :as polys]
    ))


(defonce pages (r/atom (js->clj js/jspages :keywordize-keys true)))
(defonce query-params (r/atom nil))

(defn setstyle [prop value]
  (js/document.body.style.setProperty prop value))

(let [params (-> js/window.location js/URL. .-searchParams 
                   js/Object.fromEntries (js->clj :keywordize-keys true))
      color  (:color params)]
  (reset! query-params params)
  (when color
    (case color
      "red" (do
              (setstyle "--glow-color" "#F00")
              (setstyle "--glow-fill-color" "#200")
              (setstyle "--glow-hover-color" "#FC0")
              (setstyle "--calendar-color" "#A0A")
              (setstyle "--calendar-alt" "#606")
              )
      "yellow" (do
                 (setstyle "--glow-color" "#FF0")
                 (setstyle "--glow-fill-color" "#220")
                 (setstyle "--glow-hover-color" "#FFC")
                 (setstyle "--calendar-color" "#B70")
                 (setstyle "--calendar-alt" "#930")
                 )
      )))

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

              [c/carveout-box ympoly]

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
    (rdom/render [starter] root-el)
   ))

(js/window.addEventListener "resize" mount-root)

