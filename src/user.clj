(require '[net.n01se.hassle.hw :as hw])
(require '[nextjournal.clerk :as clerk])

(clerk/serve! {:browse? true})
(clerk/serve! {:watch-paths ["notebooks" "src"]})
