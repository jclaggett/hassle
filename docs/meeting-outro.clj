;; # Questions & Answers

^{:nextjournal.clerk/visibility #{:hide}}
(render-net (let [a (input :notes) b (input :audience) c (node :presentation #{a b}) d (node :discussion c) e (output :thoughts d) f (output :code d)] (net 'meetup #{e f})))
