# XXX: neat but possibly not great when the number of elements of
#      byte-vals is large?
(defn common-prefix
  [byte-vals]
  # compare the corresponding bytes of all of the byte values
  (def compares (map |(= ;$&) ;byte-vals))
  (when (empty? compares)
    (break ""))
  #
  (def last-index
    (if-let [index (find-index false? compares)]
      index
      (length compares)))
  #
  (string/slice (first byte-vals) 0 last-index))

(defn p/common-prefix
  [byte-vals]
  (def prefix (get byte-vals 0))
  # track right end of candidate prefix
  (var p-idx (length prefix))
  #
  (for i 1 (length byte-vals)
    (def cur-val (get byte-vals i))
    (when (empty? cur-val)
      (set p-idx 0)
      (break))
    #
    (set p-idx (min p-idx (length cur-val)))
    (var max-same-idx -1)
    (loop [j :range [0 p-idx]]
      (when (not= (get prefix j) (get cur-val j))
        (set p-idx (inc max-same-idx))
        (break))
      #
      (set max-same-idx j)))
  #
  (string/slice prefix 0 p-idx))

(comment

  (p/common-prefix ["ab" "abc" "abcd"])
  # =>
  "ab"

  (p/common-prefix ["/home/alice/src/janet/src"
                  "/home/alice/src/janet/src/boot"
                  "/home/alice/src/janet"
                  "/home/alice/src/janet/src/boot/boot.janet"])
  # =>
  "/home/alice/src/janet"

  (p/common-prefix ["ab" "abc" "" "abcd"])
  # =>
  ""

  (p/common-prefix ["a" "b" "c"])
  # =>
  ""

  )

