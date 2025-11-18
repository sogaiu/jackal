(def sep
  (let [os (os/which)]
    (if (or (= :windows os) (= :mingw os)) `\` "/")))

(defn find-files
  [dir &opt pred]
  (default pred identity)
  (def paths @[])
  (defn helper
    [a-dir]
    (each path (os/dir a-dir)
      (def sub-path
        (string a-dir sep path))
      (case (os/stat sub-path :mode)
        :directory
        (when (not= path ".git")
          (when (not (os/stat (string sub-path sep ".gitrepo")))
            (helper sub-path)))
        #
        :file
        (when (pred sub-path)
          (array/push paths sub-path)))))
  (helper dir)
  paths)

(comment

  (find-files "." |(string/has-suffix? ".janet" $))

  )

(defn clean-end-of-path
  [path sep]
  (when (one? (length path))
    (break path))
  (if (string/has-suffix? sep path)
    (string/slice path 0 -2)
    path))

(comment

  (clean-end-of-path "hello/" "/")
  # =>
  "hello"

  (clean-end-of-path "/" "/")
  # =>
  "/"

  )

(defn has-janet-shebang?
  [path]
  (with [f (file/open path)]
    (def first-line (file/read f :line))
    (when first-line
      # some .js files has very long first lines and can contain
      # a lot of strings...
      (and (string/find "bin/env" first-line)
           (string/find "janet" first-line)))))

(defn looks-like-janet?
  [path]
  (or (string/has-suffix? ".janet" path)
      (has-janet-shebang? path)))

(defn collect-paths
  [includes &opt pred]
  (default pred identity)
  (def filepaths @[])
  # collect file and directory paths
  (each thing includes
    (def apath (clean-end-of-path thing sep))
    (def mode (os/stat apath :mode))
    # XXX: should :link be supported?
    (cond
      (= :file mode)
      (array/push filepaths apath)
      #
      (= :directory mode)
      (array/concat filepaths (find-files apath pred))
      #
      (do
        (eprintf "No such file or not an ordinary file or directory: %s"
                 apath)
        (os/exit 1))))
  #
  filepaths)

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

(defn common-prefix
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

  (common-prefix ["ab" "abc" "abcd"])
  # =>
  "ab"

  (common-prefix ["/home/alice/src/janet/src"
                  "/home/alice/src/janet/src/boot"
                  "/home/alice/src/janet"
                  "/home/alice/src/janet/src/boot/boot.janet"])
  # =>
  "/home/alice/src/janet"

  (common-prefix ["ab" "abc" "" "abcd"])
  # =>
  ""

  (common-prefix ["a" "b" "c"])
  # =>
  ""

  )

(defn search-paths
  [query-fn opts]
  (def {:name name :paths src-paths} opts)
  #
  (def all-results @[])
  (def hit-paths @[])
  (each path src-paths
    (def src (slurp path))
    (when (pos? (length src))
      (when (or (not name)
                (string/find name src))
        (array/push hit-paths path)
        (def results
          (try
            (query-fn src opts)
            ([e]
              (eprintf "search failed for: %s" path))))
        (when (and results (not (empty? results)))
          # item can have a variable number of elements
          (each item results
            (array/push all-results [path ;item]))))))
  #
  [all-results hit-paths])

