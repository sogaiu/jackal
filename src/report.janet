(defn report
  [all-results opts]
  (def {:editor editor
        :includes includes
        :n-paths n-paths
        :no-prefix no-prefix
        :prefix prefix
        :n-hit-paths n-hit-paths
        :limit-lines limit-lines
        :start-clock start-clock} opts)
  (var i 1)
  (each {:path path :line line-no :col col-no
         :thing thing :matched matched} all-results
    (def subpath (if no-prefix
                   path
                   (string/slice path (length prefix))))
    (printf "# %d" i)
    (when matched
      (printf "# %s" matched))
    (printf "# %s +%d %s" editor line-no subpath)
    (when (pos? (dec col-no))
      (prin (string/repeat " " (dec col-no))))
    (def lines (string/split "\n" thing))
    (if limit-lines
      (for j 0 (min (length lines) limit-lines)
        (print (get lines j)))
      (print thing))
    (print)
    (++ i))
  #
  (printf "# search space: %n" includes)
  (printf `# common prefix: "%s"` prefix)
  (printf "# files searched: %d" n-paths)
  (printf "# files analyzed: %d" n-hit-paths)
  (printf "# number of results: %d" (length all-results))
  (when start-clock
    (printf "# processing time: %.02f seconds"
            (- (os/clock) start-clock))))

(defn report-by-path-and-thing
  [all-results opts]
  (def {:editor editor
        :includes includes
        :n-paths n-paths
        :no-prefix no-prefix
        :prefix prefix
        :n-hit-paths n-hit-paths
        :start-clock start-clock} opts)
  (var i 1)
  (each group-by-path (partition-by |(get $ :path) all-results)
    (def path (get-in group-by-path [0 :path]))
    (def subpath
      (if no-prefix
        path
        (string/slice path (length prefix))))
    (printf "# %d # %s %s" i editor subpath)
    (print)
    (each group-by-thing (->> (sort-by |(get $ :thing) group-by-path)
                              (partition-by |(get $ :thing)))
      (def thing (get-in group-by-thing [0 :thing]))
      (prinf "%s # " thing)
      (def line-nos
        (->> (sort-by |(get $ :line) group-by-thing)
             (map |(string (get $ :line)))))
      (print (string/join line-nos " ")))
    (print)
    (++ i))
  (printf "# search space: %n" includes)
  (printf `# common prefix: "%s"` prefix)
  (printf "# files searched: %d" n-paths)
  (printf "# files analyzed: %d" n-hit-paths)
  (printf "# number of results: %d" (length all-results))
  (when start-clock
    (printf "# processing time: %.02f seconds"
            (- (os/clock) start-clock))))

