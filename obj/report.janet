(defn r/report
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
  (each [path line-no col-no thing] all-results
    (def subpath
      (if no-prefix
        path
        (string/slice path (length prefix))))
    (printf "# %d # %s +%d %s" i editor line-no subpath)
    (when (pos? (dec col-no))
      (prin (string/repeat " " (dec col-no))))
    (def lines (string/split "\n" thing))
    (if limit-lines
      (for i 0 (min (length lines) limit-lines)
        (print (get lines i)))
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

(defn r/report-by-path-and-thing
  [all-results opts]
  (def {:editor editor
        :includes includes
        :n-paths n-paths
        :no-prefix no-prefix
        :prefix prefix
        :n-hit-paths n-hit-paths
        :start-clock start-clock} opts)
  (var i 1)
  (each group-by-path (partition-by |(get $ 0) all-results)
    (def path (get-in group-by-path [0 0]))
    (def subpath
      (if no-prefix
        path
        (string/slice path (length prefix))))
    (printf "# %d # %s %s" i editor subpath)
    (print)
    (each group-by-thing (->> (sort-by |(get $ 3) group-by-path)
                              (partition-by |(get $ 3)))
      (def thing (get-in group-by-thing [0 3]))
      (prinf "%s # " thing)
      (def line-nos
        (->> (sort-by |(get $ 1) group-by-thing)
             (map |(string (get $ 1)))))
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

