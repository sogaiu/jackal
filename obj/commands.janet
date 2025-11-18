(import ./find-calls :prefix "")
(import ./search :prefix "")
(import ./report :prefix "")

########################################################################

(defn c/search-and-dump
  [opts]
  (def {:query-fn query-fn} opts)
  #
  (def [all-results _] (s/search-paths query-fn opts))
  # output could be done via (printf "%j" all-results), but the
  # resulting output is harder to read and manipulate
  (print "[")
  (when (not (empty? all-results))
    (def fmt (string "["
                     (-> (length (get all-results 0))
                         (array/new-filled "%n")
                         (string/join " "))
                     "]"))
    (each r all-results (printf fmt ;r)))
  (print "]\n"))

(defn c/search-and-report
  [opts]
  (def {:includes includes
        :query-fn query-fn :paths paths
        :report report
        :no-prefix no-prefix
        :limit-lines limit-lines
        :editor editor
        :stop-watch stop-watch} opts)
  #
  (def start-clock (when stop-watch (os/clock)))
  (printf "# searching space: %n ..." includes)
  (def [all-results hit-paths] (s/search-paths query-fn opts))
  (print)
  (def prefix
    (if (<= 0 (length paths) 1)
      ""
      (s/common-prefix paths)))
  (report all-results {:editor editor
                       :includes includes
                       :n-paths (length paths)
                       :n-hit-paths (length hit-paths)
                       :prefix prefix
                       :no-prefix no-prefix
                       :limit-lines limit-lines
                       :start-clock start-clock}))

########################################################################


(defn c/all-calls
  [opts]
  (def {:default-paths default-paths
        :pred pred
        :editor editor
        :no-prefix no-prefix
        :stop-watch stop-watch
        :rest the-args} opts)
  #
  (def includes
    (if (= 0 (length the-args))
      default-paths
      the-args))
  # find .janet files
  (def src-filepaths
    (s/collect-paths includes s/looks-like-janet?))
  #
  (when (get opts :dump)
    (c/search-and-dump {:query-fn fc/find-calls
                      :paths src-filepaths})
    (break))
  # search the paths
  (c/search-and-report {:includes includes
                      :query-fn fc/find-calls
                      :paths src-filepaths :pred pred
                      :report r/report-by-path-and-thing
                      :no-prefix no-prefix
                      :editor editor
                      :stop-watch stop-watch}))

(defn c/who-calls
  [opts]
  (def {:default-paths default-paths
        :pred pred
        :exact-match exact-match
        :editor editor
        :no-prefix no-prefix
        :stop-watch stop-watch
        :limit-lines limit-lines
        :rest the-args} opts)
  #
  (def name (get the-args 0))
  (array/remove the-args 0)
  #
  (def includes
    (if (= 0 (length the-args))
      default-paths
      the-args))
  # find .janet files
  (def src-filepaths
    (s/collect-paths includes s/looks-like-janet?))
  #
  (when (get opts :dump)
    (c/search-and-dump {:query-fn fc/find-callers-of
                      :paths src-filepaths
                      :name name :pred pred :exact-match exact-match})
    (break))
  # search the paths
  (c/search-and-report {:includes includes
                      :query-fn fc/find-callers-of
                      :paths src-filepaths
                      :name name :pred pred :exact-match exact-match
                      :report r/report
                      :no-prefix no-prefix
                      :limit-lines limit-lines
                      :editor editor
                      :stop-watch stop-watch}))

(defn c/calls-to
  [opts]
  (def {:default-paths default-paths
        :pred pred
        :exact-match exact-match
        :editor editor
        :no-prefix no-prefix
        :stop-watch stop-watch
        :limit-lines limit-lines
        :rest the-args} opts)
  #
  (def name (get the-args 0))
  (array/remove the-args 0)
  #
  (def includes
    (if (= 0 (length the-args))
      default-paths
      the-args))
  # find .janet files
  (def src-filepaths
    (s/collect-paths includes s/looks-like-janet?))
  #
  (when (get opts :dump)
    (c/search-and-dump {:query-fn fc/find-calls-to
                      :paths src-filepaths
                      :name name :pred pred :exact-match exact-match})
    (break))
  # search the paths
  (c/search-and-report {:includes includes
                      :query-fn fc/find-calls-to
                      :paths src-filepaths
                      :name name :pred pred :exact-match exact-match
                      :report r/report
                      :no-prefix no-prefix
                      :limit-lines limit-lines
                      :editor editor
                      :stop-watch stop-watch}))

