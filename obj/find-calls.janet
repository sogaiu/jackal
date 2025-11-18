(import ./jipper :prefix "")

########################################################################

# XXX: just doing defn, defn-, varfn for the moment
(defn fc/find-caller
  [zloc]
  (var cur-zloc zloc)
  (var name nil)
  (while (def parent-zloc (j/up cur-zloc))
    (set cur-zloc parent-zloc)
    (def parent-node-str (j/gen (j/node parent-zloc)))
    (def parsed
      (try (parse parent-node-str)
        ([e] (eprintf "failed to parse: %s" parent-node-str))))
    (when (not parsed)
      (set cur-zloc nil)
      (break))
    #
    (def head (first parsed))
    (when (get {'defn true 'defn- true 'varfn true} head)
      (set name (get parsed 1))
      (break)))
  #
  (when name
    (def node (j/node cur-zloc))
    (def {:bl bl} (get node 1))
    [bl name]))

(defn fc/find-calls
  [src &opt opts]
  (default opts {})
  (def {:pred pred} opts)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $) [:tuple]
                             $)))
    (def node (j/node next-zloc))
    (def raw-code-str (j/gen node))
    (def parsed
      (try (parse raw-code-str)
        ([e] (eprintf "failed to parse: %s" raw-code-str))))
    (when (and parsed
               (if-not pred true (pred parsed)))
      (when-let [leader (first parsed)]
        (when (symbol? leader)
          (def {:bl bl :bc bc} (get node 1))
          (array/push results [bl bc leader]))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (fc/find-calls
    ``
    (defn fly [] :flap)

    (defn smile
      [y]
      (pp y))
    ``)
  # =>
  @[[1 1 'defn]
    [3 1 'defn]
    [5 3 'pp]]

  (fc/find-calls
    ``
    (defn hello
      [x]
      (pp x)
      (print "hi")
      (if true
        (pp [:x x])
        (print "oh no")))
    ``)
  # =>
  @[[1 1 'defn]
    [3 3 'pp]
    [4 3 'print]
    [5 3 'if]
    [6 5 'pp]
    [7 5 'print]]

  )

# XXX: just doing defn, defn-, varfn for the moment
(defn fc/find-caller
  [zloc]
  (var cur-zloc zloc)
  (var name nil)
  (while (def parent-zloc (j/up cur-zloc))
    (set cur-zloc parent-zloc)
    (def parent-node-str (j/gen (j/node parent-zloc)))
    (def parsed
      (try (parse parent-node-str)
        ([e] (eprintf "failed to parse: %s" parent-node-str))))
    (when (not parsed)
      (set cur-zloc nil)
      (break))
    #
    (def head (first parsed))
    (when (get {'defn true 'defn- true 'varfn true} head)
      (set name (get parsed 1))
      (break)))
  #
  (when name
    (def node (j/node cur-zloc))
    (def {:bl bl :bc bc} (get node 1))
    [bl bc name]))

(defn fc/find-callers-of
  [src opts]
  (def {:name name :pred pred :exact-match exact-match} opts)
  (default pred identity)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (def matcher (if exact-match = string/has-suffix?))
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $) [:symbol _ value]
                             (when (matcher name value)
                               $))))
    (def parent-zloc (j/up next-zloc))
    (when (= :tuple (get (j/node parent-zloc) 0))
      (def node (j/node parent-zloc))
      (def raw-code-str (j/gen node))
      (def parsed
        (try (parse raw-code-str)
          ([e] (eprintf "failed to parse: %s" raw-code-str))))
      (when (and parsed (pred parsed))
        # ensure first non-trivial element of the tuple ends in `name`
        (when (matcher name (string (first parsed)))
          (def caller (fc/find-caller parent-zloc))
          (when caller
            (def [line-no col-no caller-name] caller)
            (array/push results [line-no col-no caller-name])))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (fc/find-callers-of
    ``
    (defn fly [] :flap)

    (defn smile
      [y]
      (pp y))
    ``
    {:name "pp"})
  # =>
  @[[3 1 'smile]]

  (fc/find-callers-of
    ``
    (defn hello
      [x]
      (pp x)
      (print "hi")
      (if true
        (pp [:x x])
        (print "oh no")))
    ``
    {:name "pp"})
  # =>
  @[[1 1 'hello]
    [1 1 'hello]]

  )

(defn fc/find-calls-to
  [src opts]
  (def {:name name :pred pred :exact-match exact-match} opts)
  (default pred identity)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (def matcher (if exact-match = string/has-suffix?))
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $) [:symbol _ value]
                             (when (matcher name value)
                               $))))
    (def parent-zloc (j/up next-zloc))
    (when (= :tuple (get (j/node parent-zloc) 0))
      (def node (j/node parent-zloc))
      (def raw-code-str (j/gen node))
      (def parsed
        (try (parse raw-code-str)
          ([e] (eprintf "failed to parse: %s" raw-code-str))))
      (when (and parsed (pred parsed))
        # ensure first non-trivial element of the tuple ends in `name`
        (when (matcher name (string (first parsed)))
          (def {:bc bc :bl bl} (get node 1))
          (array/push results [bl bc raw-code-str]))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (fc/find-calls-to
    ``
    (defn hello
      [x]
      (pp x)
      (print "hi")
      (if true
        (pp [:x x])
        (print "oh no")))
    ``
    {:name "pp"})
  # =>
  @[[3 3 "(pp x)"]
    [6 5 "(pp [:x x])"]]

  )

