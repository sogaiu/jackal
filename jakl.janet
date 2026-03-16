#! /usr/bin/env janet

(comment import ./args :prefix "")
(defn a/parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break {:show-help true}))
  #
  (when (or (not head) (= head "-v") (= head "--version"))
    (break {:show-version true}))
  #
  (array/remove the-args 0)
  #
  (def [opts cmd]
    (if-not (and (string/has-prefix? "{" head)
                 (string/has-suffix? "}" head))
      [@{} head]
      (let [parsed
            (try (parse (string "@" head))
              ([e] (eprint e)
                   (errorf "failed to parse options: %n" head)))]
        (assertf (and parsed (table? parsed))
                 "expected table but found: %s" (type parsed))
        (def opts parsed)
        (def new-head (get the-args 0))
        (array/remove the-args 0)
        (assertf new-head "expected a command but found none: %n" args)
        [opts new-head])))
  #
  (when (nil? (get opts :pred))
    (put opts :pred identity))
  #
  (when (nil? (get opts :no-prefix))
    (put opts :no-prefix true))
  #
  (when (nil? (get opts :stop-watch))
    (put opts :stop-watch true))
  #
  (when (nil? (get opts :exact-match))
    (put opts :exact-match false))
  #
  (def default-paths
    (if-let [conf-file (string (os/cwd) "/.jakl.conf")
             _ (= :file (os/stat conf-file :mode))]
      (->> (slurp conf-file)
           (string/split "\n")
           (map |(string/trim $))
           (filter |(and (not (empty? $))
                         (not (string/has-prefix? "#" $)))))
      ["."]))
  #
  (def editor (os/getenv "VISUAL" (os/getenv "EDITOR" "emacs")))
  #
  (merge opts
         {:command cmd
          :default-paths default-paths
          :rest the-args
          :editor editor}))


(comment import ./commands :prefix "")
(comment import ./empathy :prefix "")
(defn em/path-join
  [& parts]
  (def sep
    (if-let [sep (dyn :path-fs-sep)]
      sep
      (if (let [osw (os/which)]
            (or (= :windows osw) (= :mingw osw)))
        `\`
        "/")))
  #
  (string/join parts sep))

(comment

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (em/path-join "/tmp" "test.txt")))
  # =>
  "/tmp/test.txt"

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (em/path-join "/tmp" "foo" "test.txt")))
  # =>
  "/tmp/foo/test.txt"

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep `\`)
      (em/path-join "C:" "windows" "system32")))
  # =>
  `C:\windows\system32`

  )

(defn em/make-itemizer
  [& paths]
  (def todo-paths (reverse paths)) # pop used to process from end
  (def seen? @{})
  #
  (coro
    (while (def p (array/pop todo-paths))
      (def [ok? value] (protect (os/realpath p)))
      (when (and ok? (not (get seen? value)))
        (put seen? value true)
        (yield p)
        (when (= :directory (os/stat p :mode))
          (each subp (reverse (os/dir p))
            (array/push todo-paths (em/path-join p subp))))))))

(comment

  (def it (em/make-itemizer (dyn :syspath) "/etc/fonts"))

  (each p it (pp p))

  )

(comment

  (do
    (var res nil)
    (each p (em/make-itemizer "bundle")
      (when (string/has-suffix? "info.jdn" p)
        (set res true)
        (break)))
    res)
  # =>
  true

  )

(defn em/itemize
  [& paths]
  (def it (em/make-itemizer ;paths))
  #
  (seq [p :in it] p))

(comment

  (em/itemize (em/path-join (os/getenv "HOME") ".config"))

  )

(comment

  (do
    (var res nil)
    (each p (em/itemize ".")
      (when (string/has-suffix? "empathy.janet" p)
        (set res true)
        (break)))
    res)
  # =>
  true

  )


(comment import ./find-calls :prefix "")
(comment import ./jipper :prefix "")
(comment import ./helpers :prefix "")
# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn j/h/butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (j/h/butlast @[:a :b :c])
  # =>
  @[:a :b]

  (j/h/butlast [:a])
  # =>
  []

  )

(defn j/h/rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (j/h/rest [:a :b :c])
  # =>
  [:b :c]

  (j/h/rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn j/h/tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (j/h/tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (j/h/tuple-push nil :a)
  # =>
  [:a]

  (j/h/tuple-push @[] :a)
  # =>
  [:a]

  )

(defn j/h/to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (j/h/to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (j/h/to-entries {})
  # =>
  @[]

  (j/h/to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (j/h/to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn j/h/first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (j/h/rest xs) xs]))

(comment

  (j/h/first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (j/h/first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (j/h/first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (j/h/first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )


(comment import ./location :prefix "")
# bl - begin line
# bc - begin column
# el - end line
# ec - end column
(defn j/l/make-attrs
  [& items]
  (zipcoll [:bl :bc :el :ec]
           items))

(defn j/l/atom-node
  [node-type peg-form]
  ~(cmt (capture (sequence (line) (column)
                           ,peg-form
                           (line) (column)))
        ,|[node-type (j/l/make-attrs ;(slice $& 0 -2)) (last $&)]))

(defn j/l/reader-macro-node
  [node-type sigil]
  ~(cmt (capture (sequence (line) (column)
                           ,sigil
                           (any :non-form)
                           :form
                           (line) (column)))
        ,|[node-type (j/l/make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
           ;(slice $& 2 -4)]))

(defn j/l/collection-node
  [node-type open-delim close-delim]
  ~(cmt
     (capture
       (sequence
         (line) (column)
         ,open-delim
         (any :input)
         (choice ,close-delim
                 (error
                   (replace (sequence (line) (column))
                            ,|(string/format
                                "line: %p column: %p missing %p for %p"
                                $0 $1 close-delim node-type))))
         (line) (column)))
     ,|[node-type (j/l/make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
        ;(slice $& 2 -4)]))

(def j/l/loc-grammar
  ~@{:main (sequence (line) (column)
                     (some :input)
                     (line) (column))
     #
     :input (choice :non-form
                    :form)
     #
     :non-form (choice :whitespace
                       :comment)
     #
     :whitespace ,(j/l/atom-node :whitespace
                             '(choice (some (set " \0\f\t\v"))
                                      (choice "\r\n"
                                              "\r"
                                              "\n")))
     # :whitespace
     # (cmt (capture (sequence (line) (column)
     #                         (choice (some (set " \0\f\t\v"))
     #                                 (choice "\r\n"
     #                                         "\r"
     #                                         "\n"))
     #                         (line) (column)))
     #      ,|[:whitespace (make-attrs ;(slice $& 0 -2)) (last $&)])
     #
     :comment ,(j/l/atom-node :comment
                          '(sequence "#"
                                     (any (if-not (set "\r\n") 1))))
     #
     :form (choice # reader macros
                   :fn
                   :quasiquote
                   :quote
                   :splice
                   :unquote
                   # collections
                   :array
                   :bracket-array
                   :tuple
                   :bracket-tuple
                   :table
                   :struct
                   # atoms
                   :number
                   :constant
                   :buffer
                   :string
                   :long-buffer
                   :long-string
                   :keyword
                   :symbol)
     #
     :fn ,(j/l/reader-macro-node :fn "|")
     # :fn (cmt (capture (sequence (line) (column)
     #                             "|"
     #                             (any :non-form)
     #                             :form
     #                             (line) (column)))
     #          ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #             ;(slice $& 2 -4)])
     #
     :quasiquote ,(j/l/reader-macro-node :quasiquote "~")
     #
     :quote ,(j/l/reader-macro-node :quote "'")
     #
     :splice ,(j/l/reader-macro-node :splice ";")
     #
     :unquote ,(j/l/reader-macro-node :unquote ",")
     #
     :array ,(j/l/collection-node :array "@(" ")")
     # :array
     # (cmt
     #   (capture
     #     (sequence
     #       (line) (column)
     #       "@("
     #       (any :input)
     #       (choice ")"
     #               (error
     #                 (replace (sequence (line) (column))
     #                          ,|(string/format
     #                              "line: %p column: %p missing %p for %p"
     #                              $0 $1 ")" :array))))
     #       (line) (column)))
     #   ,|[:array (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #      ;(slice $& 2 -4)])
     #
     :tuple ,(j/l/collection-node :tuple "(" ")")
     #
     :bracket-array ,(j/l/collection-node :bracket-array "@[" "]")
     #
     :bracket-tuple ,(j/l/collection-node :bracket-tuple "[" "]")
     #
     :table ,(j/l/collection-node :table "@{" "}")
     #
     :struct ,(j/l/collection-node :struct "{" "}")
     #
     :number ,(j/l/atom-node :number
                         ~(drop (sequence (cmt (capture (some :num-char))
                                               ,scan-number)
                                          (opt (sequence ":" (range "AZ" "az"))))))
     #
     :num-char (choice (range "09" "AZ" "az")
                       (set "&+-._"))
     #
     :constant ,(j/l/atom-node :constant
                           '(sequence (choice "false" "nil" "true")
                                      (not :name-char)))
     #
     :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                        (set "!$%&*+-./:<?=>@^_"))
     #
     :buffer ,(j/l/atom-node :buffer
                         '(sequence `@"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :escape (sequence "\\"
                       (choice (set `"'0?\abefnrtvz`)
                               (sequence "x" (2 :h))
                               (sequence "u" (4 :h))
                               (sequence "U" (6 :h))
                               (error (constant "bad escape"))))
     #
     :string ,(j/l/atom-node :string
                         '(sequence `"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :long-string ,(j/l/atom-node :long-string
                              :long-bytes)
     #
     :long-bytes {:main (drop (sequence :open
                                        (any (if-not :close 1))
                                        :close))
                  :open (capture :delim :n)
                  :delim (some "`")
                  :close (cmt (sequence (not (look -1 "`"))
                                        (backref :n)
                                        (capture (backmatch :n)))
                              ,=)}
     #
     :long-buffer ,(j/l/atom-node :long-buffer
                              '(sequence "@" :long-bytes))
     #
     :keyword ,(j/l/atom-node :keyword
                          '(sequence ":"
                                     (any :name-char)))
     #
     :symbol ,(j/l/atom-node :symbol
                         '(some :name-char))
     })

(comment

  (get (peg/match j/l/loc-grammar " ") 2)
  # =>
  '(:whitespace @{:bc 1 :bl 1 :ec 2 :el 1} " ")

  (get (peg/match j/l/loc-grammar "true?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 6 :el 1} "true?")

  (get (peg/match j/l/loc-grammar "nil?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 5 :el 1} "nil?")

  (get (peg/match j/l/loc-grammar "false?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 7 :el 1} "false?")

  (get (peg/match j/l/loc-grammar "# hi there") 2)
  # =>
  '(:comment @{:bc 1 :bl 1 :ec 11 :el 1} "# hi there")

  (get (peg/match j/l/loc-grammar "1_000_000") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 10 :el 1} "1_000_000")

  (get (peg/match j/l/loc-grammar "8.3") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "8.3")

  (get (peg/match j/l/loc-grammar "1e2") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "1e2")

  (get (peg/match j/l/loc-grammar "0xfe") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "0xfe")

  (get (peg/match j/l/loc-grammar "2r01") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "2r01")

  (get (peg/match j/l/loc-grammar "3r101&01") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 9 :el 1} "3r101&01")

  (get (peg/match j/l/loc-grammar "2:u") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "2:u")

  (get (peg/match j/l/loc-grammar "-8:s") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "-8:s")

  (get (peg/match j/l/loc-grammar "1e2:n") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 6 :el 1} "1e2:n")

  (get (peg/match j/l/loc-grammar "printf") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 7 :el 1} "printf")

  (get (peg/match j/l/loc-grammar ":smile") 2)
  # =>
  '(:keyword @{:bc 1 :bl 1 :ec 7 :el 1} ":smile")

  (get (peg/match j/l/loc-grammar `"fun"`) 2)
  # =>
  '(:string @{:bc 1 :bl 1 :ec 6 :el 1} "\"fun\"")

  (get (peg/match j/l/loc-grammar "``long-fun``") 2)
  # =>
  '(:long-string @{:bc 1 :bl 1 :ec 13 :el 1} "``long-fun``")

  (get (peg/match j/l/loc-grammar "@``long-buffer-fun``") 2)
  # =>
  '(:long-buffer @{:bc 1 :bl 1 :ec 21 :el 1} "@``long-buffer-fun``")

  (get (peg/match j/l/loc-grammar `@"a buffer"`) 2)
  # =>
  '(:buffer @{:bc 1 :bl 1 :ec 12 :el 1} "@\"a buffer\"")

  (get (peg/match j/l/loc-grammar "@[8]") 2)
  # =>
  '(:bracket-array @{:bc 1 :bl 1
                     :ec 5 :el 1}
                   (:number @{:bc 3 :bl 1
                              :ec 4 :el 1} "8"))

  (get (peg/match j/l/loc-grammar "@{:a 1}") 2)
  # =>
  '(:table @{:bc 1 :bl 1
             :ec 8 :el 1}
           (:keyword @{:bc 3 :bl 1
                       :ec 5 :el 1} ":a")
           (:whitespace @{:bc 5 :bl 1
                          :ec 6 :el 1} " ")
           (:number @{:bc 6 :bl 1
                      :ec 7 :el 1} "1"))

  (get (peg/match j/l/loc-grammar "~x") 2)
  # =>
  '(:quasiquote @{:bc 1 :bl 1
                  :ec 3 :el 1}
                (:symbol @{:bc 2 :bl 1
                           :ec 3 :el 1} "x"))

  (get (peg/match j/l/loc-grammar "' '[:a :b]") 2)
  # =>
  '(:quote @{:bc 1 :bl 1
             :ec 11 :el 1}
           (:whitespace @{:bc 2 :bl 1
                          :ec 3 :el 1} " ")
           (:quote @{:bc 3 :bl 1
                     :ec 11 :el 1}
                   (:bracket-tuple @{:bc 4 :bl 1
                                     :ec 11 :el 1}
                                   (:keyword @{:bc 5 :bl 1
                                               :ec 7 :el 1} ":a")
                                   (:whitespace @{:bc 7 :bl 1
                                                  :ec 8 :el 1} " ")
                                   (:keyword @{:bc 8 :bl 1
                                               :ec 10 :el 1} ":b"))))

  )

(def j/l/loc-top-level-ast
  (put (table ;(kvs j/l/loc-grammar))
       :main ~(sequence (line) (column)
                        :input
                        (line) (column))))

(defn j/l/par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc tree el ec]
             (peg/match j/l/loc-top-level-ast src start)]
      @[:code (j/l/make-attrs bl bc el ec) tree]
      @[:code])
    (if-let [captures (peg/match j/l/loc-grammar src start)]
      (let [[bl bc] (slice captures 0 2)
            [el ec] (slice captures -3)
            trees (array/slice captures 2 -3)]
        (array/insert trees 0
                      :code (j/l/make-attrs bl bc el ec)))
      @[:code])))

# XXX: backward compatibility
(def j/l/ast j/l/par)

(comment

  (j/l/par "(+ 1 1)")
  # =>
  '@[:code @{:bc 1 :bl 1
             :ec 8 :el 1}
     (:tuple @{:bc 1 :bl 1
               :ec 8 :el 1}
             (:symbol @{:bc 2 :bl 1
                        :ec 3 :el 1} "+")
             (:whitespace @{:bc 3 :bl 1
                            :ec 4 :el 1} " ")
             (:number @{:bc 4 :bl 1
                        :ec 5 :el 1} "1")
             (:whitespace @{:bc 5 :bl 1
                            :ec 6 :el 1} " ")
             (:number @{:bc 6 :bl 1
                        :ec 7 :el 1} "1"))]

  )

(defn j/l/gen*
  [an-ast buf]
  (case (first an-ast)
    :code
    (each elt (drop 2 an-ast)
      (j/l/gen* elt buf))
    #
    :buffer
    (buffer/push-string buf (in an-ast 2))
    :comment
    (buffer/push-string buf (in an-ast 2))
    :constant
    (buffer/push-string buf (in an-ast 2))
    :keyword
    (buffer/push-string buf (in an-ast 2))
    :long-buffer
    (buffer/push-string buf (in an-ast 2))
    :long-string
    (buffer/push-string buf (in an-ast 2))
    :number
    (buffer/push-string buf (in an-ast 2))
    :string
    (buffer/push-string buf (in an-ast 2))
    :symbol
    (buffer/push-string buf (in an-ast 2))
    :whitespace
    (buffer/push-string buf (in an-ast 2))
    #
    :array
    (do
      (buffer/push-string buf "@(")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf ")"))
    :bracket-array
    (do
      (buffer/push-string buf "@[")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "]"))
    :bracket-tuple
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "]"))
    :tuple
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf ")"))
    :struct
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "}"))
    :table
    (do
      (buffer/push-string buf "@{")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "}"))
    #
    :fn
    (do
      (buffer/push-string buf "|")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :quasiquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :splice
    (do
      (buffer/push-string buf ";")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf ",")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    ))

(defn j/l/gen
  [an-ast]
  (let [buf @""]
    (j/l/gen* an-ast buf)
    # XXX: leave as buffer?
    (string buf)))

# XXX: backward compatibility
(def j/l/code j/l/gen)

(comment

  (j/l/gen
    [:code])
  # =>
  ""

  (j/l/gen
    '(:whitespace @{:bc 1 :bl 1
                    :ec 2 :el 1} " "))
  # =>
  " "

  (j/l/gen
    '(:buffer @{:bc 1 :bl 1
                :ec 12 :el 1} "@\"a buffer\""))
  # =>
  `@"a buffer"`

  (j/l/gen
    '@[:code @{:bc 1 :bl 1
               :ec 8 :el 1}
       (:tuple @{:bc 1 :bl 1
                 :ec 8 :el 1}
               (:symbol @{:bc 2 :bl 1
                          :ec 3 :el 1} "+")
               (:whitespace @{:bc 3 :bl 1
                              :ec 4 :el 1} " ")
               (:number @{:bc 4 :bl 1
                          :ec 5 :el 1} "1")
               (:whitespace @{:bc 5 :bl 1
                              :ec 6 :el 1} " ")
               (:number @{:bc 6 :bl 1
                          :ec 7 :el 1} "1"))])
  # =>
  "(+ 1 1)"

  )

(comment

  (def src "{:x  :y \n :z  [:a  :b    :c]}")

  (j/l/gen (j/l/par src))
  # =>
  src

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                             "/src/janet/src/boot/boot.janet"))]
      (= (string src)
         (j/l/gen (j/l/par src))))

    )

  )


(def j/version "2026-03-16_06-41-09")

# exports
(def j/par j/l/par)
(def j/gen j/l/gen)

########################################################################

(defn j/zipper
  ``
  Returns a new zipper consisting of two elements:

  * `a-root` - the passed in root node.
  * `state` - table of info about node's z-location in the tree with keys:
    * `:ls` - left siblings
    * `:pnodes` - path of nodes from root to current z-location
    * `:pstate` - parent node's state
    * `:rs` - right siblings
    * `:changed?` - indicates whether "editing" has occured

  `state` has a prototype table with four functions:

  * :branch? - fn that tests if a node is a branch (has children)
  * :children - fn that returns the child nodes for the given branch.
  * :make-node - fn that takes a node + children and returns a new branch
    node with the same.
  * :make-state - fn for creating a new state
  ``
  [a-root branch?-fn children-fn make-node-fn]
  #
  (defn make-state_
    [&opt ls_ rs_ pnodes_ pstate_ changed?_]
    (table/setproto @{:ls ls_
                      :pnodes pnodes_
                      :pstate pstate_
                      :rs rs_
                      :changed? changed?_}
                    @{:branch? branch?-fn
                      :children children-fn
                      :make-node make-node-fn
                      :make-state make-state_}))
  #
  [a-root (make-state_)])

(comment

  # XXX

  )

(defn j/indexed-zip
  ``
  Returns a zipper for nested indexed data structures (tuples
  or arrays), given a root data structure.
  ``
  [indexed]
  (j/zipper indexed
          indexed?
          j/h/to-entries
          (fn [p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (j/indexed-zip a-node))

  the-node
  # =>
  a-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/node
  "Returns the node at `zloc`."
  [zloc]
  (get zloc 0))

(comment

  (j/node (j/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/state
  "Returns the state for `zloc`."
  [zloc]
  (get zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (j/indexed-zip [:a [:b [:x :y]]])
             j/state))
  # =>
  @{}

  )

(defn j/branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((j/state zloc) :branch?) (j/node zloc)))

(comment

  (j/branch? (j/indexed-zip [:a :b [:x :y]]))
  # =>
  true

  )

(defn j/children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (j/branch? zloc)
    (((j/state zloc) :children) (j/node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

  (j/children (j/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((j/state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (j/make-state (j/indexed-zip [:a :b [:x :y]])))
  # =>
  @{}

  )

(defn j/down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (j/branch? zloc)
    (let [[z-node st] zloc
          [k rest-kids kids]
          (j/h/first-rest-maybe-all (j/children zloc))]
      (when kids
        [k
         (j/make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (j/h/tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (j/node (j/down (j/indexed-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/branch?)
  # =>
  false

  (try
    (-> (j/indexed-zip [:a])
        j/down
        j/children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               j/indexed-zip
               j/down
               j/state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # =>
  true

  )

(defn j/right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs_] (j/h/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs_)
      [r
       (j/make-state zloc
                   (j/h/tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/indexed-zip [:a :b])
      j/down
      j/right
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a])
      j/down
      j/right)
  # =>
  nil

  )

(defn j/make-node
  ``
  Returns a branch node, given `zloc`, `a-node` and `kids`.
  ``
  [zloc a-node kids]
  (((j/state zloc) :make-node) a-node kids))

(comment

  (j/make-node (j/indexed-zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # =>
  [:x :y]

  )

(defn j/up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(j/make-node zloc pnode [;ls z-node ;rs])
           (j/make-state zloc
                       (get pstate :ls)
                       (get pstate :rs)
                       (get pstate :pnodes)
                       (get pstate :pstate)
                       true)]
          [pnode pstate])))))

(comment

  (def m-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        j/down
        j/up)
    m-zip)
  # =>
  true

  (deep=
    (-> m-zip
        j/down
        j/right
        j/right
        j/down
        j/up
        j/up)
    m-zip)
  # =>
  true

  )

# XXX: used by `root` and `df-next`
(defn j/end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (j/state zloc)))

(defn j/root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (j/end? zloc)
    (j/node zloc)
    (if-let [p (j/up zloc)]
      (j/root p)
      (j/node zloc))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (j/node a-zip)
  # =>
  (-> a-zip
      j/down
      j/right
      j/right
      j/down
      j/root)

  )

(defn j/df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [a-loc]
    (if (j/up a-loc)
      (or (j/right (j/up a-loc))
          (recur (j/up a-loc)))
      [(j/node a-loc) :end]))
  #
  (if (j/end? zloc)
    zloc
    (or (and (j/branch? zloc) (j/down zloc))
        (j/right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x]]))

  (j/node (j/df-next a-zip))
  # =>
  :a

  (-> a-zip
      j/df-next
      j/df-next
      j/node)
  # =>
  :b

  (-> a-zip
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/end?)
  # =>
  true

  )

(defn j/replace
  "Replaces existing node at `zloc` with `a-node`, without moving."
  [zloc a-node]
  (let [[_ st] zloc]
    [a-node
     (j/make-state zloc
                 (get st :ls)
                 (get st :rs)
                 (get st :pnodes)
                 (get st :pstate)
                 true)]))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:w :b [:x :y]]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:a :b [:w :y]]

  )

(defn j/edit
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
  where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (j/replace zloc
           (apply f (j/node zloc) args)))

(comment

  (-> (j/indexed-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/root)
  # =>
  [2 2 [8 9]]

  (-> (j/indexed-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/right
      (j/edit inc)
      j/right
      j/down
      (j/edit dec)
      j/right
      (j/edit dec)
      j/root)
  # =>
  [2 3 [7 8]]

  )

(defn j/insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [child ;(j/children zloc)])))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      (j/insert-child :c)
      j/root)
  # =>
  [:c :a :b [:x :y]]

  )

(defn j/append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [;(j/children zloc) child])))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      (j/append-child :c)
      j/root)
  # =>
  [:a :b [:x :y] :c]

  )

(defn j/rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (j/make-state zloc
                   (j/h/tuple-push ls z-node ;(j/h/butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/node)
  # =>
  [:x :y]

  )

(defn j/remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.  Throws an error if called at the
  root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (j/branch? a-zloc) (j/down a-zloc))]
        (recur (j/rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (j/make-state zloc
                            (j/h/butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(j/make-node zloc (last pnodes) rs)
         (j/make-state zloc
                     (get pstate :ls)
                     (get pstate :rs)
                     (get pstate :pnodes)
                     (get pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/remove
      j/node)
  # =>
  :a

  (try
    (j/remove (j/indexed-zip [:a :b [:x :y]]))
    ([e] e))
  # =>
  "Called `remove` at root"

  )

(defn j/left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (j/make-state zloc
                   (j/h/butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/indexed-zip [:a :b :c])
      j/down
      j/right
      j/right
      j/left
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a])
      j/down
      j/left)
  # =>
  nil

  )

(defn j/df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (j/branch? a-zloc)
                        (j/down a-zloc))]
      (recur (j/rightmost child))
      a-zloc))
  #
  (if-let [left-loc (j/left zloc)]
    (recur left-loc)
    (j/up zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/df-prev
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/df-prev
      j/node)
  # =>
  [:x :y]

  )

(defn j/insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   ls
                   [a-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-right :z)
      j/root)
  # =>
  [:a :z :b [:x :y]]

  (try
    (j/insert-right a-zip :e)
    ([e] e))
  # =>
  "Called `insert-right` at root"

  )

(defn j/insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   (j/h/tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-left :z)
      j/root)
  # =>
  [:z :a :b [:x :y]]

  (try
    (j/insert-left a-zip :e)
    ([e] e))
  # =>
  "Called `insert-left` at root"

  )

(defn j/rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :rs)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rights)
  # =>
  [:b [:x :y]]

  (-> (j/indexed-zip [:a :b])
      j/down
      j/right
      j/rights)
  # =>
  []

  )

(defn j/lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (j/state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (j/indexed-zip [:a :b])
      j/down
      j/lefts)
  # =>
  []

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/lefts)
  # =>
  [:a :b]

  )

(defn j/leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (j/make-state zloc
                   []
                   [;(j/h/rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/leftmost
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/leftmost
      j/node)
  # =>
  :a

  )

(defn j/path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :pnodes)))

(comment

  (j/path (j/indexed-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]]]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]] [:x :y]]

  )

(defn j/right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (j/right zloc)]
    (if (pred right-sib)
      right-sib
      (j/right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      (j/right-until |(match (j/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      j/node)
  # =>
  [:symbol "+"]

  )

(defn j/left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (j/left zloc)]
    (if (pred left-sib)
      left-sib
      (j/left-until left-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      j/rightmost
      (j/left-until |(match (j/node $)
                     [:comment]
                     false
                     #
                     [:whitespace]
                     false
                     #
                     true))
      j/node)
  # =>
  [:number "1"]

  )

(defn j/search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (j/df-next zloc)]
      (when (j/end? next-zloc)
        (break nil))
      (j/search-from next-zloc pred))))

(comment

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :b
                      true))
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :d
                      true)))
  # =>
  nil

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :a
                      true))
      j/node)
  # =>
  :a

  )

(defn j/search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (j/end? zloc)
    (break nil))
  (when-let [next-zloc (j/df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (j/search-after next-zloc pred))))

(comment

  (-> (j/indexed-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :b
                       true))
      j/left
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :d
                       true)))
  # =>
  nil

  (-> (j/indexed-zip [:a [:b :c [2 [3 :smile] 5]]])
      (j/search-after |(match (j/node $)
                       [_ :smile]
                       true))
      j/down
      j/node)
  # =>
  3

  )

(defn j/unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.

  Throws an error if `zloc` corresponds to a top-most container.
  ``
  [zloc]
  (unless (j/branch? zloc)
    (break zloc))
  #
  (when (empty? (j/state zloc))
    (error "Called `unwrap` at root"))
  #
  (def kids (j/children zloc))
  (var i (dec (length kids)))
  (var curr-zloc zloc)
  (while (<= 0 i) # right to left
    (set curr-zloc
         (j/insert-right curr-zloc (get kids i)))
    (-- i))
  # try to end up at a sensible spot
  (set curr-zloc
       (j/remove curr-zloc))
  (if-let [ret-zloc (j/right curr-zloc)]
    ret-zloc
    curr-zloc))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/unwrap
      j/root)
  # =>
  [:a :b :x :y]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a :b [:x :y]]

  (-> (j/indexed-zip [[:a]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a]

  (-> (j/indexed-zip [[:a :b] [:x :y]])
      j/down
      j/down
      j/remove
      j/unwrap
      j/root)
  # =>
  [:b [:x :y]]

  (try
    (-> (j/indexed-zip [:a :b [:x :y]])
        j/unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn j/eq?
  ``
  Compare two zlocs, `a-zloc` and `b-zloc`, for equality.
  ``
  [a-zloc b-zloc]
  (and (= (length (j/lefts a-zloc)) (length (j/lefts b-zloc)))
       (= (j/path a-zloc) (j/path b-zloc))))

(comment

  (def iz (j/indexed-zip [:a :b :c :b]))

  (j/eq? (-> iz j/down j/right)
       (-> iz j/down j/right j/right j/right))
  # =>
  false

  (j/eq? (-> iz j/down j/right)
       (-> iz j/down j/right j/right j/right j/left j/left))
  # =>
  true

  )

(defn j/wrap
  ``
  Replace nodes from `start-zloc` through `end-zloc` with a single
  node of the same type as `wrap-node` containing the nodes from
  `start-zloc` through `end-zloc`.

  If `end-zloc` is not specified, just wrap `start-zloc`.

  The caller is responsible for ensuring the value of `end-zloc`
  is somewhere to the right of `start-zloc`.  Throws an error if
  an inappropriate value is specified for `end-zloc`.
  ``
  [start-zloc wrap-node &opt end-zloc]
  (default end-zloc start-zloc)
  #
  # 1. collect all nodes to wrap
  #
  (def kids @[])
  (var cur-zloc start-zloc)
  (while (and cur-zloc
              (not (j/eq? cur-zloc end-zloc))) # left to right
    (array/push kids (j/node cur-zloc))
    (set cur-zloc (j/right cur-zloc)))
  (when (nil? cur-zloc)
    (error "Called `wrap` with invalid value for `end-zloc`."))
  # also collect the last node
  (array/push kids (j/node end-zloc))
  #
  # 2. replace locations that will be removed with non-container nodes
  #
  (def dummy-node
    (j/make-node start-zloc wrap-node (tuple)))
  (set cur-zloc start-zloc)
  # trying to do this together in step 1 is not straight-forward
  # because the desired exiting condition for the while loop depends
  # on cur-zloc becoming end-zloc -- if `replace` were to be used
  # there, the termination condition never gets fulfilled properly.
  (for i 0 (dec (length kids)) # left to right again
    (set cur-zloc
         (-> (j/replace cur-zloc dummy-node)
             j/right)))
  (set cur-zloc
       (j/replace cur-zloc dummy-node))
  #
  # 3. remove all relevant locations
  #
  (def new-node
    (j/make-node start-zloc wrap-node (tuple ;kids)))
  (for i 0 (dec (length kids)) # right to left
    (set cur-zloc
         (j/remove cur-zloc)))
  # 4. put the new container node into place
  (j/replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (j/indexed-zip [:a [:b] :c :x])
        j/down
        j/right))

  (j/node start-zloc)
  # =>
  [:b]

  (-> (j/wrap start-zloc [])
      j/root)
  # =>
  [:a [[:b]] :c :x]

  (def end-zloc
    (j/right start-zloc))

  (j/node end-zloc)
  # =>
  :c

  (-> (j/wrap start-zloc [] end-zloc)
      j/root)
  # =>
  [:a [[:b] :c] :x]

  (try
    (-> (j/wrap end-zloc [] start-zloc)
        j/root)
    ([e] e))
  # =>
  "Called `wrap` with invalid value for `end-zloc`."

  )

########################################################################

(defn j/has-children?
  ``
  Returns true if `a-node` can have children.
  Returns false if `a-node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:code true
                   :fn true
                   :quasiquote true
                   :quote true
                   :splice true
                   :unquote true
                   :array true
                   :tuple true
                   :bracket-array true
                   :bracket-tuple true
                   :table true
                   :struct true}
                  head))))

(comment

  (j/has-children?
    [:tuple @{}
     [:symbol @{} "+"] [:whitespace @{} " "]
     [:number @{} "1"] [:whitespace @{} " "]
     [:number @{} "2"]])
  # =>
  true

  (j/has-children? [:number @{} "8"])
  # =>
  false

  )

(defn j/zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing Janet code.
  ``
  [a-tree]
  (defn branch?_
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (j/has-children? a-node))))
  #
  (defn children_
    [a-node]
    (if (branch?_ a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node_
    [a-node kids]
    [(first a-node) (get a-node 1) ;kids])
  #
  (j/zipper a-tree branch?_ children_ make-node_))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (j/zip root-node))

  the-node
  # =>
  root-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/attrs
  ``
  Return the attributes table for the node of a z-location.  The
  attributes table contains at least bounds of the node by 1-based line
  and column numbers.
  ``
  [zloc]
  (get (j/node zloc) 1))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip
      j/down
      j/attrs)
  # =>
  @{:bc 1 :bl 1 :ec 8 :el 1}

  )

(defn j/zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [a-tree]
  (-> (j/zip a-tree)
      j/down))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip-down
      j/node)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 8 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "+"]
   [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
   [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
   [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
   [:number @{:bc 6 :bl 1 :ec 7 :el 1} "3"]]

  (-> (j/par "(/ 1 8)")
      j/zip-down
      j/root)
  # =>
  @[:code @{:bc 1 :bl 1 :ec 8 :el 1}
    [:tuple @{:bc 1 :bl 1 :ec 8 :el 1}
     [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "/"]
     [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
     [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
     [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
     [:number @{:bc 6 :bl 1 :ec 7 :el 1} "8"]]]

  )

# wsc == whitespace, comment
(defn j/right-skip-wsc
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-wsc)
  # =>
  nil

  )

(defn j/left-skip-wsc
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 [:comment]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/right-skip-wsc
      j/left-skip-wsc
      j/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-wsc)
  # =>
  nil

  )

# ws == whitespace
(defn j/right-skip-ws
  ``
  Try to move right from `zloc`, skipping over whitespace
  nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "( # hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-ws
      j/node)
  # =>
  [:comment @{:bc 3 :bl 1 :ec 13 :el 1} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-ws)
  # =>
  nil

  )

(defn j/left-skip-ws
  ``
  Try to move left from `zloc`, skipping over whitespace
  nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right
      j/right
      j/left-skip-ws
      j/node)
  # =>
  [:comment @{:bc 2 :bl 1 :ec 12 :el 1} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-ws)
  # =>
  nil

  )



########################################################################

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
          (array/push results {:line bl :col bc
                               :thing (string leader)}))))
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
  @[{:col 1 :line 1 :thing "defn"}
    {:col 1 :line 3 :thing "defn"}
    {:col 3 :line 5 :thing "pp"}]

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
  @[{:col 1 :line 1 :thing "defn"}
    {:col 3 :line 3 :thing "pp"}
    {:col 3 :line 4 :thing "print"}
    {:col 3 :line 5 :thing "if"}
    {:col 5 :line 6 :thing "pp"}
    {:col 5 :line 7 :thing "print"}]

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
  (def {:pattern name :pred pred :exact-match exact-match} opts)
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
        (def head-str (string (first parsed)))
        (when (and (matcher name head-str)
                   # ...and only for things that are not top-level
                   (< 1 (length (j/path parent-zloc))))
          (def caller (fc/find-caller parent-zloc))
          (when caller
            (def [line-no col-no caller-name] caller)
            (array/push results {:line line-no :col col-no
                                 :matched head-str
                                 :thing (string caller-name)})))))
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
    {:pattern "pp"})
  # =>
  @[{:col 1 :line 3 :matched "pp" :thing "smile"}]

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
    {:pattern "pp"})
  # =>
  @[{:col 1 :line 1 :matched "pp" :thing "hello"}
    {:col 1 :line 1 :matched "pp" :thing "hello"}]

  )

(defn fc/find-calls-to
  [src opts]
  (def {:pattern name :pred pred
        :exact-match exact-match} opts)
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
          (array/push results {:line bl :col bc
                               :thing raw-code-str}))))
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
    {:pattern "pp"})
  # =>
  @[{:col 3 :line 3 :thing "(pp x)"}
    {:col 5 :line 6 :thing "(pp [:x x])"}]

  )


(comment import ./prefix :prefix "")
# XXX: neat but possibly not great when the number of elements of
#      byte-vals is large?
'(defn common-prefix
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


(comment import ./report :prefix "")
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


(comment import ./search :prefix "")
# query-fn should return a dictionary
(defn s/search-paths
  [paths query-fn opts &opt pattern]
  #
  (def all-results @[])
  (def hit-paths @[])
  (each p paths
    (def src (slurp p))
    (when (< 0 (length src))
      (when (or (not pattern) (string/find pattern src))
        (array/push hit-paths p)
        (def results
          (try (query-fn src opts)
            ([e] (eprintf "search failed for: %s" p))))
        (when (and results (not (empty? results)))
          (each item results
            (array/push all-results (merge item {:path p})))))))
  #
  [all-results hit-paths])


(comment import ./utils :prefix "")
(defn u/has-janet-shebang?
  [path]
  (with [f (file/open path)]
    (def first-line (file/read f :line))
    (when first-line
      # some .js files has very long first lines and can contain
      # a lot of strings...
      (and (string/find "bin/env" first-line)
           (string/find "janet" first-line)))))

(defn u/looks-like-janet?
  [path]
  (or (string/has-suffix? ".janet" path)
      (u/has-janet-shebang? path)))



########################################################################

(defn c/search-and-dump
  [opts]
  (def {:query-fn query-fn :paths paths
        :pattern pattern} opts)
  #
  (def [all-results _]
    (s/search-paths paths query-fn opts pattern))
  # output could be done via (printf "%j" all-results), but the
  # resulting output is harder to read and manipulate
  (print "[")
  (when (not (empty? all-results))
    (if (get (first all-results) :matched)
      (each r all-results
        (printf "[%n %n %n %n %n]"
              (get r :path) (get r :line) (get r :col)
              (get r :thing) (get r :matched)))
      (each r all-results
        (printf "[%n %n %n %n]"
              (get r :path) (get r :line) (get r :col)
              (get r :thing)))))
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
  (def [all-results hit-paths] (s/search-paths paths query-fn opts))
  (print)
  (def prefix (if (<= 0 (length paths) 1)
                ""
                (p/common-prefix paths)))
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
  (def includes (if (= 0 (length the-args))
                  default-paths
                  the-args))
  # find .janet files
  (def src-filepaths (filter |(and (= :file (os/stat $ :mode))
                                   (u/looks-like-janet? $))
                             (em/itemize ;includes)))
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
  (def includes (if (= 0 (length the-args))
                  default-paths
                  the-args))
  # find .janet files
  (def src-filepaths (filter |(and (= :file (os/stat $ :mode))
                                   (u/looks-like-janet? $))
                             (em/itemize ;includes)))
  #
  (when (get opts :dump)
    (c/search-and-dump {:query-fn fc/find-callers-of
                      :paths src-filepaths
                      :pattern name :pred pred
                      :exact-match exact-match})
    (break))
  # search the paths
  (c/search-and-report {:includes includes
                      :query-fn fc/find-callers-of
                      :paths src-filepaths
                      :pattern name :pred pred
                      :exact-match exact-match
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
  (def includes (if (= 0 (length the-args))
                  default-paths
                  the-args))
  # find .janet files
  (def src-filepaths (filter |(and (= :file (os/stat $ :mode))
                                   (u/looks-like-janet? $))
                             (em/itemize ;includes)))
  #
  (when (get opts :dump)
    (c/search-and-dump {:query-fn fc/find-calls-to
                      :paths src-filepaths
                      :pattern name :pred pred
                      :exact-match exact-match})
    (break))
  # search the paths
  (c/search-and-report {:includes includes
                      :query-fn fc/find-calls-to
                      :paths src-filepaths
                      :pattern name :pred pred
                      :exact-match exact-match
                      :report r/report
                      :no-prefix no-prefix
                      :limit-lines limit-lines
                      :editor editor
                      :stop-watch stop-watch}))



(def version "2026-03-16_07-43-59")

(def usage
  `````
  Usage: jakl all-calls <file-or-dir>...
         jakl calls-to <symbol> <file-or-dir>...
         jakl who-calls <symbol> <file-or-dir>...

         jakl [-h|--help] [-v|--version]

  Query some Janet source code for call information.

  Commands:

    all-calls              show all identified calls
    calls-to               show particular calls to <symbol>
    who-calls              show callers of <symbol>

  Parameters:

    <symbol>               name of identifier to query with
    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    all-calls:

      Show all calls within a file:

      $ jakl all-calls data/simple.janet

      Show all calls within a directory of `.janet` files

      $ jakl all-calls data/

    calls-to:

      Show all calls to `def` within a file:

      $ jakl calls-to def data/simple.janet

      Show all calls to `default` within a directory of
      `.janet` files:

      $ jakl calls-to default data/

    who-calls:

      Show all callers of `default` within a file:

      $ jakl who-calls default data/zipper.janet

      Show all callers of `default` within a directory of
      `.janet files`:

      $ jakl who-calls default data/

  `````)

########################################################################

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def cmd (get opts :command))
  #
  (cond
    (get {"all-calls" 1 "calls" 1} cmd)
    (c/all-calls opts)
    #
    (get {"who-calls" 1 "who" 1} cmd)
    (c/who-calls opts)
    #
    (get {"calls-to" 1 "to" 1} cmd)
    (c/calls-to opts)
    #
    (errorf "unrecognized command: %n" cmd)))

