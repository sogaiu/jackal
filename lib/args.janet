(defn parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break {:help true}))
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
        (def head (get the-args 0))
        (array/remove the-args 0)
        (assertf head "expected a command but found none: %n" args)
        [opts head])))
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

