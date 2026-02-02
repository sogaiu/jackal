#! /usr/bin/env janet

(use ./sh-dsl)

(defn copy-file
  [src dst]
  (spit dst (slurp src)))

(prin "running jell...") (flush)
(def jell-exit ($ janet ./bin/jell))
(assertf (zero? jell-exit)
         "jell exited: %d" jell-exit)
(print "done")

(prin "copying jakl.janet to jakl...")
(copy-file "jakl.janet" "jakl")
(print "done")

(print "running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

########################################################################

(print `trying some "raw" invocations...`)

# sourced from jakl -h output
(def expectations
  [[5 '[./jakl all-calls data/simple.janet]]
   [900 '[./jakl all-calls data/]]
   [3 '[./jakl calls-to def data/simple.janet]]
   [2 '[./jakl calls-to default data/]]
   [1 '[./jakl who-calls default data/zipper.janet]]
   [2 '[./jakl who-calls default data/]]])

(each [n cmd] expectations
  (def new-cmd
    [(first cmd) "{:dump true}" ;(drop 1 cmd)])
  (def output ($< ;new-cmd))
  (def results (parse output))
  (def len (length results))
  (if (= n len)
    (printf "got all %d expected result(s) for: %n" n new-cmd)
    (do
      (eprintf "expected %d result(s) but got %d for: %n"
               n len new-cmd)
      (os/exit 1))))

(print "done")

########################################################################

(print "trying some invocations...")

# sourced from jakl -h output
(def invocations
  ['[./jakl all-calls data/simple.janet]
   '[./jakl calls-to default data/]
   '[./jakl who-calls default data/]])

(each cmd invocations
  (def exit-code ($ ;cmd))
  (if (= 0 exit-code)
    (printf "%n returned: %d" cmd exit-code)
    (do
      (eprintf "%n returned non-zero exit code: %d" cmd exit-code)
      (os/exit 1))))

(print "done")

