#! /usr/bin/env janet

(import ./args :prefix "")
(import ./commands :prefix "")

(def usage
  `````
  Usage: jakl all-calls <file-or-dir>...
         jakl calls-to <symbol> <file-or-dir>...
         jakl who-calls <symbol> <file-or-dir>...

         jakl [-h|--help]

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
  (when (get opts :help)
    (print usage)
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

