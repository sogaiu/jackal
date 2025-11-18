(import ../lib/cli)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (cli/main "" "{:stop-watch false}"
                "all-calls" "data/simple.janet"))
    buf)
  # =>
  (slurp "data/cli-output-all-calls-simple.txt")

  )

