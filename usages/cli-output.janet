(import ../src/main)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main "" "{:stop-watch false}"
                "all-calls" "data/simple.janet"))
    buf)
  # =>
  (slurp "data/cli-output-all-calls-simple.txt")

  )

