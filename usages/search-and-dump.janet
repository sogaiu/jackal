(import ../src/commands :as c)
(import ../src/find-calls :as fc)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (c/search-and-dump {:query-fn fc/find-calls-to
                          :paths ["data/location.janet"]
                          :name "default"}))
    buf)
  # =>
  (buffer
    "[\n"
    "["
    `"data/location.janet" `
    `317 3 `
    `"(default start 0)"`
    "]\n"
    "]\n\n")

  (let [buf @""]
    (with-dyns [:out buf]
      (c/search-and-dump {:query-fn fc/find-callers-of
                          :paths ["data/zipper.janet"]
                          :name "default"}))
    buf)
  # =>
  (buffer
    "[\n"
    "["
    `"data/zipper.janet" `
    `1212 1 `
    `wrap`
    "]\n"
    "]\n\n")

  )

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (c/search-and-dump {:query-fn fc/find-calls
                          :paths ["data/location.janet"]}))
    buf)
  # =>
  (slurp "data/calls-in-location.jdn")

  )

