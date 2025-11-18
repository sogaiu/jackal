(def older
  ["Edward"
   "Michael"
   "Derek"
   "Terence"
   "Michel"
   "Alan"
   "Donald"
   "Tony"
   "Cyril"
   "Maurice"
   "Delphine"])

(def younger
  ["Bruce"
   "Richard"
   "Sidney"])

(def muses
  ["3D"
   "Daddy G"
   "Tricky"
   "Mushroom"])

(defn mixer
  "To doc or not to doc..."
  [x]
  (map |[$ $ $]
       older younger muses))

