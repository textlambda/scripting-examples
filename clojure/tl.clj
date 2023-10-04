;; If `tl` command is not in your path, use the full path instead.
(def ^{:doc "path to tl command"} tl-command "tl")

(require '[babashka.pods :as pods])
(pods/load-pod [tl-command])

(ns user
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [babashka.fs :as fs]
   [pp-grid.api :as g]
   [pod.textlambda.tl :as tl])
  (:import [java.lang ProcessBuilder$Redirect]))

(def editor-command (or (System/getenv "EDITOR") "vim"))

(defn edit-file
  "Opens given file in editor and returns the file contents after the editor exits.

  Does not work on windows."
  [path]
  (-> (ProcessBuilder. [editor-command path])
      (.inheritIO)
      (.start)
      (.waitFor))
  (slurp path))

(defn edit
  "Opens user.clj in editor and loads the content after editor exits.

  Does not work on windows."
  []
  (load-string (edit-file "user.clj")))

(defn edit-in-temp-file
  "Opens editor to edit given content in a temporary file.

  1. Creates a temporary file with given content
  2. Opens the temporary file in editor, and waits for it to exit.
  3. Reads edited content
  4. Deletes temporary file
  4. Returns edited content.

  Does not work in windows."
  [content]
  (let [tmp-file-path (str (fs/create-temp-file))]
    (try
      (spit tmp-file-path content)
      (edit-file tmp-file-path)
      (finally (fs/delete-if-exists tmp-file-path)))))

(defn edit-entry
  "Opens editor to edit an entry.

  Does not work on windows."
  [id]
  (let [entry (tl/get id)
        edited-content (edit-in-temp-file (:content entry))]
    (tl/set id edited-content)))

(defn edit-entry-in
  "Opens editor to edit entry in given path of names.

  Does not work on windows."
  [& names]
  (let [content (:content (apply tl/get-in names))
        edited-content (edit-in-temp-file content)]
    (tl/set-in names edited-content)))

(defn docstrings
  "Gets sequence of maps with symbol-name and symbol-docstring."
  ([namespace] (docstrings namespace (constantly true) ""))
  ([namespace prefix] (docstrings namespace (constantly true) prefix))
  ([namespace pred prefix]
   (let [m  (into (sorted-map) (ns-publics namespace))]
     (for [[k v] m
           :when (pred k v)]
       {:name (str prefix k) :description (:doc (meta v))}))))

(defn pretty-print-docstrings
  "Pretty-prints maps from `docstrings` function."
  [ms]
  (doseq [{name :name description :description} ms]
    (println (str "▸ " name))
    (when (not (nil? description))
      (println (->> description
                    s/split-lines
                    (map s/trim)
                    (s/join "\n")))
      (println))))

(defn tabulate-docstrings
  "Prints maps from `docstrings` function as a table.

  Only the first line of each description will be shown."
  [ms]
  (->> ms
       (map (fn [m] (update-in m [:description] #(and %1 (first (s/split-lines %1))))))
       (#(g/table [:name :description] % :align :left))
       str
       println))

(defn help
  "Pretty prints all docstrings of functions in tl."
  []
  (pretty-print-docstrings (docstrings 'pod.textlambda.tl)))

(defn load-notebook
  "Loads notebook from given path. Returns notebook info on success, false on error."
  ([path passphrase] (load-notebook path passphrase true))
  ([path passphrase create-notebook-if-missing]
   (try
     (tl/init {:passphrase passphrase
               :notebook-path path
               :create-notebook-if-missing create-notebook-if-missing})
     (catch Exception e
       (let [type (keyword (:type (ex-data e)))
             message (.getMessage e)]
         (println (str "☠️  " type "\n" message))
         false)))))

(defn startup
  "Sets up the repl.

  1. loads user.clj if it exists
  2. prints some help"
  []
  (println (str "Text λ v" (tl/version)))
  (let [f (io/file "user.clj")]
    (when (.exists f)
      (println " ✔︎ Found user.clj. Loading...")
      (load-string (slurp f))))

  (println)
  (println "▸ Available functions from tl")
  (println "  Use doc function for more information.")
  (-> (docstrings 'pod.textlambda.tl "tl/")
      tabulate-docstrings)

  (println "▸ Other functions in this repl")
  (println "  Use doc function for more information.")
  (-> (docstrings 'user
                  (fn [k _v]
                    (#{"edit"
                       "edit-entry"
                       "edit-entry-in"
                       "help"
                       "load-notebook"} (str k)))
                  "")
      tabulate-docstrings))

(defn shutdown
  "Shutdown hook."
  []
  (println "Goodbye!"))

(-> (Runtime/getRuntime)
    (.addShutdownHook (Thread. shutdown)))

(startup)
