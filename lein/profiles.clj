{:user {:plugins [;;[lein-nodisassemble "0.1.2"]
                  [jonase/eastwood "0.1.4"]
                  [lein-try "0.4.3"]
                  [cider/cider-nrepl "0.8.0-SNAPSHOT"]
                  [lein-gorilla "0.3.3"]
                  [lein-exec "0.3.4"]]
        :dependencies [[slamhound "1.5.5"]
                       [criterium "0.4.3"]]
        :repl-options {:init (require '[criterium.core :as crit])}}}
