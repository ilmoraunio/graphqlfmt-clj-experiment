(ns perf-testing
  (:require [clj-async-profiler.core :as flame-graphs]
            [graphqlfmt.core :as core]))

(comment
 (flame-graphs/profile
  (core/fmt "
 {
  a {
    a {
      a {
        a {
          a {
            a {
              a {
                a {
                  a {
                    a {
                      a {
                        a {
                          a {
                            a(
                              a: \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"
                              b: \"b\"
                            )
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
"))
 (flame-graphs/serve-ui 8080))