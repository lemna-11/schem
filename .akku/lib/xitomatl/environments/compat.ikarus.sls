#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl environments compat)
  (export
    environment?
    environment-symbols)
  (import
    (only (ikarus) environment? environment-symbols))
)
