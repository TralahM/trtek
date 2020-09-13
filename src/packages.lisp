;;; packages.lisp

(defpackage trtek
  (:use :cl)
  (:export
    sigmoid
    sigmoid*
    cmplmnt
    memoize
    compose
    fif
    fint
    fun
    readlist
    prompt
    break-loop
    last1
    single
    append1
    conc1
    mklist
    longer
    filter
    group
    flatten
    prune
    mapa-b
    map0-n
    map1-n
    map->
    mappend
    mapcars
    rmapcar
    find2
    before
    after
    duplicate
    split-if
    most
    best
    mostn
    mkstr
    symb
    reread
    explode
    cumsum
    zip
    zipsum
    zipdiff
    zipdiv
    zipmult
    zipn
    sumlist
    package-internal-symbols
    package-external-symbols))

