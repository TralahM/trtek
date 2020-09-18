;;; packages.lisp

(defpackage trtek
  (:use :cl)
  (:export
    *banana-ops*
    *ops*
    *school-ops*
    acos-deg
    after
    append1
    asin-deg
    atan-deg
    before
    best
    break-loop
    chars
    choose
    cmplmnt
    compose
    conc1
    cos-deg
    cross-product
    cumsum
    deg-to-rad
    dot-product
    duplicate
    explode
    fact
    fif
    filter
    find2
    fint
    flatten
    fun
    gdebug
    gps
    group
    last1
    longer
    lrec
    map->
    map0-n
    map1-n
    mapa-b
    mapcars
    mappend
    memoize
    mklist
    mkstr
    most
    mostn
    op
    prompt
    prune
    rad-to-deg
    readlist
    reread
    rfind-if
    rmapcar
    sigmoid
    sigmoid*
    sin-deg
    single
    split-if
    sumlist
    symb
    tan-deg
    trec
    ttrav
    undebug
    use
    zip
    zipdiff
    zipdiv
    zipmult
    zipn
    zipsum
    package-internal-symbols
    package-external-symbols))

(defpackage trtek-tests
  (:use :cl
        :trtek
        :fiveam)
  (:export #:run! #:all-tests test-trtek))
