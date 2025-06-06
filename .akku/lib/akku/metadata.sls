#!r6rs
;; This file was written by Akku.scm
;; This file is automatically generated and is not a copyrightable work.
(library
 (akku metadata)
 (export
  main-package-name
  main-package-version
  installed-libraries
  installed-assets)
 (import (only (rnrs) define quote))
 (define main-package-name '"dstra_chez")
 (define main-package-version '"0.0.0-alpha.0")
 (define installed-libraries
   '((algos)
     (dynarray)
     (graph-parser)
     (pheap)
     (private install sipp)
     (srfi :0)
     (srfi :0 cond-expand)
     (srfi :1)
     (srfi :1 lists)
     (srfi :11)
     (srfi :11 let-values)
     (srfi :111)
     (srfi :111 boxes)
     (srfi :113)
     (srfi :113 sets-and-bags)
     (srfi :115)
     (srfi :115 regexp)
     (srfi :115 regexp boundary)
     (srfi :116)
     (srfi :116 ilists)
     (srfi :117)
     (srfi :117 list-queues)
     (srfi :125)
     (srfi :125 hashtables)
     (srfi :126)
     (srfi :126 helpers helpers)
     (srfi :126 r6rs-hashtables)
     (srfi :127)
     (srfi :127 lazy-sequences)
     (srfi :128)
     (srfi :128 comparators)
     (srfi :129)
     (srfi :129 titlecase)
     (srfi :13)
     (srfi :13 strings)
     (srfi :130)
     (srfi :130 string-cursors)
     (srfi :131)
     (srfi :131 records)
     (srfi :132)
     (srfi :132 sorting)
     (srfi :133)
     (srfi :133 vectors)
     (srfi :134)
     (srfi :134 ideques)
     (srfi :139)
     (srfi :139 impl)
     (srfi :14)
     (srfi :14 char-sets)
     (srfi :14 char-sets inversion-list)
     (srfi :141)
     (srfi :141 integer-division)
     (srfi :143)
     (srfi :143 fixnums)
     (srfi :143 helpers)
     (srfi :145)
     (srfi :145 assumptions)
     (srfi :146)
     (srfi :146 gleckler hamt)
     (srfi :146 gleckler hamt-map)
     (srfi :146 gleckler hamt-misc)
     (srfi :146 gleckler vector-edit)
     (srfi :146 hash)
     (srfi :146 mappings)
     (srfi :146 nieper rbtree)
     (srfi :15 fluid-let)
     (srfi :151)
     (srfi :151 bitwise-operations)
     (srfi :152)
     (srfi :152 strings)
     (srfi :156)
     (srfi :156 predicate-combiners)
     (srfi :158)
     (srfi :158 generators-and-accumulators)
     (srfi :16)
     (srfi :16 case-lambda)
     (srfi :165)
     (srfi :166)
     (srfi :166 base)
     (srfi :166 color)
     (srfi :166 columnar)
     (srfi :166 pretty)
     (srfi :166 show-shared)
     (srfi :166 unicode)
     (srfi :17)
     (srfi :17 generalized-set!)
     (srfi :17 helpers)
     (srfi :171)
     (srfi :171 meta)
     (srfi :171 transducers)
     (srfi :175)
     (srfi :19)
     (srfi :19 time)
     (srfi :19 time compat)
     (srfi :19 time not-implemented)
     (srfi :197)
     (srfi :197 pipeline-operators)
     (srfi :2)
     (srfi :2 and-let*)
     (srfi :213)
     (srfi :213 impl)
     (srfi :214)
     (srfi :214 impl)
     (srfi :214 parameters)
     (srfi :219)
     (srfi :224)
     (srfi :23)
     (srfi :23 error)
     (srfi :23 error tricks)
     (srfi :244)
     (srfi :244 define-values)
     (srfi :25)
     (srfi :25 multi-dimensional-arrays)
     (srfi :25 multi-dimensional-arrays all)
     (srfi :25 multi-dimensional-arrays arlib)
     (srfi :26)
     (srfi :26 cut)
     (srfi :27)
     (srfi :27 random-bits)
     (srfi :28)
     (srfi :28 basic-format-strings)
     (srfi :29)
     (srfi :29 localization)
     (srfi :31)
     (srfi :31 rec)
     (srfi :34)
     (srfi :34 exception-handling)
     (srfi :35)
     (srfi :35 conditions)
     (srfi :37)
     (srfi :37 args-fold)
     (srfi :38)
     (srfi :38 with-shared-structure)
     (srfi :39)
     (srfi :39 parameters)
     (srfi :4)
     (srfi :4 numeric-vectors)
     (srfi :41)
     (srfi :41 streams)
     (srfi :41 streams derived)
     (srfi :41 streams primitive)
     (srfi :42)
     (srfi :42 eager-comprehensions)
     (srfi :43)
     (srfi :43 vectors)
     (srfi :45)
     (srfi :45 lazy)
     (srfi :48)
     (srfi :48 intermediate-format-strings)
     (srfi :48 intermediate-format-strings compat)
     (srfi :5)
     (srfi :5 let)
     (srfi :51)
     (srfi :51 rest-values)
     (srfi :54)
     (srfi :54 cat)
     (srfi :6)
     (srfi :6 basic-string-ports)
     (srfi :6 basic-string-ports compat)
     (srfi :60)
     (srfi :60 integer-bits)
     (srfi :61)
     (srfi :61 cond)
     (srfi :64)
     (srfi :64 testing)
     (srfi :67)
     (srfi :67 compare-procedures)
     (srfi :69)
     (srfi :69 basic-hash-tables)
     (srfi :78)
     (srfi :78 lightweight-testing)
     (srfi :78 lightweight-testing compat)
     (srfi :8)
     (srfi :8 receive)
     (srfi :9)
     (srfi :9 records)
     (srfi :98)
     (srfi :98 os-environment-variables)
     (srfi :99)
     (srfi :99 records)
     (srfi :99 records helper)
     (srfi :99 records inspection)
     (srfi :99 records procedural)
     (srfi :99 records syntactic)
     (srfi private OS-id-features)
     (srfi private check-arg)
     (srfi private define-values)
     (srfi private feature-cond)
     (srfi private helpers)
     (srfi private include)
     (srfi private include compat)
     (srfi private include read)
     (srfi private let-opt)
     (srfi private platform-features)
     (srfi private registry)
     (srfi private registry-names)
     (srfi private vanish)
     (srfi srfi-0)
     (srfi srfi-48 compat)
     (srfi srfi-78 compat)
     (test_utils)
     (xitomatl AS-match)
     (xitomatl IU-match)
     (xitomatl IU-match extras)
     (xitomatl IU-match macro-helpers)
     (xitomatl R6RS-bindings spec)
     (xitomatl R6RS-bindings utils)
     (xitomatl R6RS-lexer)
     (xitomatl R6RS-lexical-transformations)
     (xitomatl alists)
     (xitomatl box)
     (xitomatl byte-expressions)
     (xitomatl bytevectors)
     (xitomatl common)
     (xitomatl conditionals)
     (xitomatl conditions)
     (xitomatl conditions print-condition)
     (xitomatl control)
     (xitomatl coroutines)
     (xitomatl curry)
     (xitomatl datum-find)
     (xitomatl debug)
     (xitomatl define)
     (xitomatl define define-values)
     (xitomatl define extras)
     (xitomatl delimited-control)
     (xitomatl engines)
     (xitomatl enumerators)
     (xitomatl environments)
     (xitomatl environments compat)
     (xitomatl exceptions)
     (xitomatl feature-cond)
     (xitomatl file-system base)
     (xitomatl file-system base compat)
     (xitomatl file-system paths)
     (xitomatl file-system value-file)
     (xitomatl fmt)
     (xitomatl fmt base)
     (xitomatl fmt c)
     (xitomatl fmt color)
     (xitomatl fmt column)
     (xitomatl fmt let-optionals*)
     (xitomatl fmt pretty)
     (xitomatl fmt srfi-33)
     (xitomatl fmt unicode)
     (xitomatl fuego)
     (xitomatl gcc-ast)
     (xitomatl generics)
     (xitomatl htmlprag)
     (xitomatl include)
     (xitomatl include compat)
     (xitomatl indexes)
     (xitomatl irregex)
     (xitomatl irregex counting)
     (xitomatl irregex extras)
     (xitomatl irregex-tool)
     (xitomatl keywords)
     (xitomatl keywords expand-time)
     (xitomatl keywords expand-time process-options)
     (xitomatl keywords parser)
     (xitomatl lang)
     (xitomatl lexer)
     (xitomatl library-utils)
     (xitomatl lists)
     (xitomatl lists compat)
     (xitomatl logic)
     (xitomatl logic relations)
     (xitomatl logic stream)
     (xitomatl macro-utils)
     (xitomatl macro-utils fib)
     (xitomatl macro-utils fib ctxt)
     (xitomatl macro-utils fib p-ctxt)
     (xitomatl match)
     (xitomatl numeral-system balanced-nonary)
     (xitomatl persistence base)
     (xitomatl persistence transcoded-serializing)
     (xitomatl ports)
     (xitomatl ports compat)
     (xitomatl predicates)
     (xitomatl profiler meta)
     (xitomatl profiler srfi-time)
     (xitomatl queue)
     (xitomatl records)
     (xitomatl regexp)
     (xitomatl repl)
     (xitomatl repl compat)
     (xitomatl rnrs-profiled)
     (xitomatl rnrs-restricted)
     (xitomatl ssax html)
     (xitomatl ssax multi-parser)
     (xitomatl ssax parsing)
     (xitomatl ssax private-5-1 define-opt)
     (xitomatl ssax private-5-1 error)
     (xitomatl ssax private-5-1 input-parse)
     (xitomatl ssax private-5-1 look-for-str)
     (xitomatl ssax private-5-1 misc)
     (xitomatl ssax private-5-1 output)
     (xitomatl ssax private-5-1 to-html)
     (xitomatl ssax private-5-1 to-html-ext)
     (xitomatl ssax private-5-1 util)
     (xitomatl ssax raise)
     (xitomatl ssax sxpath)
     (xitomatl ssax tree-trans)
     (xitomatl stack-lang)
     (xitomatl stack-lang core)
     (xitomatl stack-lang unsafe)
     (xitomatl strings)
     (xitomatl strings compat)
     (xitomatl sxml-match)
     (xitomatl sxml-match void)
     (xitomatl sxml-tools ddo-axes)
     (xitomatl sxml-tools ddo-txpath)
     (xitomatl sxml-tools fragments)
     (xitomatl sxml-tools guides)
     (xitomatl sxml-tools lazy-xpath)
     (xitomatl sxml-tools modif)
     (xitomatl sxml-tools serializer)
     (xitomatl sxml-tools stx-engine)
     (xitomatl sxml-tools sxml-tools)
     (xitomatl sxml-tools sxpath)
     (xitomatl sxml-tools sxpath-ext)
     (xitomatl sxml-tools sxpath-plus)
     (xitomatl sxml-tools sxpathlib)
     (xitomatl sxml-tools txpath)
     (xitomatl sxml-tools xlink-parser)
     (xitomatl sxml-tools xpath-ast)
     (xitomatl sxml-tools xpath-context--xlink)
     (xitomatl sxml-tools xpath-parser)
     (xitomatl vectors)
     (xitomatl zipper base)
     (xitomatl zipper trees)))
 (define installed-assets
   '(((include/resolve ("srfi" "%3a1") "srfi-1-reference.scm")
      ("srfi/%3a1/srfi-1-reference.scm")
      (srfi :1 lists))
     ((include/resolve ("srfi" "%3a115") "regexp-impl.scm")
      ("srfi/%3a115/regexp-impl.scm")
      (srfi :115 regexp))
     ((include/resolve ("srfi" "%3a115") "regexp-impl.scm")
      ("srfi/%3a115/regexp-impl.scm")
      (srfi srfi-115))
     ((include/resolve ("srfi" "%3a115" "regexp") "boundary-impl.scm")
      ("srfi/%3a115/regexp/boundary-impl.scm")
      (srfi :115 regexp boundary))
     ((include/resolve ("srfi" "%3a115" "regexp") "boundary-impl.scm")
      ("srfi/%3a115/regexp/boundary-impl.scm")
      (srfi srfi-115 boundary))
     ((include/resolve ("srfi" "%3a117") "list-queues-impl.scm")
      ("srfi/%3a117/list-queues-impl.scm")
      (srfi :117 list-queues))
     ((include/resolve ("srfi" "%3a117") "list-queues-impl.scm")
      ("srfi/%3a117/list-queues-impl.scm")
      (srfi srfi-117))
     ((include/resolve ("srfi" "%3a125") "125.body.scm")
      ("srfi/%3a125/125.body.scm")
      (srfi :125 hashtables))
     ((include/resolve ("srfi" "%3a125") "125.body.scm")
      ("srfi/%3a125/125.body.scm")
      (srfi srfi-125))
     ((include/resolve ("srfi" "%3a126") "126.body.scm")
      ("srfi/%3a126/126.body.scm")
      (srfi :126 r6rs-hashtables))
     ((include/resolve ("srfi" "%3a126") "126.body.scm")
      ("srfi/%3a126/126.body.scm")
      (srfi srfi-126))
     ((include/resolve ("srfi" "%3a127") "lseqs-impl.scm")
      ("srfi/%3a127/lseqs-impl.scm")
      (srfi :127 lazy-sequences))
     ((include/resolve ("srfi" "%3a127") "lseqs-impl.scm")
      ("srfi/%3a127/lseqs-impl.scm")
      (srfi srfi-127))
     ((include/resolve ("srfi" "%3a128") "128.body1.scm")
      ("srfi/%3a128/128.body1.scm")
      (srfi :128 comparators))
     ((include/resolve ("srfi" "%3a128") "128.body1.scm")
      ("srfi/%3a128/128.body1.scm")
      (srfi srfi-128))
     ((include/resolve ("srfi" "%3a128") "128.body2.scm")
      ("srfi/%3a128/128.body2.scm")
      (srfi :128 comparators))
     ((include/resolve ("srfi" "%3a128") "128.body2.scm")
      ("srfi/%3a128/128.body2.scm")
      (srfi srfi-128))
     ((include/resolve ("srfi" "%3a129") "titlecase-impl.scm")
      ("srfi/%3a129/titlecase-impl.scm")
      (srfi :129 titlecase))
     ((include/resolve ("srfi" "%3a129") "titlecase-impl.scm")
      ("srfi/%3a129/titlecase-impl.scm")
      (srfi srfi-129))
     ((include/resolve ("srfi" "%3a129") "titlemaps.scm")
      ("srfi/%3a129/titlemaps.scm")
      (srfi :129 titlecase))
     ((include/resolve ("srfi" "%3a129") "titlemaps.scm")
      ("srfi/%3a129/titlemaps.scm")
      (srfi srfi-129))
     ((include/resolve ("srfi" "%3a13") "srfi-13.scm")
      ("srfi/%3a13/srfi-13.scm")
      (srfi :13 strings))
     ((include/resolve ("srfi" "%3a130") "130.body.scm")
      ("srfi/%3a130/130.body.scm")
      (srfi :130 string-cursors))
     ((include/resolve ("srfi" "%3a130") "130.body.scm")
      ("srfi/%3a130/130.body.scm")
      (srfi srfi-130))
     ((include/resolve ("srfi" "%3a132") "delndups.scm")
      ("srfi/%3a132/delndups.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "delndups.scm")
      ("srfi/%3a132/delndups.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "lmsort.scm")
      ("srfi/%3a132/lmsort.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "lmsort.scm")
      ("srfi/%3a132/lmsort.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "median.scm")
      ("srfi/%3a132/median.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "median.scm")
      ("srfi/%3a132/median.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "sort.scm")
      ("srfi/%3a132/sort.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "sort.scm")
      ("srfi/%3a132/sort.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "sortp.scm")
      ("srfi/%3a132/sortp.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "sortp.scm")
      ("srfi/%3a132/sortp.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "vector-util.scm")
      ("srfi/%3a132/vector-util.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "vector-util.scm")
      ("srfi/%3a132/vector-util.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "vhsort.scm")
      ("srfi/%3a132/vhsort.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "vhsort.scm")
      ("srfi/%3a132/vhsort.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "visort.scm")
      ("srfi/%3a132/visort.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "visort.scm")
      ("srfi/%3a132/visort.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "vmsort.scm")
      ("srfi/%3a132/vmsort.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "vmsort.scm")
      ("srfi/%3a132/vmsort.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a132") "vqsort2.scm")
      ("srfi/%3a132/vqsort2.scm")
      (srfi :132 sorting))
     ((include/resolve ("srfi" "%3a132") "vqsort2.scm")
      ("srfi/%3a132/vqsort2.scm")
      (srfi srfi-132))
     ((include/resolve ("srfi" "%3a133") "vectors-impl.scm")
      ("srfi/%3a133/vectors-impl.scm")
      (srfi :133 vectors))
     ((include/resolve ("srfi" "%3a133") "vectors-impl.scm")
      ("srfi/%3a133/vectors-impl.scm")
      (srfi srfi-133))
     ((include/resolve ("srfi" "%3a14") "srfi-14-base-char-sets.scm")
      ("srfi/%3a14/srfi-14-base-char-sets.scm")
      (srfi :14 char-sets))
     ((include/resolve ("srfi" "%3a14") "srfi-14-char-sets.scm")
      ("srfi/%3a14/srfi-14-char-sets.scm")
      (srfi :14 char-sets))
     ((include/resolve ("srfi" "%3a14") "srfi-14.scm")
      ("srfi/%3a14/srfi-14.scm")
      (srfi :14 char-sets))
     ((include/resolve ("srfi" "%3a14" "char-sets") "inversion-list-impl.scm")
      ("srfi/%3a14/char-sets/inversion-list-impl.scm")
      (srfi :14 char-sets inversion-list))
     ((include/resolve ("srfi" "%3a141") "srfi-141-impl.scm")
      ("srfi/%3a141/srfi-141-impl.scm")
      (srfi :141 integer-division))
     ((include/resolve ("srfi" "%3a141") "srfi-141-impl.scm")
      ("srfi/%3a141/srfi-141-impl.scm")
      (srfi srfi-141))
     ((include/resolve ("srfi" "%3a152") "extend-comparisons.scm")
      ("srfi/%3a152/extend-comparisons.scm")
      (srfi :152 strings))
     ((include/resolve ("srfi" "%3a152") "extend-comparisons.scm")
      ("srfi/%3a152/extend-comparisons.scm")
      (srfi srfi-152))
     ((include/resolve ("srfi" "%3a152") "macros.scm")
      ("srfi/%3a152/macros.scm")
      (srfi :152 strings))
     ((include/resolve ("srfi" "%3a152") "macros.scm")
      ("srfi/%3a152/macros.scm")
      (srfi srfi-152))
     ((include/resolve ("srfi" "%3a152") "portable.scm")
      ("srfi/%3a152/portable.scm")
      (srfi :152 strings))
     ((include/resolve ("srfi" "%3a152") "portable.scm")
      ("srfi/%3a152/portable.scm")
      (srfi srfi-152))
     ((include/resolve ("srfi" "%3a152") "r7rs-shim.scm")
      ("srfi/%3a152/r7rs-shim.scm")
      (srfi :152 strings))
     ((include/resolve ("srfi" "%3a152") "r7rs-shim.scm")
      ("srfi/%3a152/r7rs-shim.scm")
      (srfi srfi-152))
     ((include/resolve ("srfi" "%3a156") "srfi-156-impl.scm")
      ("srfi/%3a156/srfi-156-impl.scm")
      (srfi :156 predicate-combiners))
     ((include/resolve ("srfi" "%3a156") "srfi-156-impl.scm")
      ("srfi/%3a156/srfi-156-impl.scm")
      (srfi srfi-156))
     ((include/resolve ("srfi" "%3a158") "srfi-158-impl.scm")
      ("srfi/%3a158/srfi-158-impl.scm")
      (srfi :158 generators-and-accumulators))
     ((include/resolve ("srfi" "%3a165") "implementation.scm")
      ("srfi/%3a165/implementation.scm")
      (srfi :165))
     ((include/resolve ("srfi" "%3a166") "base.scm")
      ("srfi/%3a166/base.scm")
      (srfi :166 base))
     ((include/resolve ("srfi" "%3a166") "color.scm")
      ("srfi/%3a166/color.scm")
      (srfi :166 color))
     ((include/resolve ("srfi" "%3a166") "column.scm")
      ("srfi/%3a166/column.scm")
      (srfi :166 columnar))
     ((include/resolve ("srfi" "%3a166") "pretty.scm")
      ("srfi/%3a166/pretty.scm")
      (srfi :166 pretty))
     ((include/resolve ("srfi" "%3a166") "show.scm")
      ("srfi/%3a166/show.scm")
      (srfi :166 base))
     ((include/resolve ("srfi" "%3a166") "unicode.scm")
      ("srfi/%3a166/unicode.scm")
      (srfi :166 unicode))
     ((include/resolve ("srfi" "%3a166") "width.scm")
      ("srfi/%3a166/width.scm")
      (srfi :166 unicode))
     ((include/resolve ("srfi" "%3a166") "write.scm")
      ("srfi/%3a166/write.scm")
      (srfi :166 base))
     ((include/resolve ("srfi" "%3a19") "srfi-19.scm")
      ("srfi/%3a19/srfi-19.scm")
      (srfi :19 time))
     ((include/resolve ("srfi" "%3a224") "224.scm")
      ("srfi/%3a224/224.scm")
      (srfi :224))
     ((include/resolve ("srfi" "%3a224") "matchers.scm")
      ("srfi/%3a224/matchers.scm")
      (srfi :224))
     ((include/resolve ("srfi" "%3a224") "trie.scm")
      ("srfi/%3a224/trie.scm")
      (srfi :224))
     ((include/resolve ("srfi" "%3a25") "arlib.scm")
      ("srfi/%3a25/arlib.scm")
      (srfi :25 multi-dimensional-arrays arlib))
     ((include/resolve ("srfi" "%3a25") "arlib.scm")
      ("srfi/%3a25/arlib.scm")
      (srfi srfi-25 arlib))
     ((include/resolve ("srfi" "%3a25") "array.scm")
      ("srfi/%3a25/array.scm")
      (srfi :25 multi-dimensional-arrays all))
     ((include/resolve ("srfi" "%3a25") "array.scm")
      ("srfi/%3a25/array.scm")
      (srfi srfi-25 all))
     ((include/resolve ("srfi" "%3a25") "ix-ctor.scm")
      ("srfi/%3a25/ix-ctor.scm")
      (srfi :25 multi-dimensional-arrays all))
     ((include/resolve ("srfi" "%3a25") "ix-ctor.scm")
      ("srfi/%3a25/ix-ctor.scm")
      (srfi srfi-25 all))
     ((include/resolve ("srfi" "%3a25") "op-ctor.scm")
      ("srfi/%3a25/op-ctor.scm")
      (srfi :25 multi-dimensional-arrays all))
     ((include/resolve ("srfi" "%3a25") "op-ctor.scm")
      ("srfi/%3a25/op-ctor.scm")
      (srfi srfi-25 all))
     ((include/resolve ("srfi" "%3a26") "cut-impl.scm")
      ("srfi/%3a26/cut-impl.scm")
      (srfi :26 cut))
     ((include/resolve ("srfi" "%3a27") "mrg32k3a-a.scm")
      ("srfi/%3a27/mrg32k3a-a.scm")
      (srfi :27 random-bits))
     ((include/resolve ("srfi" "%3a27") "mrg32k3a.scm")
      ("srfi/%3a27/mrg32k3a.scm")
      (srfi :27 random-bits))
     ((include/resolve ("srfi" "%3a37") "srfi-37-reference.scm")
      ("srfi/%3a37/srfi-37-reference.scm")
      (srfi :37 args-fold))
     ((include/resolve ("srfi" "%3a42") "ec.scm")
      ("srfi/%3a42/ec.scm")
      (srfi :42 eager-comprehensions))
     ((include/resolve ("srfi" "%3a43") "vector-lib.scm")
      ("srfi/%3a43/vector-lib.scm")
      (srfi :43 vectors))
     ((include/resolve ("srfi" "%3a51") "srfi-51-impl.scm")
      ("srfi/%3a51/srfi-51-impl.scm")
      (srfi :51 rest-values))
     ((include/resolve ("srfi" "%3a51") "srfi-51-impl.scm")
      ("srfi/%3a51/srfi-51-impl.scm")
      (srfi srfi-51))
     ((include/resolve ("srfi" "%3a54") "srfi-54-impl.scm")
      ("srfi/%3a54/srfi-54-impl.scm")
      (srfi :54 cat))
     ((include/resolve ("srfi" "%3a54") "srfi-54-impl.scm")
      ("srfi/%3a54/srfi-54-impl.scm")
      (srfi srfi-54))
     ((include/resolve ("srfi" "%3a64") "testing-impl.scm")
      ("srfi/%3a64/testing-impl.scm")
      (srfi :64 testing))
     ((include/resolve ("srfi" "%3a67") "compare.scm")
      ("srfi/%3a67/compare.scm")
      (srfi :67 compare-procedures))
     ((include/resolve ("srfi" "%3a78") "check.scm")
      ("srfi/%3a78/check.scm")
      (srfi :78 lightweight-testing))
     ((include/resolve ("srfi" "%3a78") "check.scm")
      ("srfi/%3a78/check.scm")
      (srfi srfi-78))
     ((include/resolve ("xitomatl" "fmt") "fmt-c.scm")
      ("xitomatl/fmt/fmt-c.scm")
      (xitomatl fmt c))
     ((include/resolve ("xitomatl" "fmt") "fmt-color.scm")
      ("xitomatl/fmt/fmt-color.scm")
      (xitomatl fmt color))
     ((include/resolve ("xitomatl" "fmt") "fmt-column.scm")
      ("xitomatl/fmt/fmt-column.scm")
      (xitomatl fmt column))
     ((include/resolve ("xitomatl" "fmt") "fmt-pretty.scm")
      ("xitomatl/fmt/fmt-pretty.scm")
      (xitomatl fmt pretty))
     ((include/resolve ("xitomatl" "fmt") "fmt-unicode.scm")
      ("xitomatl/fmt/fmt-unicode.scm")
      (xitomatl fmt unicode))
     ((include/resolve ("xitomatl" "fmt") "fmt.scm")
      ("xitomatl/fmt/fmt.scm")
      (xitomatl fmt base))
     ((include/resolve ("xitomatl" "fmt") "srfi-33.scm")
      ("xitomatl/fmt/srfi-33.scm")
      (xitomatl fmt srfi-33))
     ((include/resolve ("xitomatl" "htmlprag") "htmlprag.scm")
      ("xitomatl/htmlprag/htmlprag.scm")
      (xitomatl htmlprag))
     ((include/resolve ("xitomatl" "irregex") "irregex-r6rs.scm")
      ("xitomatl/irregex/irregex-r6rs.scm")
      (xitomatl irregex))
     ((include/resolve ("xitomatl" "irregex") "irregex-utils.scm")
      ("xitomatl/irregex/irregex-utils.scm")
      (xitomatl irregex))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "SSAX.scm")
      ("xitomatl/ssax/private-5-1/SSAX.scm")
      (xitomatl ssax parsing))
     ((include/resolve
       ("xitomatl" "ssax" "private-5-1")
       "SXML-to-HTML-ext.scm")
      ("xitomatl/ssax/private-5-1/SXML-to-HTML-ext.scm")
      (xitomatl ssax private-5-1 to-html-ext))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-to-HTML.scm")
      ("xitomatl/ssax/private-5-1/SXML-to-HTML.scm")
      (xitomatl ssax private-5-1 to-html))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-tree-trans.scm")
      ("xitomatl/ssax/private-5-1/SXML-tree-trans.scm")
      (xitomatl ssax tree-trans))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "SXPath-old.scm")
      ("xitomatl/ssax/private-5-1/SXPath-old.scm")
      (xitomatl ssax sxpath))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "define-opt.scm")
      ("xitomatl/ssax/private-5-1/define-opt.scm")
      (xitomatl ssax private-5-1 define-opt))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "input-parse.scm")
      ("xitomatl/ssax/private-5-1/input-parse.scm")
      (xitomatl ssax private-5-1 input-parse))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "look-for-str.scm")
      ("xitomatl/ssax/private-5-1/look-for-str.scm")
      (xitomatl ssax private-5-1 look-for-str))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "output.scm")
      ("xitomatl/ssax/private-5-1/output.scm")
      (xitomatl ssax private-5-1 output))
     ((include/resolve ("xitomatl" "ssax" "private-5-1") "util.scm")
      ("xitomatl/ssax/private-5-1/util.scm")
      (xitomatl ssax private-5-1 util))
     ((include/resolve ("xitomatl" "ssax" "private-plt") "id.ss")
      ("xitomatl/ssax/private-plt/id.ss")
      (xitomatl ssax multi-parser))
     ((include/resolve ("xitomatl" "ssax" "private-plt") "multi-parser.ss")
      ("xitomatl/ssax/private-plt/multi-parser.ss")
      (xitomatl ssax multi-parser))
     ((include/resolve ("xitomatl" "ssax" "private-plt") "ssax-prim.ss")
      ("xitomatl/ssax/private-plt/ssax-prim.ss")
      (xitomatl ssax multi-parser))
     ((include/resolve ("xitomatl" "sxml-match") "sxml-match.ss")
      ("xitomatl/sxml-match/sxml-match.ss")
      (xitomatl sxml-match))
     ((include/resolve ("xitomatl" "sxml-tools") "ddo-axes.scm")
      ("xitomatl/sxml-tools/ddo-axes.scm")
      (xitomatl sxml-tools ddo-axes))
     ((include/resolve ("xitomatl" "sxml-tools") "ddo-txpath.scm")
      ("xitomatl/sxml-tools/ddo-txpath.scm")
      (xitomatl sxml-tools ddo-txpath))
     ((include/resolve ("xitomatl" "sxml-tools") "fragments.scm")
      ("xitomatl/sxml-tools/fragments.scm")
      (xitomatl sxml-tools fragments))
     ((include/resolve ("xitomatl" "sxml-tools") "guides.scm")
      ("xitomatl/sxml-tools/guides.scm")
      (xitomatl sxml-tools guides))
     ((include/resolve ("xitomatl" "sxml-tools") "lazy-xpath.scm")
      ("xitomatl/sxml-tools/lazy-xpath.scm")
      (xitomatl sxml-tools lazy-xpath))
     ((include/resolve ("xitomatl" "sxml-tools") "modif.scm")
      ("xitomatl/sxml-tools/modif.scm")
      (xitomatl sxml-tools modif))
     ((include/resolve ("xitomatl" "sxml-tools") "serializer.scm")
      ("xitomatl/sxml-tools/serializer.scm")
      (xitomatl sxml-tools serializer))
     ((include/resolve ("xitomatl" "sxml-tools") "stx-engine.scm")
      ("xitomatl/sxml-tools/stx-engine.scm")
      (xitomatl sxml-tools stx-engine))
     ((include/resolve ("xitomatl" "sxml-tools") "sxml-tools.scm")
      ("xitomatl/sxml-tools/sxml-tools.scm")
      (xitomatl sxml-tools sxml-tools))
     ((include/resolve ("xitomatl" "sxml-tools") "sxpath-ext.scm")
      ("xitomatl/sxml-tools/sxpath-ext.scm")
      (xitomatl sxml-tools sxpath-ext))
     ((include/resolve ("xitomatl" "sxml-tools") "sxpath-plus.scm")
      ("xitomatl/sxml-tools/sxpath-plus.scm")
      (xitomatl sxml-tools sxpath-plus))
     ((include/resolve ("xitomatl" "sxml-tools") "sxpath.scm")
      ("xitomatl/sxml-tools/sxpath.scm")
      (xitomatl sxml-tools sxpath))
     ((include/resolve ("xitomatl" "sxml-tools") "sxpathlib.scm")
      ("xitomatl/sxml-tools/sxpathlib.scm")
      (xitomatl sxml-tools sxpathlib))
     ((include/resolve ("xitomatl" "sxml-tools") "txpath.scm")
      ("xitomatl/sxml-tools/txpath.scm")
      (xitomatl sxml-tools txpath))
     ((include/resolve ("xitomatl" "sxml-tools") "xlink-parser.scm")
      ("xitomatl/sxml-tools/xlink-parser.scm")
      (xitomatl sxml-tools xlink-parser))
     ((include/resolve ("xitomatl" "sxml-tools") "xlink.scm")
      ("xitomatl/sxml-tools/xlink.scm")
      (xitomatl sxml-tools xpath-context--xlink))
     ((include/resolve ("xitomatl" "sxml-tools") "xpath-ast.scm")
      ("xitomatl/sxml-tools/xpath-ast.scm")
      (xitomatl sxml-tools xpath-ast))
     ((include/resolve ("xitomatl" "sxml-tools") "xpath-context.scm")
      ("xitomatl/sxml-tools/xpath-context.scm")
      (xitomatl sxml-tools xpath-context--xlink))
     ((include/resolve ("xitomatl" "sxml-tools") "xpath-parser.scm")
      ("xitomatl/sxml-tools/xpath-parser.scm")
      (xitomatl sxml-tools xpath-parser)))))
