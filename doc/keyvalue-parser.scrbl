#lang scribble/manual

@(require scribble/eval
          (for-label "../lib/keyvalue-parser.rkt"))



@defmodule[lib/keyvalue-parser]

@;@(define helper-eval (make-base-eval))
@;@interaction-eval[
@;  #:eval helper-eval
@;  (define data "User: 256413\nBatteryLevel: 15\nOS: FreeBSD\n\nUserID: 65542\nBatteryLevel: 85\nOS: GNU\n")  
@;]

@title[#:tag "keyvalue"]{Parsing key-value files}

Reading data from a key-value file is a recurring task in many data science projects.
Indeed, in a data collecting campaign we might often find it convenient to store some information in
key-value format, as in the following example.

@filebox["data.txt"]{
  @verbatim[#:indent 1]{
    UserID: 256413
    BatteryLevel: 15
    OS: FreeBSD

    UserID: 65542
    BatteryLevel: 85
    OS: GNU
}}

To parse files like this, this library provides the function @tt{read-keyvalue-file}.

@defproc[(read-keyvalue-file [filename string?]
                             [#:mode mode-flag (or/c 'binary 'text) 'text])
         (stream?)]{
It is not necessary that a file has exactly the same fields for every data point. Also, blank lines
between each data point are not necessary.
This function will keep reading the file @filepath{filename}, progressively building a hash map,
until one of the keys in the file is already seen in the current hash. At this stage, it will return
the last complete hash map and wait for the next call to @tt{stream-first}.

@racketblock[
  (define data (read-keyvalue-file "data.txt"))
  (for/list ((d data))
  (for (((key value) (in-hash d)))
    (printf "Key: ~a --- Value: ~a\n" key value)))
]
@verbatim|{
Key: OS --- Value:  GNU
Key: BatteryLevel --- Value:  85
Key: OS --- Value:  FreeBSD
Key: UserID --- Value:  65542
Key: BatteryLevel --- Value:  15
Key: User --- Value:  256413
'(#<void> #<void>)
}|

}