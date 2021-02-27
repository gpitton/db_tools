#lang scribble/manual

@(require (for-label "../lib/hardware-monitor.rkt"))


@defmodule[lib/hardware-monitor]

@title[#:tag "monitor"]{Monitoring hardware resources}

It is sometimes useful to register the memory and cpu utilisation data on a log file.
We provide the function @tt{start-log} for doing precisely this in a simple way.

@defproc[(start-log [filename string? "resources.log"]
                    [#:time-step time-step-flag positive? 0.5]
                    [#:mode mode-flag (or/c 'binary 'text) 'text]
                    [#:exists exists-flag
                     (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace) 'replace])
         any]{
The resulting log file has three columns separated by a white space.
The first two columns are user and system cpu utilisation, the third one is memory usage.
Every measurement is stored sequentially as a row in the output file, and the default
sampling frequency is of 2Hz (two measurements every second).

If you would like to just monitor system's resources while another process is running,
a quick way to do so is to run the racket interpreter, require @tt{lib/hardware-monitor},
and call @tt{(start-log)}.
Then when it is no longer needed to track the resource utilisation, just exit the interpreter.
}