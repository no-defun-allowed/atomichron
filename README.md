# An atomic metering library.

atomichron is yet another library we yoinked out of
decentralise2-utilities. It currently implements a time meter, which
tracks how many times a form is evaluated, how long evaluation takes,
and the standard deviation of execution time. It uses atomic
instructions so that meters will present correct results in the
presence of multiple threads, while trying to minimize synchronisation
latency.

We have used this code to meter phenomena in decentralise2 which may
occur on the scale of tens to hundreds of nanoseconds. Atomics aren't
free, but metering anything but the tightest of loops should not pose
any performance issues.

The time meter is inspired by [the meter the Cleavir compiler
framework uses](https://github.com/robert-strandh/SICL/blob/master/Code/Cleavir/Meter/meter.lisp)
which took the general idea from Multics. If one is uncertain about
using this library, [The Instrumentation of
Multics](https://multicians.org/InstrumentationPaper.html) suggests
"building permanent instrumentation into key supervisor modules is
well worth the effort, since the cost of maintaining well-organized
instrumentation is low, and the payoff in being able to "look at the
meters" any time a performance problem is suspected or even when one
is not is very high." 

If one really thinks metering will be too slow, they may also push
`:disable-meters` to `*features*` and recompile to disable metering.
However, we are confident that metering has negligible time overhead.

## Time meters

Macro **define-time-meter** *name* *description* *group*

Create a time meter bound to the variable *name*, with the given 
description (a string) and group (an unevaluated symbol). It is
an error to re-bind the variable used.

Macro **with-time-meter** (*name*) &rest *body*

Evaluate *body* in an implicit progn, then update the meter bound to
*name*.

Function **print-meters** &key *groups* *stream*

Print information about all time meters in the given group to the
given stream. If *groups* is not provided or is the empty list, all
groups are used. If *stream* is not provided, output is written to
\*standard-output\*.

Function **reset-meters** *groups*

Reset the values of all time meters in the given groups. If *groups*
is the empty list, all groups are used.

## Todo

- Implement a latency histogram meter a la 
  [HdrHistogram](http://www.hdrhistogram.org/)
