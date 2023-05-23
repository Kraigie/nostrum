# benchmarks

This directory contains microbenchmarks that validate performance of various
parts of nostrum, primarily its caches.

If you feel something in nostrum could be improved in speed, this is the place
to start with!


## Resource usage

Benchmarks here are intended to benchmark nostrum under full, relatively big
deployments. Small microbenchmarks are also welcome, but especially the cache
backends try to push nostrum's caching to the limits to find any issues that
occur there. It is therefore recommended to not test on anything that you don't
expect to rapidly warm up your room in the next few minutes.


<!-- vim: set textwidth=80 sw=2 ts=2: -->
