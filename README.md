# field-trip (Scheme-in-C)

In this repository I explore many things around implementing semantics, including
* the gist of ParentheC
* Garbage Collection
* Foreign Function Interface

What I have done includes
* a C program that interpreter a C data structure that correspond to a very small unityped lambda calculus (similar to C311 A9)
* a Racket program that generate most part of the C program (including code for a mark-and-sweep GC). This program is similar to ParentheC.
* a Racket program that compile a bigger language to the small language

What I would like to do includes
* writing an interpreter in the big language to the small language, which will give me an interpreter for the big langauge in C.
* semi-space copying GC
