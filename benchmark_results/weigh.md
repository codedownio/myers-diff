myers-diff-0.2.0.0: unregistering (components added: bench:myers-diff-weigh)
myers-diff> build (lib + bench)
Preprocessing library for myers-diff-0.2.0.0..
Building library for myers-diff-0.2.0.0..
Preprocessing benchmark 'myers-diff-weigh' for myers-diff-0.2.0.0..
Building benchmark 'myers-diff-weigh' for myers-diff-0.2.0.0..
myers-diff> copy/register
Installing library in /home/tom/tools/myers-diff/.stack-work/install/x86_64-linux-nix/6197b2d233f6b9d554c760249dda0539e52a94fad180e5769470576c0a2a7dd2/9.4.4/lib/x86_64-linux-ghc-9.4.4/myers-diff-0.2.0.0-Gb7SORFHk8i6ZUIC2JakLU
Registering library for myers-diff-0.2.0.0..
myers-diff> benchmarks
Running 1 benchmarks...
Benchmark myers-diff-weigh: RUNNING...

#Single insert (100 samples each)

#10 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|1,681,120|0|
|Diff|8,904,176|2|

#100 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|3,619,928|0|
|Diff|13,833,520|3|

#1000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|20,044,048|4|
|Diff|31,669,480|7|

#10000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|171,103,240|35|
|Diff|250,594,080|58|

#100000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|1,666,421,824|337|
|Diff|2,172,753,512|504|

#Single delete (100 samples each)

#10 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|310,760|0|
|Diff|451,992|0|

#100 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|2,203,608|0|
|Diff|5,448,872|1|

#1000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|17,821,272|3|
|Diff|30,562,200|7|

#10000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|177,047,320|35|
|Diff|253,055,344|58|

#100000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff|1,648,287,432|328|
|Diff|2,288,706,264|531|
Benchmark myers-diff-weigh: FINISH
Completed 2 action(s).
