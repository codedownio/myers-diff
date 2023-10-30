myers-diff-0.2.0.0: unregistering (components added: bench:myers-diff-weigh)
myers-diff> build (lib + bench)
Preprocessing library for myers-diff-0.2.0.0..
Building library for myers-diff-0.2.0.0..
Preprocessing benchmark 'myers-diff-weigh' for myers-diff-0.2.0.0..
Building benchmark 'myers-diff-weigh' for myers-diff-0.2.0.0..
[1 of 8] Compiling Paths_myers_diff [Source file changed]
[2 of 8] Compiling TestLib.Apply [Flags changed]
[3 of 8] Compiling TestLib.Generators [Flags changed]
[4 of 8] Compiling TestLib.Instances [Flags changed]
[5 of 8] Compiling TestLib.Benchmarking [Flags changed]
[6 of 8] Compiling Main [Flags changed]
[7 of 8] Compiling TestLib.Util [Flags changed]
[8 of 8] Compiling TestLib.VectorIO [Flags changed]
[9 of 9] Linking .stack-work/dist/x86_64-linux-nix/Cabal-3.8.1.0/build/myers-diff-weigh/myers-diff-weigh [Objects changed]
myers-diff> copy/register
Installing library in /home/tom/tools/myers-diff/.stack-work/install/x86_64-linux-nix/6197b2d233f6b9d554c760249dda0539e52a94fad180e5769470576c0a2a7dd2/9.4.4/lib/x86_64-linux-ghc-9.4.4/myers-diff-0.2.0.0-5CCUivd0dIY5KA74IfAqVJ
Registering library for myers-diff-0.2.0.0..
myers-diff> benchmarks
Running 1 benchmarks...
Benchmark myers-diff-weigh: RUNNING...

#Single insert (100 samples each)

#10 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|1,247,440|0|
|myers-diff-string|1,384,896|0|
|myers-diff-text|1,252,128|0|
|Diff|10,380,104|2|

#100 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|2,602,600|0|
|myers-diff-string|2,768,528|0|
|myers-diff-text|2,625,496|0|
|Diff|10,236,352|2|

#1000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|11,183,584|2|
|myers-diff-string|12,413,232|2|
|myers-diff-text|12,645,176|2|
|Diff|32,527,736|7|

#10000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|96,097,256|19|
|myers-diff-string|120,667,872|21|
|myers-diff-text|106,282,928|19|
|Diff|261,987,664|60|

#100000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|930,477,784|185|
|myers-diff-string|1,082,037,168|191|
|myers-diff-text|1,045,369,752|184|
|Diff|2,448,416,192|567|

#Single delete (100 samples each)

#10 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|186,944|0|
|myers-diff-string|202,440|0|
|myers-diff-text|221,032|0|
|Diff|403,864|0|

#100 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|1,325,560|0|
|myers-diff-string|1,342,160|0|
|myers-diff-text|1,395,576|0|
|Diff|3,898,080|0|

#1000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|7,666,528|1|
|myers-diff-string|9,573,752|1|
|myers-diff-text|9,198,048|1|
|Diff|28,855,120|6|

#10000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|67,881,280|12|
|myers-diff-string|92,139,184|15|
|myers-diff-text|84,187,304|14|
|Diff|227,417,856|52|

#100000 characters

|Case|Allocated|GCs|
|:---|---:|---:|
|myers-diff-vector|674,589,512|124|
|myers-diff-string|888,319,096|143|
|myers-diff-text|905,887,008|150|
|Diff|2,499,935,040|579|
Benchmark myers-diff-weigh: FINISH
Completed 2 action(s).
