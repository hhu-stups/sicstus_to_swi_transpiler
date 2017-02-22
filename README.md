# A Transpiler from Sicstus to Swi Prolog

## Project structure
```
|->src ::> source code
| |->swi_prolog_extension ::> extension modules for missing functionality in SWI
| |  |-assoc_extension.pl
| |-transpiler_avl.pl
| |-transpiler_core.pl
| |-transpiler.pl
|
|->tests
| |->sicstuscode ::> prolog files containing Sicstus  prolog for testing; used by test_transpiler module
| | |-...
| |->swi_prolog_extension_tests
| | |-...
| |-test_transpiler_core.pl
| |-test_transpiler.pl
|-> thesis ::> bachelor thesis
|-README.md
```
## Process

### Libraries
cf. chapter 10 Sicstus user manual

|Library|SWI counterpart/Transpilation       |
|-------|------------------------------------|
|avl    |assoc library/assoc_extension module|
