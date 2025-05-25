# Camlkit bindings generator

Generate [Camlkit](https://github.com/dboris/camlkit) bindings for Objective-C
classes and C globals of Cocoa frameworks.

## Example usage

* List libraries registered with the Objective-C runtime:
```
dune exec ckgen -- inspect --libs
```
* Generate bindings for C globals:
```
mkdir -p data/CoreGraphics
dune exec ckgen -- globals CoreGraphics \
  --open-modules=CoreFoundation \
  < /System/Library/Frameworks/CoreGraphics.framework/Resources/BridgeSupport/CoreGraphics.bridgesupport \
  > data/CoreGraphics/CoreGraphics_globals.ml
```
* Generate bindings for Objective-C classes:
```
mkdir -p data/Foundation
cd data/Foundation && dune exec ckgen -- classes Foundation \
  --all-in /System/Library/Frameworks/Foundation.framework/Versions/C/Foundation \
  --open-modules=CoreFoundation,CoreAnimation
```

### Command reference

```
NAME
       ckgen-classes - Generate bindings for Objective-C classes and protocols

SYNOPSIS
       ckgen classes [OPTION]… FW_NAME

ARGUMENTS
       FW_NAME (required)
           Framework name of the generated bindings.

OPTIONS
       -a LIB, --all-in=LIB
           Generate bindings for classes in LIB.

       -d CLASS, --method-defs-in=CLASS
           Generate method definitions of CLASS.

       -f FILE_NAME, --filter=FILE_NAME
           Filename with class names to keep, one class name per line.

       -l FW_PATH, --load=FW_PATH
           Load framework bundle specified by FW_PATH.

       -m CLASS, --methods-in=CLASS
           Generate bindings for methods of CLASS.

       --meta
           Generate method definitions for the metaclass.

       -o M1,M2,..., --open-modules=M1,M2,...
           Comma-separated list of modules to open in generated code.

       -p, --proto
           Generate protocols registered in the Runtime.

       -s, --super
           Include superclass methods in generated module.
```
---
```
NAME
       ckgen-globals - Generate bindings for C globals using the framework
       "bridgesupport" file

SYNOPSIS
       ckgen globals [--open-modules=M1,M2,...] [--verbose] [OPTION]… FW_NAME

ARGUMENTS
       FW_NAME (required)
           Framework name of the generated bindings.

OPTIONS
       -o M1,M2,..., --open-modules=M1,M2,...
           Comma-separated list of modules to open in generated code.

       -v, --verbose
           Verbose error output.
```
---
```
NAME
       ckgen-inspect - Inspect the Objective-C runtime for loaded frameworks,
       classes, methods

SYNOPSIS
       ckgen inspect [OPTION]…

OPTIONS
       -c LIB, --classes-in=LIB
           Show classes in LIB.

       -i CLASS, --image-for=CLASS
           Show library image where CLASS is defined.

       -l FW_PATH, --load=FW_PATH
           Load framework bundle specified by FW_PATH.

       --libs
           Show loaded frameworks and libraries.

       -m CLASS, --methods-in=CLASS
           Show methods in CLASS.
```