mapgroup
========

Command-line utility to map a command to groups of consecutive lines matching a pattern

Example: `mapgroup '/usb/bin/sort --ignore-case --unique' '^#include|^using namespace' main.cc`

   sorts alphabetically, removing duplicates, `#include`s and `using namespace`s lines in C++.
   `mapgroup` takes into account groups of includes/namespaces so that if they are separated by
   whatever line different from those matching the pattern, then it is considered another group
   to be sort independently. Please, note that this example assumes a Unix-like environment where
   `sort` command is available.

Example: `mapgroup /usr/bin/uniq '^\s*$' Main.hs`

   removes consecutive empty lines (assuming `uniq` command is available in the system).
