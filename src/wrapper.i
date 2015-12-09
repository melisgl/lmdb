%module wrapper

%{
#include <lmdb.h>
%}

%insert("lisphead") %{
(in-package :lmdb)
%}
%feature("intern_function", "swig-lispify");

%include "/usr/include/lmdb.h"
