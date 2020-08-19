<a id='x-28LMDB-3A-40LMDB-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# LMDB Manual

## Table of Contents

- [1 lmdb ASDF System Details][1e9b]
- [2 Links][4746]
- [3 Introduction][00ec]
- [4 Design and implementation][67a2]
    - [4.1 Safety][a9f7]
    - [4.2 Deviations from C lmdb API][cb1c]
- [5 Library versions][ac4c]
- [6 Environments][e4d5]
    - [6.1 Creating environments][22cc]
    - [6.2 Opening and closing environments][e12f]
    - [6.3 Miscellaneous environment functions][6c6e]
- [7 Transactions][7652]
    - [7.1 Nesting transactions][1fa4]
- [8 Databases][258b]
    - [8.1 The unnamed database][15a5]
    - [8.2 DUPSORT][6f4e]
    - [8.3 Database API][2fff]
- [9 Encoding and decoding data][3489]
    - [9.1 Special encodings][9697]
- [10 Basic operations][b471]
- [11 Cursors][d610]
    - [11.1 Positioning cursors][ed71]
    - [11.2 Basic cursor operations][3d52]
    - [11.3 Miscellaneous cursor operations][175f]
- [12 Conditions][097c]
    - [12.1 Conditions for C lmdb error codes][c260]
    - [12.2 Additional conditions][4850]

###### \[in package LMDB\]
<a id='x-28-23A-28-284-29-20BASE-CHAR-20-2E-20-22lmdb-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 lmdb ASDF System Details

- Version: 0.1
- Description: Bindings to LMDB, the Lightning Memory-mapped Database.
- Licence: MIT, see COPYING.
- Author: Fernando Borretti <eudoxiahp@gmail.com>, James Anderson <james.anderson@setf.de>, Gábor Melis <mega@retes.hu>
- Maintainer: Fernando Borretti <eudoxiahp@gmail.com>
- Homepage: [https://github.com/antimer/lmdb](https://github.com/antimer/lmdb)
- Bug tracker: [https://github.com/antimer/lmdb/issues](https://github.com/antimer/lmdb/issues)
- Source control: [GIT](git@github.com:antimer/lmdb.git)

<a id='x-28LMDB-3A-40LMDB-LINKS-20MGL-PAX-3ASECTION-29'></a>

## 2 Links

Here is the [official repository](https://github.com/antimer/lmdb)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/lmdb-manual.html)
for the latest version.

<a id='x-28LMDB-3A-40LMDB-INTRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 3 Introduction

[LMDB](http://www.lmdb.tech/doc/), the Lightning Memory-mapped
Database, is an [ACID](https://en.wikipedia.org/wiki/ACID) key-value
database with
[MVCC](https://en.wikipedia.org/wiki/Multiversion_concurrency_control).
It is a small C library ("C lmdb" from now on), around which [`LMDB`][1e9b]
is a Common Lisp wrapper. [`LMDB`][1e9b] covers most of C lmdb's
functionality, has a simplified API, much need [Safety][a9f7] checks, and
comprehensive documentation.

Compared to other key-value stores, [`LMDB`][1e9b]'s distuingishing features
are:

- Transactions span multiple keys.

- Concurrent use not only by multiple threads but by multiple OS
  processes, too.

- Extremely high read performance: millions of transactions per
  second.

- Very low maintenance.

Other notable things:

- With its default - the most durable - settings, it has average
  write performance, which is bottlenecked by `fsync()`.

- Readers don't block readers or writers, but there is at most one
  writer at a time.

- Extremely simple, crash-proof design.

- The entire database (called *environment*) is backed by a single
  memory-mapped file, with a
  [copy-on-write](https://en.wikipedia.org/wiki/Copy-on-write)
  [B+ tree](https://en.wikipedia.org/wiki/B%2B_tree).

- No transaction log.

- It is very much like [Berkeley
  DB](https://en.wikipedia.org/wiki/Berkeley_DB) done right, without
  the fluff and much improved administration.

Do read the [Caveats](http://www.lmdb.tech/doc/), though. On the
Lisp side, this library **will not work with virtual threads**
because [`LMDB`][1e9b]'s write locking is tied to native threads.

Using [`LMDB`][1e9b] is easy:

```
(with-temporary-env (env)
  (let ((db (get-db "test" :if-does-not-exist :create)))
    (with-txn (:write t)
      (put db "k1" #(2 3))
      (print (g3t db "k1")) ; => #(2 3)
      (del db "k1"))))
```

More typically, the environment and databases are opened once so
that multiple threads and transactions can access them:

```
(defvar *test-db*)

(unless *env*
  (setq *env* (make-env "/tmp/lmdb-test-env/")))
(unless (open-env-p *env*)
  (setq *env* (open-env  *env* :if-does-not-exist :create))
  (setq *test-db* (get-db "test" :if-does-not-exist :create
                                 :value-encoding :utf-8)))

(with-txn (:write t)
  (put *test-db* 1 "hello")
  (print (g3t *test-db* 1)) ; => "hello"
  (del *test-db* 1))
```

Note how `:VALUE-ENCODING` sneaked in above. This was so to make [`G3T`][7258]
return a string instead of an octet vector.

[`LMDB`][1e9b] treats keys and values as opaque byte arrays to be hung on a B+
tree, and only requires a comparison function to be defined over
keys. [`LMDB`][1e9b] knows how to serialize the types `(UNSIGNED-BYTE 64)` and
`STRING` (which are often used as keys so sorting must work as
expected). Serialization of the rest of the datatypes is left to the
client. See [Encoding and decoding data][3489] for more.

<a id='x-28LMDB-3A-40DESIGN-AND-IMPLEMENTATION-20MGL-PAX-3ASECTION-29'></a>

## 4 Design and implementation

<a id='x-28LMDB-3A-40SAFETY-20MGL-PAX-3ASECTION-29'></a>

### 4.1 Safety

The lmdb C API trusts client code to respect its rules. Being C,
managing object lifetimes is the biggest burden. There are also
rules that are documented, but not enforced. This Lisp wrapper tries
to enforce these rules itself and manage object lifetimes in a safe
way to avoid database corruption. How and what it does is described
in the following.

##### Environments

- [`OPEN-ENV`][b084] checks that the same path is not used in multiple open
  environments to prevent locking issues documented in
  [Caveats](http://www.lmdb.tech/doc/).

##### Transactions

- Transactions are not exposed to the client, their lifetime is tied
  to the dynamic extent of their [`WITH-TXN`][a030]. Using transactions from
  threads other than the thread of their creation is thus not
  possible. This design allows [`WITH-TXN`][a030] to work with zero consing
  and prevents difficult to diagnose races that could arise with
  garbage transactions being accessed in other threads. The
  alternative of locking every access to transaction objects and
  dealing with their finalization is much too heavyweight. Cursors,
  described below, offer a way to circumvent this protection.

- Checks are made to detect illegal operations on parent
  transactions (see [`LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR`][2a7d]).

##### Databases

- [mdb\_dbi\_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a) is wrapped by [`GET-DB`][a8aa] in a transaction and is
  protected by a mutex to comply with C lmdb's requirements:

         A transaction that opens a database must finish (either
         commit or abort) before another transaction may open it.
         Multiple concurrent transactions cannot open the same
         database.

- [mdb\_dbi\_close()](http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5)
  is too dangerous to be exposed as explained in the [`GET-DB`][a8aa]
  documentation.

- For similar reasons, [`DROP-DB`][822f] is wrapped in [`WITH-ENV`][5dfb].

- [mdb\_env\_set\_mapsize()](http://www.lmdb.tech/doc/group__mdb.html#gaa2506ec8dab3d969b0e609cd82e619e5),
  [mdb\_env\_set\_max\_readers()](http://www.lmdb.tech/doc/group__mdb.html#gae687966c24b790630be2a41573fe40e2),
  and [mdb\_env\_set\_maxdbs()](http://www.lmdb.tech/doc/group__mdb.html#gaa2fc2f1f37cb1115e733b62cab2fcdbc)
  are only available through [`OPEN-ENV`][b084] because they either require that
  there are no write transactions or do not work on open environments.

##### Cursors

- Unless `MULTITHREADED`, [`WITH-CURSOR`][7bb1] creates cursors that can be used
  only in the thread they were created in. Similarly to the
  rationale above for not exposing transactions, this is to ensure
  safe and performant access to foreign objects.

- Note that since cursors are associated with transactions,
  `MULTITHREADED` allows to access transactions from other threads if
  not with [Basic operations][b471], but with [Cursors][d610] only.

- Cursors in write transactions cannot use the `MULTITHREADED` option
  to accommodate C lmdb restrictions.

##### Signal handling

The C lmdb library handles system calls being interrupted (`EINTR`
and `EAGAIN`), but unwinding the stack from interrupts in the middle
of [`LMDB`][1e9b] calls can leave the in-memory data structures such as
transactions inconsistent. If this happens, their further use risks
database corruption. For this reason, calls to [`LMDB`][1e9b] are performed
with interrupts disabled. For SBCL, this means
`SB-SYS:WITHOUT-INTERRUPTS`. It is an error when compiling [`LMDB`][1e9b] if an
equivalent facility is not found in the Lisp implementation. A
warning is signalled if no substitute is found for
`SB-SYS:WITH-INTERRUPTS` because this makes the body of [`WITH-TXN`][a030] and
[`WITH-CURSOR`][7bb1] uninterruptible.

Operations that do not modify the database ([`G3T`][7258], [`CURSOR-FIRST`][1ca8],
[`CURSOR-VALUE`][1175] and similar) are async unwind safe, and for performance
they are called without the above provisions.

Note that the library is not reentrant, so don't call [`LMDB`][1e9b] from
signal handlers.

<a id='x-28LMDB-3A-40DEVIATIONS-FROM-THE-LMDB-API-20MGL-PAX-3ASECTION-29'></a>

### 4.2 Deviations from C lmdb API

The following are the most prominent deviations and omissions from
the C lmdb API in addition to those listed in [Safety][a9f7].

##### Environments

- [mdb\_reader\_list()](http://www.lmdb.tech/doc/group__mdb.html#ga8550000cd0501a44f57ee6dff0188744)
  is not implemented.

- [mdb\_env\_copy](http://www.lmdb.tech/doc/group__mdb.html#ga5d51d6130325f7353db0955dbedbc378)
  and its close kin are not yet implemented.

##### Transactions

- Read-only [`WITH-TXN`][a030]s are turned into noops when "nested" (unless
  `IGNORE-PARENT`).

##### Databases

- [mdb\_set\_compare()](http://www.lmdb.tech/doc/group__mdb.html#ga68e47ffcf72eceec553c72b1784ee0fe)
  and [mdb\_set\_dupsort()](http://www.lmdb.tech/doc/group__mdb.html#gacef4ec3dab0bbd9bc978b73c19c879ae)
  are not exposed. If this is needed, implement a foreign comparison
  function and call `LIBLMDB:SET-COMPARE` or `LIBLMDB:SET-DUPSORT`
  directly or perhaps change the encoding of the data.

- Working with multiple contiguous values with `DUPFIXED` is not yet
  implemented. This functionality would belong in [`PUT`][e21e], [`CURSOR-PUT`][893b],
  [`CURSOR-NEXT`][5ba5] and [`CURSOR-VALUE`][1175].

- [`PUT`][e21e], [`CURSOR-PUT`][893b] do not support the
  [RESERVE](http://www.lmdb.tech/doc/group__mdb__put.html#gac0545c6aea719991e3eae6ccc686efcc)
  flag.


<a id='x-28LMDB-3A-40VERSION-20MGL-PAX-3ASECTION-29'></a>

## 5 Library versions

<a id='x-28LMDB-3ALMDB-FOREIGN-VERSION-20FUNCTION-29'></a>

- [function] **LMDB-FOREIGN-VERSION** 

    Return the version of the C lmdb library as a string like `0.9.26`.
    
    Wraps [mdb\_version()](http://www.lmdb.tech/doc/group__mdb.html#ga0e5d7298fc39b3c187fffbe30264c968).

<a id='x-28LMDB-3ALMDB-BINDING-VERSION-20FUNCTION-29'></a>

- [function] **LMDB-BINDING-VERSION** 

    Return a string representing the version of C lmdb based on which
    the `CFFI` bindings were created. The version string has the same
    format as [`LMDB-FOREIGN-VERSION`][4607].

<a id='x-28LMDB-3A-40ENVIRONMENTS-20MGL-PAX-3ASECTION-29'></a>

## 6 Environments

An environment (class [`ENV`][783a]) is basically a single memory-mapped file
holding all the data, plus some flags determining how we interact
it. An environment can have multiple databases (class [`DB`][8400]), each of
which is a B+ tree within the same file. An environment is like a
database in a relational db, and the databases in it are like tables
and indices. The terminology comes from [Berkeley
DB](https://docs.oracle.com/cd/E17276_01/html/programmer_reference/env.html).

<a id='x-28LMDB-3A-40CREATING-ENV-20MGL-PAX-3ASECTION-29'></a>

### 6.1 Creating environments

<a id='x-28LMDB-3AMAKE-ENV-20FUNCTION-29'></a>

- [function] **MAKE-ENV** *PATH &KEY (MAX-DBS 1) (MAX-READERS 126) (MAP-SIZE (\* 1024 1024)) (MODE 436) (SUBDIR T) (SYNC T) (META-SYNC T) READ-ONLY (TLS T) (READ-AHEAD T) (LOCK T) (MEM-INIT T) FIXED-MAP WRITE-MAP MAP-ASYNC*

    Create an [`ENV`][783a] object through which the [`LMDB`][1e9b] environment can be
    accessed. Before it can be used though, it must be opened with
    [`OPEN-ENV`][b084].
    
    Unless explicitly noted, none of arguments persist (i.e. they are
    not saved in the data file).
    
    `PATH` is the filesystem location of the environment files (see `SUBDIR`
    below for more). Do not use [`LMDB`][1e9b] data files on remote filesystems,
    even between processes on the same host. This breaks `flock()` on
    some OSes, possibly memory map sync, and certainly sync between
    programs on different hosts.
    
    - `MAX-DBS`: The maximum number of named databases in the environment.
      Currently a moderate number is cheap, but a huge number gets
      expensive: 7-120 words per transaction, and every [`GET-DB`][a8aa] does a
      linear search of the opened database.
    
    - `MAP-SIZE`: Specifies the size of the data file in bytes. The new
      size takes effect immediately for the current process, but will
      not be persisted to any others until a write transaction has been
      committed by the current process. Also, only map size increases
      are persisted into the environment. If the map size is increased
      by another process, and data has grown beyond the range of the
      current mapsize, startin a new transaction (see [`WITH-TXN`][a030]) will
      signal [`LMDB-MAP-RESIZED-ERROR`][0fe6]. If zero is specified for `MAP-SIZE`,
      then the persisted size is used from the data file. Also see
      [`LMDB-MAP-FULL-ERROR`][a68b].
    
    - `MODE`: Unix file mode for files created. The default is `#o664`.
      Has no effect when opening an existing environment.
    
    The rest of the arguments correspond to [`LMDB`][1e9b] environment flags and
    are available in the plist [`ENV-FLAGS`][ee1c].
    
    - `SUBDIR`: If `SUBDIR`, then the path is a directory which holds the
      `data.mdb` and the `lock.mdb` files. If `SUBDIR` is `NIL`, the path
      is the filename of the data file and the lock file has the same
      name plus a `-lock` suffix.
    
    - `SYNC`: If `NIL`, don't `fsync` after commit. This optimization means
      a system crash can corrupt the database or lose the last
      transactions if buffers are not yet flushed to disk. The risk is
      governed by how often the system flushes dirty buffers to disk and
      how often [`SYNC-ENV`][64b5] is called. However, if the filesystem preserves
      write order (very few do) and the `WRITE-MAP` (currently
      unsupported) flag is not used, transactions exhibit
      ACI (atomicity, consistency, isolation) properties and only lose
      D (durability). I.e. database integrity is maintained, but a
      system crash may undo the final transactions.
    
    - `META-SYNC`: If `NIL`, flush system buffers to disk only once per
      transaction, but omit the metadata flush. Defer that until the
      system flushes files to disk, the next commit of a non-read-only
      transaction or [`SYNC-ENV`][64b5]. This optimization maintains database
      integrity, but a system crash may undo the last committed
      transaction. I.e. it preserves the ACI (atomicity, consistency,
      isolation) but not D (durability) database property.
    
    - `READ-ONLY`: Map the data file in read-only mode. It is an error to
      try to modify anything in it.
    
    - `TLS`: Setting it to `NIL` allows each OS thread to have multiple
      read-only transactions (see [`WITH-TXN`][a030]'s `IGNORE-PARENT` argument). It
      also allows and transactions not to be tied to a single thread,
      but that's quite dangerous, see [Safety][a9f7].
    
    - `READ-AHEAD`: Turn off readahead as in `madvise(MADV_RANDOM)`. Most
      operating systems perform read-ahead on read requests by default.
      This option turns it off if the OS supports it. Turning it off may
      help random read performance when the [`DB`][8400] is larger than RAM and
      system RAM is full. This option is not implemented on Windows.
    
    - `LOCK`: Database corruption lurks here. If `NIL`, don't do any
      locking. If concurrent access is anticipated, the caller must
      manage all concurrency itself. For proper operation the caller
      must enforce single-writer semantics, and must ensure that no
      readers are using old transactions while a writer is active. The
      simplest approach is to use an exclusive lock so that no readers
      may be active at all when a writer begins.
    
    - `MEM-INIT`: If `NIL`, don't initialize `malloc`ed memory before
      writing to unused spaces in the data file. By default, memory for
      pages written to the data file is obtained using `malloc`. While
      these pages may be reused in subsequent transactions, freshly
      `malloc`ed pages will be initialized to zeroes before use. This
      avoids persisting leftover data from other code (that used the
      heap and subsequently freed the memory) into the data file. Note
      that many other system libraries may allocate and free memory from
      the heap for arbitrary uses. E.g., stdio may use the heap for file
      I/O buffers. This initialization step has a modest performance
      cost so some applications may want to disable it using this flag.
      This option can be a problem for applications which handle
      sensitive data like passwords, and it makes memory checkers like
      Valgrind noisy. This flag is not needed with `WRITE-MAP`, which
      writes directly to the mmap instead of using malloc for pages.
    
    - `FIXED-MAP` (experimental): This flag must be specified when
      creating the environment and is stored persistently in the data
      file. If successful, the memory map will always reside at the same
      virtual address and pointers used to reference data items in the
      database will be constant across multiple invocations. This option
      may not always work, depending on how the operating system has
      allocated memory to shared libraries and other uses.
    
    Unsupported flags (an error is signalled if they are changed from
    their default values):
    
    - `WRITE-MAP`: Use a writable memory map unless `READ-ONLY` is set. This
      is faster and uses fewer mallocs, but loses protection from
      application bugs like wild pointer writes and other bad updates
      into the database. Incompatible with nested transactions. This may
      be slightly faster for [`DB`][8400]s that fit entirely in RAM, but is slower
      for [`DB`][8400]s larger than RAM. Do not mix processes with and without
      `WRITE-MAP` on the same environment. This can defeat
      durability ([`SYNC-ENV`][64b5], etc).
    
    - `MAP-ASYNC`: When using `WRITE-MAP`, use asynchronous flushes to disk.
      As with `SYNC` `NIL`, a system crash can then corrupt the database or
      lose the last transactions. Calling #sync ensures on-disk database
      integrity until next commit.
    
    Open environments have a finalizer attached to them that takes care
    of freeing foreign resources. Thus, the common idiom:
    
    ```
    (setq *env* (open-env (make-env "some-path")))
    ```
    
    is okay for development, too. No need to always do [`WITH-ENV`][5dfb],
    which does not mesh with threads anyway.
    
    Related to [mdb\_env\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga32a193c6bf4d7d5c5d579e71f22e9340).

<a id='x-28LMDB-3AENV-20CLASS-29'></a>

- [class] **ENV**

    An environment object through which a memory-mapped
    data file can be accessed. Always to be created by [`MAKE-ENV`][6382].

##### Environment reader functions

<a id='x-28LMDB-3AENV-PATH-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29'></a>

- [reader] **ENV-PATH** *ENV* *(:PATH)*

    The location of the memory-mapped and the
    environment lock files.

<a id='x-28LMDB-3AENV-MAX-DBS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29'></a>

- [reader] **ENV-MAX-DBS** *ENV* *(:MAX-DBS)*

    The maximum number of named databases in the
    environment. Currently a moderate number is cheap, but a huge
    number gets expensive: 7-120 words per transaction, and every
    [`GET-DB`][a8aa] does a linear search of the opened database.

<a id='x-28LMDB-3AENV-MAX-READERS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29'></a>

- [reader] **ENV-MAX-READERS** *ENV* *(:MAX-READERS)*

    The maximum number of threads/reader slots. See
    the documentation of the [reader lock
    table](http://lmdb.tech/doc/group__readers.html) for more.

<a id='x-28LMDB-3AENV-MAP-SIZE-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29'></a>

- [reader] **ENV-MAP-SIZE** *ENV* *(:MAP-SIZE)*

    Specifies the size of the data file in bytes.

<a id='x-28LMDB-3AENV-MODE-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29'></a>

- [reader] **ENV-MODE** *ENV* *(:MODE)*

<a id='x-28LMDB-3AENV-FLAGS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29'></a>

- [reader] **ENV-FLAGS** *ENV* *(:FLAGS)*

    A plist of the options as captured by [`MAKE-ENV`][6382].
    For example, `(:FIXED-MAP NIL :SUBDIR T ...)`.

<a id='x-28LMDB-3A-40OPENING-AND-CLOSING-ENV-20MGL-PAX-3ASECTION-29'></a>

### 6.2 Opening and closing environments

<a id='x-28LMDB-3AOPEN-ENV-20FUNCTION-29'></a>

- [function] **OPEN-ENV** *ENV &KEY (IF-DOES-NOT-EXIST :ERROR)*

    Open `ENV` for accessing the data in it. It is an error to open an
    already open environment.
    
    `IF-DOES-NOT-EXIST` determines what happens if [`ENV-PATH`][f8f6] does not
    exists:
    
    - `:ERROR:` An error is signalled.
    
    - `:CREATE:` A new memory-mapped file is created ensuring that all
      containing directories exist.
    
    - `NIL`: Return `NIL` without doing anything.
    
    To prevent corruption, an error is signalled if the same data file
    is opened multiple times. However, the checks performed do not work
    on remote filesystems (see [`ENV-PATH`][f8f6]).
    
    [`LMDB-ERROR`][7351] is signalled if open the environment fails for any other
    reason.
    
    Wraps [mdb\_env\_create()](http://www.lmdb.tech/doc/group__mdb.html#gaad6be3d8dcd4ea01f8df436f41d158d4)
    and [mdb\_env\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga32a193c6bf4d7d5c5d579e71f22e9340).

<a id='x-28LMDB-3ACLOSE-ENV-20FUNCTION-29'></a>

- [function] **CLOSE-ENV** *ENV*

    Close `ENV` and free the memory. After closing, `ENV` can be opened again.
    
    Only a single thread may call this function. All env-dependent
    objects, such as transactions and databases, must be closed before
    calling this function. Attempts to use those objects are closing the
    environment will result in a segmentation fault.
    
    Wraps [mdb\_env\_close()](http://www.lmdb.tech/doc/group__mdb.html#ga4366c43ada8874588b6a62fbda2d1e95).

<a id='x-28LMDB-3A-2AENV-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*ENV\*** *NIL*

    The default ENVIRONMENT for [`WITH-TXN`][a030].

<a id='x-28LMDB-3AWITH-ENV-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-ENV** *(ENV &KEY (IF-DOES-NOT-EXIST :ERROR)) &BODY BODY*

    Call [`OPEN-ENV`][b084] on `ENV` with `IF-DOES-NOT-EXIST`, bind [`*ENV*`][53fa] to
    `ENV`, execute `BODY`, and [`CLOSE-ENV`][97d7].

<a id='x-28LMDB-3AOPEN-ENV-P-20FUNCTION-29'></a>

- [function] **OPEN-ENV-P** *ENV*

    See if `ENV` is open, i.e. [`OPEN-ENV`][b084] has been called on it without a
    corresponding [`CLOSE-ENV`][97d7].

<a id='x-28LMDB-3A-40MISC-ENV-20MGL-PAX-3ASECTION-29'></a>

### 6.3 Miscellaneous environment functions

<a id='x-28LMDB-3ACHECK-FOR-STALE-READERS-20FUNCTION-29'></a>

- [function] **CHECK-FOR-STALE-READERS** *ENV*

    Check for stale entries in the reader lock table. See
    [Caveats](http://www.lmdb.tech/doc/). This function is called
    automatically by [`OPEN-ENV`][b084]. If OS other processes or threads
    accessing `ENV` abort without closing read transactions, call this
    function periodically to get rid off them. Alternatively, close all
    environments accessing the data file.
    
    Wraps [mdb\_reader\_check()](http://www.lmdb.tech/doc/group__mdb.html#ga366923d08bb384b3d9580a98edf5d668).

<a id='x-28LMDB-3AENV-STATISTICS-20FUNCTION-29'></a>

- [function] **ENV-STATISTICS** *ENV*

    Return statistics about `ENV` as a plist.
    
    - `:PAGE-SIZE:` The size of a database page in bytes.
    
    - `:DEPTH:` The height of the B-tree.
    
    - `:BRANCH-PAGES:` The number of internal (non-leaf) pages.
    
    - `:LEAF-PAGES:` The number of leaf pages.
    
    - `:OVERFLOW-PAGES:` The number of overflow pages.
    
    - `:ENTRIES:` The number of data items.
    
    Wraps [mdb\_env\_stat()](http://www.lmdb.tech/doc/group__mdb.html#gaf881dca452050efbd434cd16e4bae255).

<a id='x-28LMDB-3AENV-INFO-20FUNCTION-29'></a>

- [function] **ENV-INFO** *ENV*

    Return information about `ENV` as a plist.
    
    - `:MAP-ADDRESS:` Address of memory map, if fixed (see [`OPEN-ENV`][b084]'s
      `FIXED-MAP`).
    
    - `:MAP-SIZE:` Size of the memory map in bytes.
    
    - `:LAST-PAGE-NUMBER:` Id of the last used page.
    
    - `:LAST-TXN-ID:` Id of the last committed transaction.
    
    - `:MAXIMUM-READERS:` The number of reader slots.
    
    - `:N-READERS:` The number of reader slots current used.
    
    Wraps [mdb\_env\_info()](http://www.lmdb.tech/doc/group__mdb.html#ga18769362c7e7d6cf91889a028a5c5947).

<a id='x-28LMDB-3ASYNC-ENV-20FUNCTION-29'></a>

- [function] **SYNC-ENV** *ENV*

    Flush the data buffers to disk as in calling `fsync()`. When `ENV`
    had been opened with `:SYNC` `NIL` or `:META-SYNC` `NIL`, this may be handy
    to force flushing the OS buffers to disk, which avoids potential
    durability and integrity issues.
    
    Wraps [mdb\_env\_sync()](http://www.lmdb.tech/doc/group__mdb.html#ga85e61f05aa68b520cc6c3b981dba5037).

<a id='x-28LMDB-3AENV-MAX-KEY-SIZE-20FUNCTION-29'></a>

- [function] **ENV-MAX-KEY-SIZE** *ENV*

    Return the maximum size of keys and [DUPSORT][6f4e] data in bytes. Depends
    on the compile-time constant `MDB_MAXKEYSIZE` in the C library. The
    default is 511. If this limit is exceeded [`LMDB-BAD-VALSIZE-ERROR`][6f1e] is
    signalled.
    
    Wraps [mdb\_env\_get\_maxkeysize()](http://www.lmdb.tech/doc/group__mdb.html#gaaf0be004f33828bf2fb09d77eb3cef94).

<a id='x-28LMDB-3AWITH-TEMPORARY-ENV-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-TEMPORARY-ENV** *(VAR &REST ENV-ARGS) &BODY BODY*

    Run `BODY` with an open temporary environment bound to `VAR`. In more
    detail, create an environment in a fresh temporary directory in an
    OS specific location. `ENV-ARGS` is a list of keyword arguments and
    values for [`MAKE-ENV`][6382]. This is intended for testing and examples.

<a id='x-28LMDB-3A-40TRANSACTIONS-20MGL-PAX-3ASECTION-29'></a>

## 7 Transactions

The [`LMDB`][1e9b] environment supports transactional reads and writes. By
default, these provide the standard ACID (atomicity, consistency,
isolation, durability) guarantees. Writes from a transaction are not
immediately visible to transactions. When the transaction is
committed, all its writes become visible atomically for future
transactions even if Lisp crashes or there is power failure. If the
transaction is aborted, its writes are discarded.

Transactions span the entire environment (see [`ENV`][783a]). All the updates
made in the course of an update transaction - writing records across
all databases, creating databases, and destroying databases - are
either completed atomically or rolled back.

Write transactions can be nested. Child transactions see the
uncommitted writes of their parent. The child transaction can commit
or abort, at which point its writes become visible to the parent
transaction or are discarded. If parent aborts, all of the writes
performed in the context of the parent, including those from
committed child transactions, are discarded.

<a id='x-28LMDB-3AWITH-TXN-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-TXN** *(&KEY (ENV '\*ENV\*) WRITE IGNORE-PARENT (SYNC T) (META-SYNC T)) &BODY BODY*

    Start a transaction in `ENV`, execute `BODY`. Then, if the transaction
    is open (see [`OPEN-TXN-P`][3b48]) and `BODY` returned normally, attempt to
    commit the transaction. Next, if `BODY` performed a non-local exit or
    committing failed, but the transaction is still open, then abort it.
    It is explicitly allowed to call [`COMMIT-TXN`][0147] or [`ABORT-TXN`][eab0] within
    [`WITH-TXN`][a030].
    
    Transactions provide ACID guarantees (with `SYNC` and `META-SYNC` both
    on). They span the entire environment, they are not specific to
    individual [`DB`][8400].
    
    - If `WRITE` is `NIL`, the transaction is read-only and no writes (e.g.
      [`PUT`][e21e]) may be performed in the transaction. On the flipside, many
      read-only transactions can run concurrently (see [`ENV-MAX-READERS`][52b7]),
      while there can be at most one write transaction. Furthermore, the
      single write transaction can also run concurrently with read
      transactions, just keep in mind that read transactions hold on to
      the state of the environment at the time of their creation and
      thus prevent pages since replaced from being reused.
    
    - If `IGNORE-PARENT` is true, then in an enclosing [`WITH-TXN`][a030], instead
      of creating a child transaction, start an independent transaction.
    
    - If `SYNC` is `NIL`, then no flushing of buffers will take place after
      a commit as if the environment had been opened with `:SYNC` `NIL`.
    
    - Likewise, `META-SYNC` is the per-transaction equivalent of the
      [`OPEN-ENV`][b084]'s `META-SYNC`.
    
    Also see [Nesting transactions][1fa4].
    
    Wraps [mdb\_txn\_begin()](http://www.lmdb.tech/doc/group__mdb.html#gad7ea55da06b77513609efebd44b26920).

<a id='x-28LMDB-3A-40ACTIVE-TRANSACTION-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **active transaction**

    During execution, the active transaction is the transaction created
    by the immediately enclosing [`WITH-TXN`][a030]. That is, the innermost
    [`WITH-TXN`][a030] that has the currently running code within its dynamic
    extent.
    
    Operations that require a transaction always attempt to use the
    active transaction even if it is not open (see [`OPEN-TXN-P`][3b48]).

<a id='x-28LMDB-3AOPEN-TXN-P-20FUNCTION-29'></a>

- [function] **OPEN-TXN-P** 

    See if `TXN` is open, i.e. [`COMMIT-TXN`][0147] or [`ABORT-TXN`][eab0] have not been
    called on it. Also, [`RESET-TXN`][ea3b] without a corresponding [`RENEW-TXN`][ca56]
    closes the transaction.

<a id='x-28LMDB-3ATXN-ID-20FUNCTION-29'></a>

- [function] **TXN-ID** 

    The ID of `TXN`. IDs are integers incrementing from 1. For a
    read-only transaction, this corresponds to the snapshot being read;
    concurrent readers will frequently have the same transaction ID.
    Only committed write transactions increment the ID. If a transaction
    aborts, the ID may be re-used by the next writer.

<a id='x-28LMDB-3ACOMMIT-TXN-20FUNCTION-29'></a>

- [function] **COMMIT-TXN** 

    Commit `TXN` or signal an error if it is not open. If `TXN` is not
    nested in another transaction, committing makes updates performed
    visible to future transactions. If `TXN` is a child transaction, then
    committing makes updates visible to its parent only. For read-only
    transactions, committing releases the reference to a historical
    version environment, allowing reuse of pages since replaced.
    
    Wraps [mdb\_txn\_commit()](http://www.lmdb.tech/doc/group__mdb.html#ga846fbd6f46105617ac9f4d76476f6597).

<a id='x-28LMDB-3AABORT-TXN-20FUNCTION-29'></a>

- [function] **ABORT-TXN** 

    Close `TXN` by discarding all updates performed, which will then not
    be visible to either parent or future transactions. Aborting an
    already closed transaction is a noop. Always succeeds.
    
    Wraps [mdb\_txn\_abort()](http://www.lmdb.tech/doc/group__mdb.html#ga73a5938ae4c3239ee11efa07eb22b882).

<a id='x-28LMDB-3ARENEW-TXN-20FUNCTION-29'></a>

- [function] **RENEW-TXN** 

    Renew `TXN` that was reset by [`RESET-TXN`][ea3b]. This acquires a new reader
    lock that had been released by [`RESET-TXN`][ea3b]. After renewal, it is as if
    `TXN` had just been started.
    
    Wraps [mdb\_txn\_renew()](http://www.lmdb.tech/doc/group__mdb.html#ga6c6f917959517ede1c504cf7c720ce6d).

<a id='x-28LMDB-3ARESET-TXN-20FUNCTION-29'></a>

- [function] **RESET-TXN** 

    Abort the open, read-only `TXN`, release the reference to the
    historical version of the environment, but make it faster to start
    another read-only transaction with [`RENEW-TXN`][ca56]. This is accomplished
    by not deallocating some data structures, and keeping the slot in
    the reader table. Cursors opened within the transaction must not be
    used again, except if renewed (see RENEW-CURSOR). If `TXN` is an open,
    read-only transaction, this function always succeeds.
    
    Wraps [mdb\_txn\_reset()](http://www.lmdb.tech/doc/group__mdb.html#ga02b06706f8a66249769503c4e88c56cd).

<a id='x-28LMDB-3A-40NESTING-TRANSACTIONS-20MGL-PAX-3ASECTION-29'></a>

### 7.1 Nesting transactions

Transactions can be nested to arbitrary levels by dynamically
nesting [`WITH-TXN`][a030]s. Child transactions may be committed or aborted
independently from their parent transaction (the immediately
enclosing [`WITH-TXN`][a030]). Committing a child transaction only makes the
updates made by it visible to the parent. If the parent then aborts,
the child's updates are aborted too. If the parent commits, all
child transactions that were not aborted are committed, too.

Actually, the C lmdb library only supports nesting write
transactions. To simplify usage, the Lisp side turns read-only
[`WITH-TXN`][a030]s nested in another [`WITH-TXN`][a030]s into noops.

```
(with-temporary-env (env)
  (let ((db (get-db "test" :if-does-not-exist :create
                    :value-encoding :uint64)))
    ;; Create a top-level write transaction.
    (with-txn (:write t)
      (put db "p" 0)
      ;; First child transaction
      (with-txn (:write t)
        ;; Writes of the parent are visible in children.
        (assert (= (g3t db "p") 0))
        (put db "c1" 1))
      ;; Parent sees what the child committed (but it's not visible to
      ;; unrelated transactions).
      (assert (= (g3t db "c1") 1))
      ;; Second child transaction
      (with-txn (:write t)
        ;; Sees writes from the parent that came from the first child.
        (assert (= (g3t db "c1") 1))
        (put db "c1" 2)
        (put db "c2" 2)
        (abort-txn)))
    ;; Create a top-level read transaction to check what was committed.
    (with-txn ()
      ;; Since the second child aborted, its writes are discarded.
      (assert (= (g3t db "p") 0))
      (assert (= (g3t db "c1") 1))
      (assert (null (g3t db "c2"))))))
```

[`COMMIT-TXN`][0147], [`ABORT-TXN`][eab0], and [`RESET-TXN`][ea3b] all close the transaction (see
[`OPEN-TXN-P`][3b48]), which prevents database operations such as [`G3T`][7258], [`PUT`][e21e],
[`DEL`][1852] within that transaction. Furthermore, any cursors created in the
context of the transaction will no longer be valid (but see
[`CURSOR-RENEW`][e544]).

An [`LMDB`][1e9b] parent transaction and its [Cursors][d610] must not issue
operations other than [`COMMIT-TXN`][0147] and [`ABORT-TXN`][eab0] while there are
active child transactions. As the Lisp side does not expose
transaction objects directly, performing [Basic operations][b471] in the
parent transaction is not possible. See the description of
`MULTITHREADED` `NIL` case of [`WITH-CURSOR`][7bb1] for a more complete picture.

`IGNORE-PARENT` true overrides the default nesting semantics of
[`WITH-TXN`][a030] and creates a new top-level transaction, which is not a
child of the enclosing [`WITH-TXN`][a030].

- Since [`LMDB`][1e9b] is single-writer, on nesting an `IGNORE-PARENT` write
  transaction in another write transaction, [`LMDB-BAD-TXN-ERROR`][85dd] is
  signalled to avoid the deadlock.

- Nesting a read-only [`WITH-TXN`][a030] with `IGNORE-PARENT` in another
  read-only [`WITH-TXN`][a030] is [`LMDB-BAD-RSLOT-ERROR`][5349] error with the `TLS`
  option because it would create two read-only transactions in the
  same thread.

Nesting a read transaction in another transaction would be an
[`LMDB-BAD-RSLOT-ERROR`][5349] according to the C lmdb library, but a
read-only [`WITH-TXN`][a030] with `IGNORE-PARENT` nested in another [`WITH-TXN`][a030] is
turned into a noop so this edge case is papered over.

<a id='x-28LMDB-3A-40DATABASES-20MGL-PAX-3ASECTION-29'></a>

## 8 Databases

<a id='x-28LMDB-3A-40UNNAMED-DATABASE-20MGL-PAX-3ASECTION-29'></a>

### 8.1 The unnamed database

[`LMDB`][1e9b] has a default, unnamed database backed by a B+ tree. This db
can hold normal key-value pairs and named databases. The unnamed
database can be accessed by passing `NIL` as the database name to
[`GET-DB`][a8aa]. There are some restrictions on the flags of the unnamed
database, see [`LMDB-INCOMPATIBLE-ERROR`][5a39].

<a id='x-28LMDB-3A-40DUPSORT-20MGL-PAX-3ASECTION-29'></a>

### 8.2 DUPSORT

A prominent feature of [`LMDB`][1e9b] is the ability to associate multiple
sorted values with keys, which is enabled by the `DUPSORT` argument of
[`GET-DB`][a8aa]. Just as a named database is a B+ tree associated with a
key (its name) in the B+ tree of the unnamed database, so do these
sorted duplicates form a B+ tree under a key in a named or the
unnamed database. Among the [Basic operations][b471], [`PUT`][e21e] and [`DEL`][1852] are
equipped to deal with duplicate values, but [`G3T`][7258] is too limited, and
[Cursors][d610] are needed to make full use of `DUPSORT`.

When using this feature the limit on the maximum key size applies to
duplicate data, as well. See [`ENV-MAX-KEY-SIZE`][4247].

<a id='x-28LMDB-3A-40DATABASE-API-20MGL-PAX-3ASECTION-29'></a>

### 8.3 Database API

<a id='x-28LMDB-3AGET-DB-20FUNCTION-29'></a>

- [function] **GET-DB** *NAME &KEY (ENV \*ENV\*) (IF-DOES-NOT-EXIST :ERROR) KEY-ENCODING VALUE-ENCODING INTEGER-KEY REVERSE-KEY DUPSORT INTEGER-DUP REVERSE-DUP DUPFIXED*

    Open the database with `NAME` in the open environment `ENV`, and return
    a [`DB`][8400] object. If `NAME` is `NIL`, then the [The unnamed database][15a5] is opened.
    Must not be called from an open transaction. This is because [`GET-DB`][a8aa]
    starts a transaction itself to comply with C lmdb's
    requirements (see [Safety][a9f7]).
    
    If `IF-DOES-NOT-EXIST` is `:CREATE`, then a new named database is
    created. If `IF-DOES-NOT-EXIST` is `:ERROR`, then an error is signalled
    if the database does not exists.
    
    `KEY-ENCODING` and `VALUE-ENCODING` are both one of `NIL`, `:UINT64`,
    `:OCTETS` or `:UTF-8`. `KEY-ENCODING` is set to `:UINT64` when `INTEGER-KEY`
    is true. `VALUE-ENCODING` is set to `:UINT64` when `INTEGER-DUP` is true.
    Note that changing the encoding does *not* reencode already existing
    data. encoding. See [Encoding and decoding data][3489] for their full semantics.
    
    The recommended practice is to open a database in a process once, in
    an initial read-only transaction, which commits. This leaves the db
    open for use with later transactions.
    
    The following flags are for database creation, they do not have any
    effect in subsequent calls (except for the [The unnamed database][15a5]).
    
    - `INTEGER-KEY`: Keys in the database are C `unsigned` or `size_t`
      integers encoded in native byte order. Keys must all be either
      `unsigned` or `size_t`, they cannot be mixed in a single database.
    
    - `REVERSE-KEY`: Keys are strings to be compared in reverse order,
      from the end of the strings to the beginning. By default, keys are
      treated as strings and compared from beginning to end.
    
    - `DUPSORT`: Duplicate keys may be used in the database (or, from
      another perspective, keys may have multiple values, stored in
      sorted order). By default, keys must be unique and may have only a
      single value. Also, see [DUPSORT][6f4e].
    
    - `INTEGER-DUP`: This option specifies that duplicate data items are
      binary integers, similarly to `INTEGER-KEY`. Only matters if
      `DUPSORT`.
    
    - `REVERSE-DUP`: This option specifies that duplicate data items
      should be compared as strings in reverse order. Only matters if
      `DUPSORT`.
    
    - `DUPFIXED`: This flag may only be used in combination `DUPSORT`. When
      true, data items for this database must all be the same size,
      which allows further optimizations in storage and retrieval.
      Currently, the wrapper functions that could take advantage of
      this (e.g. [`PUT`][e21e], [`CURSOR-PUT`][893b], [`CURSOR-NEXT`][5ba5] and [`CURSOR-VALUE`][1175]), do not.
    
    No function to close a database (an equivalent to
    [mdb\_dbi\_close()](http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5))
    is provided due to subtle races and corruption it could cause when
    an `MDB_dbi` (unsigned integer, similar to an fd) is assigned by a
    subsequent open to another named database.
    
    Wraps [mdb\_dbi\_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a).

<a id='x-28LMDB-3ADB-20CLASS-29'></a>

- [class] **DB**

    A database in an environment (class [`ENV`][783a]). Always to
    be created by [`GET-DB`][a8aa].

<a id='x-28LMDB-3ADB-NAME-20-28MGL-PAX-3AREADER-20LMDB-3ADB-29-29'></a>

- [reader] **DB-NAME** *DB* *(:NAME)*

    The name of the database.

<a id='x-28LMDB-3ADROP-DB-20FUNCTION-29'></a>

- [function] **DROP-DB** *NAME &KEY (DELETE T) (ENV \*ENV\*)*

    Empty the database with `NAME` in `ENV`. If `DELETE`, then delete it from
    `ENV` and close it. Since closing a database is dangerous (see
    [`GET-DB`][a8aa]), to minimize the chance of races, [`DROP-DB`][822f] opens the *closed*
    environment `ENV` itself.
    
    Wraps [mdb\_drop()](http://www.lmdb.tech/doc/group__mdb.html#gab966fab3840fc54a6571dfb32b00f2db).

<a id='x-28LMDB-3ADB-STATISTICS-20FUNCTION-29'></a>

- [function] **DB-STATISTICS** *DB*

    Return statistics about the database.
    
    Wraps [mdb\_stat()](http://www.lmdb.tech/doc/group__mdb.html#gae6c1069febe94299769dbdd032fadef6).

<a id='x-28LMDB-3A-40ENCODINGS-20MGL-PAX-3ASECTION-29'></a>

## 9 Encoding and decoding data

In the C lmdb library, keys and values are opaque byte vectors
only ever inspected internally to maintain the sort order (of keys
and duplicate values if [DUPSORT][6f4e]). The client is given the freedom
and the responsibility to choose how to perform conversion to and
from byte vectors.

[`LMDB`][1e9b] exposes this full flexibility while at the same time providing
reasonable defaults for the common cases. In particular, with the
`KEY-ENCODING` and `VALUE-ENCODING` arguments of [`GET-DB`][a8aa], the
data (meaning the key or value here) encoding can be declared
explicitly. The following values are supported:

- `:UINT64:` Data to be encoded must be of type '(`UNSIGNED-BYTE` 64)\`,
  which is then encoded as an 8 byte array in *native* byte order.
  The reverse transformation takes place when returning values. This
  is the encoding used for `INTEGER-KEY` and `INTEGER-DUP` [`DB`][8400]s.

- `:OCTETS:` Note that plural. Data to be encoded (e.g. `KEY` argument
  of [`G3T`][7258]) must be a 1D byte array. If its element type
  is `(UNSIGNED-BYTE 8)`, then the data can be passed to the foreign
  code more efficiently, but declaring the element type is not
  required. For example, `VECTOR`s can be used as long as the actual
  elements are of type `(UNSIGNED-BYTE 8)`. Foreign byte arrays to
  be decoded (e.g. the value returned by [`G3T`][7258]) are simply returned as
  a Lisp array.

- `:UTF-8:` Data to be encoded must be a string, which is converted to
  a octets by `TRIVIAL-UTF-8`. Null-terminated. Foreign byte arrays
  are decoded the same way.

- `NIL`: Data is encoded using the default encoding according to its
  Lisp type: strings as `:UTF-8`, vectors as `:OCTETS`, `(UNSIGNED-BYTE
  64)` as `:UINT64`. Decoding is always performed as `:OCTETS`.

Even if the encoding is undeclared, it is recommended to use a
single type for keys (and duplicate values) to avoid unexpected
conflicts that could arise, for example, when the UTF-8 encoding of
a string and the `:UINT64` encoding of an integer coincide. The same
consideration doubly applies to named databases, which share the key
space with normal key-value pairs in the default database.

Together, `:UINT64` and `:UTF-8` cover the common cases for keys. They
trade off dynamic typing for easy sortability (using the default C
lmdb behaviour). On the other hand, non-duplicate values (i.e. no
[DUPSORT][6f4e]), for which there is no sorting requirement, may be
serialized more freely. For this purpose, using an encoding of
`:OCTETS` or `NIL` with
[cl-conspack](https://github.com/conspack/cl-conspack) is
recommended because it works with complex objects, it encodes object
types, it is fast and space-efficient, has a stable specification
and an alternative implementation in C. For example:

```
(with-temporary-env (env)
  (let ((db (get-db "test" :if-does-not-exist :create)))
    (with-txn (:write t)
      (put db "key1" (cpk:encode (list :some "stuff" 42)))
      (cpk:decode (g3t db "key1")))))
=> (:SOME "stuff" 42)
```


<a id='x-28LMDB-3A-40SPECIAL-ENCODINGS-20MGL-PAX-3ASECTION-29'></a>

### 9.1 Special encodings

<a id='x-28LMDB-3A-2AKEY-ENCODER-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*KEY-ENCODER\*** *NIL*

    A function designator or `NIL`. If non-NIL, it overrides the encoding
    method determined by `KEY-ENCODING` (see [`GET-DB`][a8aa]). It is called with a
    single argument when a key is to be converted to an octet vector.

<a id='x-28LMDB-3A-2AKEY-DECODER-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*KEY-DECODER\*** *NIL*

    A function designator or `NIL`. If non-NIL, it is called with a
    single `MDB-VAL` argument (see [`WITH-MDB-VAL-SLOTS`][563b]), that holds a
    pointer to data to be decoded and its size. This function is called
    whenever a key is to be decoded and overrides the `KEY-ENCODING`
    argument of [`GET-DB`][a8aa].
    
    For example, if we are only interested in the length of the value
    and want to avoid creating a lisp vector on the heap, we can do
    this:
    
    ```
    (with-temporary-env (env)
      (let ((db (get-db "test" :if-does-not-exist :create)))
        (with-txn (:write t)
          (put db "key1" "abc")
          (let ((*value-decoder* (lambda (mdb-val)
                                   (with-mdb-val-slots (%bytes size mdb-val)
                                     (declare (ignore %bytes))
                                     ;; Take null termination into account.
                                     (1- size)))))
            (g3t db "key1")))))
    => 3
    => T
    ```


<a id='x-28LMDB-3A-2AVALUE-ENCODER-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*VALUE-ENCODER\*** *NIL*

    Like [`*KEY-ENCODER*`][803c], but for values.

<a id='x-28LMDB-3A-2AVALUE-DECODER-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*VALUE-DECODER\*** *NIL*

    Like [`*KEY-DECODER*`][21b9], but for values.
    
    Apart from performing actual decoding, the main purpose of
    `*VALUE-DECODER*`, one can also pass the foreign data on to other
    foreign functions such as `write()` directly from the decoder
    function and returning a constant such as `T` to avoid consing.

<a id='x-28LMDB-3AWITH-MDB-VAL-SLOTS-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-MDB-VAL-SLOTS** *(%BYTES SIZE MDB-VAL) &BODY BODY*

    Bind `%BYTES` and `SIZE` locally to the corresponding slots of `MDB-VAL`.
    `MDB-VAL` is an opaque handle for a foreign `MDB_val` struct, that
    holds the pointer to a byte array and the number of bytes in the
    array. This macro is needed to access the foreign data in a function
    used as [`*KEY-DECODER*`][21b9] or [`*VALUE-DECODER*`][05cd]. `MDB-VAL` is dynamic extent,
    so don't hold on to it. Also, the pointer to which `%BYTES` is bound
    is valid only within the context of current top-level transaction.

<a id='x-28LMDB-3A-25BYTES-TO-OCTETS-20FUNCTION-29'></a>

- [function] **%BYTES-TO-OCTETS** *MDB-VAL*

    A utility function provided for writing [`*KEY-DECODER*`][21b9] and
    [`*VALUE-DECODER*`][05cd] functions. It returns a Lisp octet vector that holds
    the same bytes as `MDB-VAL`.

<a id='x-28LMDB-3A-40BASIC-OPERATIONS-20MGL-PAX-3ASECTION-29'></a>

## 10 Basic operations

<a id='x-28LMDB-3AG3T-20FUNCTION-29'></a>

- [function] **G3T** *DB KEY*

    Return the value from `DB` associated with `KEY` and `T` as the second
    value. If `KEY` is not found in `DB`, then `NIL` is returned. If `DB`
    supports [DUPSORT][6f4e], then the first value for `KEY` will be returned.
    Retrieval of other values requires the use of [Cursors][d610].
    
    This function is called [`G3T`][7258] instead of `GET` to avoid having to shadow
    `CL:GET` when importing [`LMDB`][1e9b].
    
    Wraps [mdb\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga8bf10cd91d3f3a83a34d04ce6b07992d).

<a id='x-28LMDB-3APUT-20FUNCTION-29'></a>

- [function] **PUT** *DB KEY VALUE &KEY (OVERWRITE T) (DUPDATA T) APPEND APPEND-DUP*

    Add a `KEY`, `VALUE` pair to `DB` within `TXN` (which must support writes).
    Return `VALUE`.
    
    - `OVERWRITE`: If `NIL`, signal [`LMDB-KEY-EXISTS-ERROR`][f4de] if `KEY` already
      appears in `DB`.
    
    - `DUPDATA`: If `NIL`, signal [`LMDB-KEY-EXISTS-ERROR`][f4de] if the `KEY`, `VALUE`
      pair already appears in `DB`. Has no effect if `DB` doesn't have
      `DUPSORT`.
    
    - `APPEND`: Append the `KEY`, `VALUE` pair to the end of `DB` instead of
      finding `KEY`'s location in the B+ tree by performing comparisons.
      The client effectively promises that keys are inserted in sort
      order, which allows for fast bulk loading. If the promise is
      broken, a [`LMDB-KEY-EXISTS-ERROR`][f4de] is signalled.
    
    - `APPEND-DUP`: The client promises that duplicate values are inserted
      in sort order. If the promise is broken, a [`LMDB-KEY-EXISTS-ERROR`][f4de]
      is signalled.
    
    May signal [`LMDB-MAP-FULL-ERROR`][a68b], [`LMDB-TXN-FULL-ERROR`][1233],
    [`LMDB-TXN-READ-ONLY-ERROR`][5ab9].
    
    Wraps [mdb\_put()](http://www.lmdb.tech/doc/group__mdb.html#ga4fa8573d9236d54687c61827ebf8cac0).

<a id='x-28LMDB-3ADEL-20FUNCTION-29'></a>

- [function] **DEL** *DB KEY &KEY VALUE*

    Delete `KEY` from `DB`. Returns `T` if data was deleted, `NIL` otherwise.
    If `DB` supports sorted duplicates ([DUPSORT][6f4e]), then `VALUE` is taken
    into account: if it's `NIL`, then all duplicate values for `KEY` are
    deleted, if it's not `NIL`, then only the matching value. May signal
    [`LMDB-TXN-READ-ONLY-ERROR`][5ab9].
    
    Wraps [mdb\_del()](http://www.lmdb.tech/doc/group__mdb.html#gab8182f9360ea69ac0afd4a4eaab1ddb0).

<a id='x-28LMDB-3A-40CURSORS-20MGL-PAX-3ASECTION-29'></a>

## 11 Cursors

<a id='x-28LMDB-3AWITH-CURSOR-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-CURSOR** *(VAR DB &KEY MULTITHREADED) &BODY BODY*

    Bind `VAR` to a fresh [`CURSOR`][ecad] on `DB`. Execute `BODY`, then close the
    cursor. Within the dynamic extent of `BODY`, this will be the
    [default cursor][9298]. The cursor is tied to the [active transaction][fad5].
    
    If `MULTITHREADED` is `NIL`, [`LMDB-CURSOR-THREAD-ERROR`][37f9] is signalled if
    the cursor is accessed from threads other than the one in which it
    was created.
    
    If `MULTITHREADED` is true, this safety check is disabled and code is
    trusted to never access the cursor when the dynamic extent of the
    corresponding [`WITH-CURSOR`][7bb1] is left. If the code performs its own
    synchronization to ensure that the cursor is live, then it is safe
    to use from multiple threads. Note that for read-only transactions -
    since cursors are tied to transactions - the environment must be
    opened with `:TLS` `NIL` (see [`OPEN-ENV`][b084]).
    
    `MULTITHREADED` true signals [`LMDB-CURSOR-THREAD-ERROR`][37f9] in write
    transactions because they must never be accessed accessed by more
    than one thread.
    
    Wraps [mdb\_cursor\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga9ff5d7bd42557fd5ee235dc1d62613aa)
    and [mdb\_cursor\_close()](http://www.lmdb.tech/doc/group__mdb.html#gad685f5d73c052715c7bd859cc4c05188).

<a id='x-28LMDB-3AWITH-IMPLICIT-CURSOR-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-IMPLICIT-CURSOR** *(DB) &BODY BODY*

    Like [`WITH-CURSOR`][7bb1], but the cursor object is not accessible directly,
    only through the [default cursor][9298] mechanism. The cursor is
    stack-allocated, which eliminates the consing of [`WITH-CURSOR`][7bb1]. Note
    that stack allocation of cursors in [`WITH-CURSOR`][7bb1] would risk database
    corruption if the cursor were accessed beyond its dynamic extent.
    
    Use [`WITH-IMPLICIT-CURSOR`][1604] instead of [`WITH-CURSOR`][7bb1] if a single cursor
    at a time will suffice. Conversely, use [`WITH-CURSOR`][7bb1] if a second
    cursor is needed or the `MULTITHREADED` option is necessary. That is,
    use
    
    ```
    (with-implicit-cursor (db)
      (cursor-set-key 1))
    ```
    
    but when two cursor iterate in an interleaved manner, use
    [`WITH-CURSOR`][7bb1]:
    
    ```
    (with-cursor (c1 db)
      (with-cursor (c2 db)
        (cursor-first c1)
        (cursor-last c2)
        (if (some-pred (cursor-value c1) (cursor-value c2))
            (cursor-next c1)
            (cursor-prev c2))
        ...))
    ```
    
    Wraps [mdb\_cursor\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga9ff5d7bd42557fd5ee235dc1d62613aa)
    and [mdb\_cursor\_close()](http://www.lmdb.tech/doc/group__mdb.html#gad685f5d73c052715c7bd859cc4c05188).

<a id='x-28LMDB-3ACURSOR-20CLASS-29'></a>

- [class] **CURSOR** *STRUCTURE-OBJECT*

<a id='x-28LMDB-3ACURSOR-DB-20FUNCTION-29'></a>

- [function] **CURSOR-DB** *INSTANCE*

<a id='x-28LMDB-3A-40DEFAULT-CURSOR-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **default cursor**

    All operations, described below, that take cursor arguments accept
    `NIL` instead of a [`CURSOR`][ecad] object, in which case the cursor from the
    immediately enclosing [`WITH-CURSOR`][7bb1] or [`WITH-IMPLICIT-CURSOR`][1604] is used.
    This cursor is referred to as the *default cursor*.
    
    To reduce syntactic clutter, some operations thus make cursor
    arguments `&OPTIONAL`. When this is undesirable - because there are
    keyword arguments as well - the cursor may be a required argument as
    in [`CURSOR-PUT`][893b]. Still `NIL` can be passed explicitly.

<a id='x-28LMDB-3A-40POSITIONING-CURSORS-20MGL-PAX-3ASECTION-29'></a>

### 11.1 Positioning cursors

The following functions *position* or *initialize* a cursor while
returning the value (*a* value with [DUPSORT][6f4e]) associated with a key,
or both the key and the value. Initialization is successful if there
is the cursor points to a key-value pair, which is indicated by the
last return value being `T`.

<a id='x-28LMDB-3ACURSOR-FIRST-20FUNCTION-29'></a>

- [function] **CURSOR-FIRST** *&OPTIONAL CURSOR*

    Move `CURSOR` to the first key of its database. Return the key, the
    value and `T`, or `NIL` if the database is empty. If [DUPSORT][6f4e], position
    `CURSOR` on the first value of the first key.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_FIRST](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-FIRST-DUP-20FUNCTION-29'></a>

- [function] **CURSOR-FIRST-DUP** *&OPTIONAL CURSOR*

    Move `CURSOR` to the first duplicate value of the current key. Return
    the value and `T`. Return `NIL` if `CURSOR` is not positioned.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_FIRST\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-LAST-20FUNCTION-29'></a>

- [function] **CURSOR-LAST** *&OPTIONAL CURSOR*

    Move `CURSOR` to the last key of its database. Return the key, the
    value and `T`, or `NIL` if the database is empty. If [DUPSORT][6f4e], position
    `CURSOR` on the last value of the last key.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_LAST](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-LAST-DUP-20FUNCTION-29'></a>

- [function] **CURSOR-LAST-DUP** *&OPTIONAL CURSOR*

    Move `CURSOR` to the last duplicate value of the current key. Return
    the value and `T`. Return `NIL` if `CURSOR` is not positioned.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_LAST\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-NEXT-20FUNCTION-29'></a>

- [function] **CURSOR-NEXT** *&OPTIONAL CURSOR*

    Move `CURSOR` to the next key-value pair of its database and return
    the key, the value, and `T`. Return `NIL` if there is no next item. If
    [DUPSORT][6f4e], position `CURSOR` on the next value of the current key if
    exists, else the first value of next key. If `CURSOR` is
    uninitialized, then [`CURSOR-FIRST`][1ca8] is called on it first.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_NEXT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-NEXT-NODUP-20FUNCTION-29'></a>

- [function] **CURSOR-NEXT-NODUP** *&OPTIONAL CURSOR*

    Move `CURSOR` to the first value of next key pair of its
    database (skipping over duplicate values of the current key). Return
    the key, the value and `T`. Return `NIL` if there is no next item. If
    `CURSOR` is uninitialized, then [`CURSOR-FIRST`][1ca8] is called on it first.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_NEXT\_NODUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-NEXT-DUP-20FUNCTION-29'></a>

- [function] **CURSOR-NEXT-DUP** *&OPTIONAL CURSOR*

    Move `CURSOR` to the next value of current key pair of its database.
    Return the value and `T`. Return `NIL` if there is no next value. If
    `CURSOR` is uninitialized, then [`CURSOR-FIRST`][1ca8] is called on it first.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_NEXT\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-PREV-20FUNCTION-29'></a>

- [function] **CURSOR-PREV** *&OPTIONAL CURSOR*

    Move `CURSOR` to the previous key-value pair of its database.
    Return the key, the value and `T`. Return `NIL` if there is no previous
    item. If [DUPSORT][6f4e], position `CURSOR` on the previous value of the
    current key if exists, else the last value of previous key. If
    `CURSOR` is uninitialized, then [`CURSOR-LAST`][f43c] is called on it first.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_PREV](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-PREV-NODUP-20FUNCTION-29'></a>

- [function] **CURSOR-PREV-NODUP** *&OPTIONAL CURSOR*

    Move `CURSOR` to the last value of previous key pair of its
    database (skipping over duplicate values of the current and the
    previous key). Return the key, the value, and `T`. Return `NIL` if
    there is no prev item. If `CURSOR` is uninitialized, then [`CURSOR-LAST`][f43c]
    is called on it first.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_PREV\_NODUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-PREV-DUP-20FUNCTION-29'></a>

- [function] **CURSOR-PREV-DUP** *&OPTIONAL CURSOR*

    Move `CURSOR` to the previous duplicate value of current key pair of
    its database. Return the value and `T`. Return `NIL` if there is no prev
    value. If `CURSOR` is uninitialized, then [`CURSOR-LAST`][f43c] is called on it
    first.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_PREV\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-SET-KEY-20FUNCTION-29'></a>

- [function] **CURSOR-SET-KEY** *KEY &OPTIONAL CURSOR*

    Move `CURSOR` to `KEY` of its database. Return the corresponding value
    and `T`. Return `NIL` if `KEY` was not found. If [DUPSORT][6f4e], position `CURSOR`
    on the first value of `KEY`.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_SET\_KEY](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-SET-KEY-DUP-20FUNCTION-29'></a>

- [function] **CURSOR-SET-KEY-DUP** *KEY VALUE &OPTIONAL CURSOR*

    Move `CURSOR` to the `KEY`, `VALUE` pair of its database and return `T` on
    success. Return `NIL` if the pair was not found.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_BOTH](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-SET-RANGE-20FUNCTION-29'></a>

- [function] **CURSOR-SET-RANGE** *KEY &OPTIONAL CURSOR*

    Position `CURSOR` on the first key equal to or greater than `KEY`.
    Return the found key, the value and `T`. Return `NIL` if `KEY` was not
    found. If [DUPSORT][6f4e], position `CURSOR` on the first value of the found
    key.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_SET\_RANGE](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-SET-RANGE-DUP-20FUNCTION-29'></a>

- [function] **CURSOR-SET-RANGE-DUP** *KEY VALUE &OPTIONAL CURSOR*

    Position `CURSOR` exactly at `KEY` on the first value greater than or
    equal to `VALUE`. Return the value at the position and `T` on success,
    or `NIL` if there is no such value associated with `KEY`.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_BOTH\_RANGE](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3A-40BASIC-CURSOR-OPERATIONS-20MGL-PAX-3ASECTION-29'></a>

### 11.2 Basic cursor operations

The following operations are similar to [`G3T`][7258], [`PUT`][e21e], [`DEL`][1852] (the
[Basic operations][b471]), but [`G3T`][7258] has three variants ([`CURSOR-KEY-VALUE`][1283],
[`CURSOR-KEY`][6b56], and [`CURSOR-VALUE`][1175]). All of them require the cursor to be
positioned (see [Positioning cursors][ed71]).

<a id='x-28LMDB-3ACURSOR-KEY-VALUE-20FUNCTION-29'></a>

- [function] **CURSOR-KEY-VALUE** *&OPTIONAL CURSOR*

    Return the key and value `CURSOR` is positioned at and `T`. Return `NIL`
    if `CURSOR` is uninitialized.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-KEY-20FUNCTION-29'></a>

- [function] **CURSOR-KEY** *&OPTIONAL CURSOR*

    Return the key `CURSOR` is positioned at and `T`. Return `NIL` if `CURSOR`
    is uninitialized.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-VALUE-20FUNCTION-29'></a>

- [function] **CURSOR-VALUE** *&OPTIONAL CURSOR*

    Return the value `CURSOR` is positioned at and `T`. Return `NIL` if
    `CURSOR` is uninitialized.
    
    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id='x-28LMDB-3ACURSOR-PUT-20FUNCTION-29'></a>

- [function] **CURSOR-PUT** *KEY VALUE CURSOR &KEY CURRENT (OVERWRITE T) (DUPDATA T) APPEND APPEND-DUP*

    Like [`PUT`][e21e], store key-value pairs into `CURSOR`'s database.
    `CURSOR` is positioned at the new item, or on failure usually near it.
    Return `VALUE`.
    
    - `CURRENT`: Replace the item at the current cursor position. `KEY` must
      still be provided, and must match it. If using sorted
      duplicates ([DUPSORT][6f4e]), `VALUE` must still sort into the same place.
      This is intended to be used when the new data is the same size as
      the old. Otherwise it will simply perform a delete of the old
      record followed by an insert.
    
    - `OVERWRITE`: If `NIL`, signal KEY-EXISTS-ERROR if `KEY` already appears
      in [`CURSOR-DB`][b2ae].
    
    - `DUPDATA`: If `NIL`, signal KEY-EXISTS-ERROR if the `KEY`, `VALUE` pair
      already appears in [`DB`][8400]. Has no effect if [`CURSOR-DB`][b2ae] doesn't have
      [DUPSORT][6f4e].
    
    - `APPEND`: Append the `KEY`, `VALUE` pair to the end of [`CURSOR-DB`][b2ae] instead
      of finding `KEY`'s location in the B+ tree by performing
      comparisons. The client effectively promises that keys are
      inserted in sort order, which allows for fast bulk loading. If the
      promise is broken, a KEY-EXISTS-ERROR is signalled.
    
    - `APPEND-DUP`: The client promises that duplicate values are inserted
      in sort order. If the promise is broken, a KEY-EXISTS-ERROR is
      signalled.
    
    May signal [`LMDB-MAP-FULL-ERROR`][a68b], [`LMDB-TXN-FULL-ERROR`][1233],
    [`LMDB-TXN-READ-ONLY-ERROR`][5ab9].
    
    Wraps [mdb\_cursor\_put()](http://www.lmdb.tech/doc/group__mdb.html#ga1f83ccb40011837ff37cc32be01ad91e).

<a id='x-28LMDB-3ACURSOR-DEL-20FUNCTION-29'></a>

- [function] **CURSOR-DEL** *CURSOR &KEY DELETE-DUPS*

    Delete the key-value pair `CURSOR` is positioned at. This does not
    invalidate the cursor, so operations such as [`CURSOR-NEXT`][5ba5] can still
    be used on it. Both [`CURSOR-NEXT`][5ba5] and [`CURSOR-KEY-VALUE`][1283] will return the
    same record after this operation. If `CURSOR` is not initialized,
    [`LMDB-CURSOR-UNINITIALIZED-ERROR`][8aed] is signalled. Returns no values.
    
    If `DELETE-DUPS`, delete all duplicate values that belong to the
    current key. With `DELETE-DUPS`, [`CURSOR-DB`][b2ae] must have [DUPSORT][6f4e], else
    [`LMDB-INCOMPATIBLE-ERROR`][5a39] is signalled.
    
    May signal [`LMDB-CURSOR-UNINITIALIZED-ERROR`][8aed],
    [`LMDB-TXN-READ-ONLY-ERROR`][5ab9].
    
    Wraps [mdb\_cursor\_del()](http://www.lmdb.tech/doc/group__mdb.html#ga26a52d3efcfd72e5bf6bd6960bf75f95).

<a id='x-28LMDB-3A-40MISC-CURSOR-20MGL-PAX-3ASECTION-29'></a>

### 11.3 Miscellaneous cursor operations

<a id='x-28LMDB-3ACURSOR-RENEW-20FUNCTION-29'></a>

- [function] **CURSOR-RENEW** *&OPTIONAL CURSOR*

    Associate `CURSOR` with read-only `TXN` as if it had been created with
    that transaction to begin with to avoid allocation overhead.
    [`CURSOR-DB`][b2ae] stays the same. This may be done whether the previous
    transaction is open or closed (see [`OPEN-TXN-P`][3b48]). No values are
    returned.
    
    Wraps [mdb\_cursor\_renew()](http://www.lmdb.tech/doc/group__mdb.html#gac8b57befb68793070c85ea813df481af).

<a id='x-28LMDB-3ACURSOR-COUNT-20FUNCTION-29'></a>

- [function] **CURSOR-COUNT** *&OPTIONAL CURSOR*

    Return the number of duplicate values for the current key of
    `CURSOR`. If [`CURSOR-DB`][b2ae] doesn't have [DUPSORT][6f4e], [`LMDB-INCOMPATIBLE-ERROR`][5a39]
    is signalled. If `CURSOR` is not initialized,
    [`LMDB-CURSOR-UNINITIALIZED-ERROR`][8aed] is signalled.
    
    Wraps [mdb\_cursor\_count()](http://www.lmdb.tech/doc/group__mdb.html#ga4041fd1e1862c6b7d5f10590b86ffbe2).

<a id='x-28LMDB-3ADO-CURSOR-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DO-CURSOR** *(KEY-VAR VALUE-VAR CURSOR &KEY FROM-END NODUP) &BODY BODY*

    Iterate over key-value pairs starting from the position of `CURSOR`.
    If `CURSOR` is not positioned then no key-value pairs will be seen. If
    `FROM-END`, then iterate with [`CURSOR-PREV`][6474] instead of [`CURSOR-NEXT`][5ba5]. If
    `NODUP`, then make that [`CURSOR-PREV-NODUP`][da9c] and [`CURSOR-NEXT-NODUP`][a492].
    
    If `CURSOR` is `NIL`, the [default cursor][9298] is used.
    
    If `NODUP` and not `FROM-END`, then the first duplicate of each key will
    be seen. If `NODUP` and `FROM-END`, then the last duplicate of each key
    will be seen.
    
    To iterate over all key-value pairs with keys >= 7:
    
    ```
    (with-cursor (cursor db)
      (cursor-set-key 7 cursor)
      (do-cursor (key value cursor)
        (print (cons key value))))
    ```


<a id='x-28LMDB-3ADO-CURSOR-DUP-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DO-CURSOR-DUP** *(VALUE-VAR CURSOR &KEY FROM-END) &BODY BODY*

    Iterate over duplicate values with starting from the position of
    `CURSOR`. If `CURSOR` is not positioned then no values will be seen. If
    `FROM-END`, then iterate with [`CURSOR-PREV-DUP`][944d] instead of
    [`CURSOR-NEXT-DUP`][084c].
    
    If `CURSOR` is `NIL`, the [default cursor][9298] is used.
    
    To iterate over all values that not smaller than #(3 4 5),
    associated with the key 7:
    
    ```
    (with-cursor (cursor db)
      (cursor-set-key-dup cursor 7 #(3 4 5))
      (do-cursor-dup (value cursor)
        (print value)))
    ```


<a id='x-28LMDB-3ADO-DB-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DO-DB** *(KEY-VAR VALUE-VAR DB &KEY FROM-END NODUP) &BODY BODY*

    Iterate over all keys and values in `DB`. If `NODUP`, then all but the
    first (or last if `FROM-END`) value for each key are skipped. If
    `FROM-END`, then iterate in reverse order.
    
    To iterate over all values in `DB`:
    
    ```
    (do-db (key value db)
      (print (cons key value)))
    ```


<a id='x-28LMDB-3ADO-DB-DUP-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DO-DB-DUP** *(VALUE-VAR DB KEY &KEY FROM-END) &BODY BODY*

    Iterate over all values associated with `KEY` in `DB`. If `FROM-END`,
    then iteration starts at the largest value.
    
    To iterate over all values associated with the key 7:
    
    ```
    (do-db-dup (value db 7)
      (print value))
    ```


<a id='x-28LMDB-3A-40CONDITIONS-20MGL-PAX-3ASECTION-29'></a>

## 12 Conditions

<a id='x-28LMDB-3ALMDB-SERIOUS-CONDITION-20CONDITION-29'></a>

- [condition] **LMDB-SERIOUS-CONDITION** *SERIOUS-CONDITION*

    The base class of all [`LMDB`][1e9b] conditions. Conditions
    that are `LMDB-SERIOUS-CONDITION`s, but not [`LMDB-ERROR`][7351]s are corruption
    and internal errors, which are hard to recover from.

<a id='x-28LMDB-3ALMDB-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-ERROR** *LMDB-SERIOUS-CONDITION ERROR*

    Base class for normal, recoverable [`LMDB`][1e9b] errors.

<a id='x-28LMDB-3A-40LMDB-ERROR-CODE-CONDITIONS-20MGL-PAX-3ASECTION-29'></a>

### 12.1 Conditions for C lmdb error codes

The following conditions correspond to [C lmdb error
codes](http://www.lmdb.tech/doc/group__errors.html).

<a id='x-28LMDB-3ALMDB-KEY-EXISTS-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-KEY-EXISTS-ERROR** *LMDB-ERROR*

    Key-value pair already exists. Signalled by [`PUT`][e21e]
    and [`CURSOR-PUT`][893b].

<a id='x-28LMDB-3ALMDB-NOT-FOUND-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-NOT-FOUND-ERROR** *LMDB-ERROR*

    Key-value pair does not exist. All functions ([`G3T`][7258],
    [`CURSOR-NEXT`][5ba5], ...) should return `NIL` instead of signalling this
    error. If it is signalled, that's a bug.

<a id='x-28LMDB-3ALMDB-PAGE-NOT-FOUND-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-PAGE-NOT-FOUND-ERROR** *LMDB-SERIOUS-CONDITION*

    Requested page not found - this usually indicates
    corruption.

<a id='x-28LMDB-3ALMDB-CORRUPTED-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-CORRUPTED-ERROR** *LMDB-SERIOUS-CONDITION*

    Located page was wrong type.

<a id='x-28LMDB-3ALMDB-PANIC-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-PANIC-ERROR** *LMDB-SERIOUS-CONDITION*

    Update of meta page failed or environment had fatal
    error.

<a id='x-28LMDB-3ALMDB-VERSION-MISMATCH-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-VERSION-MISMATCH-ERROR** *LMDB-ERROR*

    Environment version mismatch.

<a id='x-28LMDB-3ALMDB-INVALID-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-INVALID-ERROR** *LMDB-SERIOUS-CONDITION*

    File is not a valid [`LMDB`][1e9b] file.

<a id='x-28LMDB-3ALMDB-MAP-FULL-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-MAP-FULL-ERROR** *LMDB-ERROR*

    [`ENV-MAP-SIZE`][01d2] reached. Reopen the environment with a
    larger `:MAP-SIZE`.

<a id='x-28LMDB-3ALMDB-DBS-FULL-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-DBS-FULL-ERROR** *LMDB-ERROR*

    [`ENV-MAX-DBS`][30a6] reached. Reopen the environment with a
    higher `:MAX-DBS`.

<a id='x-28LMDB-3ALMDB-READERS-FULL-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-READERS-FULL-ERROR** *LMDB-ERROR*

    [`ENV-MAX-READERS`][52b7] reached. Reopen the environment
    with a higher `:MAX-READERS`.

<a id='x-28LMDB-3ALMDB-TXN-FULL-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-TXN-FULL-ERROR** *LMDB-ERROR*

    `TXN` has too many dirty pages. This condition is
    expected to occur only when using nested read-write transactions or
    operations multiple items (currently not supported by this
    wrapper).

<a id='x-28LMDB-3ALMDB-CURSOR-FULL-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-CURSOR-FULL-ERROR** *LMDB-SERIOUS-CONDITION*

    Cursor stack too deep - internal error.

<a id='x-28LMDB-3ALMDB-PAGE-FULL-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-PAGE-FULL-ERROR** *LMDB-SERIOUS-CONDITION*

    Page has not enough space - internal error.

<a id='x-28LMDB-3ALMDB-MAP-RESIZED-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-MAP-RESIZED-ERROR** *LMDB-ERROR*

    Data file contents grew beyond [`ENV-MAP-SIZE`][01d2]. This
    can happen if another OS process using the same environment path set
    a larger map size than this process did.

<a id='x-28LMDB-3ALMDB-INCOMPATIBLE-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-INCOMPATIBLE-ERROR** *LMDB-ERROR*

    Operation and [`DB`][8400] incompatible, or [`DB`][8400] type changed.
    This can mean:
    
    - The operation expects a [DUPSORT][6f4e] or `DUPFIXED` database.
    
    - Opening a named [`DB`][8400] when the unnamed [`DB`][8400] has `DUPSORT` or `INTEGER-KEY`.
    
    - Accessing a data record as a database, or vice versa.
    
    - The database was dropped and recreated with different flags.


<a id='x-28LMDB-3ALMDB-BAD-RSLOT-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-BAD-RSLOT-ERROR** *LMDB-ERROR*

    Invalid reuse of reader locktable slot. May be
    signalled by [`WITH-TXN`][a030].

<a id='x-28LMDB-3ALMDB-BAD-TXN-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-BAD-TXN-ERROR** *LMDB-ERROR*

    Transaction must abort, has a child, or is invalid.
    Signalled, for example, when a read-only transaction is nested in a
    read-write transaction, or when a cursor is used whose transaction
    has been closed (committed, aborted, or reset).

<a id='x-28LMDB-3ALMDB-BAD-VALSIZE-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-BAD-VALSIZE-ERROR** *LMDB-ERROR*

    Unsupported size of key/DB name/data, or wrong
    `DUPFIXED`, `INTEGER-KEY` or `INTEGER-DUP`. See [`ENV-MAX-KEY-SIZE`][4247].

<a id='x-28LMDB-3ALMDB-BAD-DBI-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-BAD-DBI-ERROR** *LMDB-ERROR*

    The specified `DBI` was changed unexpectedly.

<a id='x-28LMDB-3A-40ADDITIONAL-CONDITIONS-20MGL-PAX-3ASECTION-29'></a>

### 12.2 Additional conditions

The following conditions do not have a dedicated C lmdb error
code.

<a id='x-28LMDB-3ALMDB-CURSOR-UNINITIALIZED-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-CURSOR-UNINITIALIZED-ERROR** *LMDB-ERROR*

    Cursor was not initialized. Position the cursor at
    a key-value pair with a function like [`CURSOR-FIRST`][1ca8] or
    [`CURSOR-SET-KEY`][353b]. Signalled when some functions return the C error
    code `EINVAL`.

<a id='x-28LMDB-3ALMDB-CURSOR-THREAD-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-CURSOR-THREAD-ERROR** *LMDB-ERROR*

    Cursor was accessed from a thread other than the
    one in which it was created. Since the foreign cursor object's
    lifetime is tied to the dynamic extent of its [`WITH-CURSOR`][7bb1], this
    might mean accessing garbage in foreign memory with unpredictable
    consequences. This safety check can be turned off with [`WITH-CURSOR`][7bb1]'s
    `MULTITHREADED` option. Also signalled if a `MULTITHREADED` cursor would
    be created in a write transaction.

<a id='x-28LMDB-3ALMDB-TXN-READ-ONLY-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-TXN-READ-ONLY-ERROR** *LMDB-ERROR*

    Attempt was made to write in a read-only
    transaction. Signalled when some functions return the C error code
    `EACCESS`.

<a id='x-28LMDB-3ALMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR-20CONDITION-29'></a>

- [condition] **LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR** *LMDB-ERROR*

    A parent transaction and its cursors may not
    issue any other operations than [`COMMIT-TXN`][0147] and [`ABORT-TXN`][eab0] while it
    has active child transactions. In [`LMDB`][1e9b], [Basic operations][b471] are always
    executed in the [active transaction][fad5], but [Cursors][d610] can refer to the
    parent transaction:
    
    ```
    (with-temporary-env (env)
      (let ((db (get-db "db" :if-does-not-exist :create)))
        (with-txn (:write t)
          (put db #(1) #(1))
          (with-cursor (cursor db)
            (with-txn (:write t)
              (assert-error lmdb-illegal-access-to-parent-txn-error
                (cursor-set-key #(1) cursor)))))))
    ```
    
    Illegal access to a parent transaction is not detected for
    `MULTITHREADED` cursors (see [`WITH-CURSOR`][7bb1]) being accessed in another
    thread.

  [00ec]: #x-28LMDB-3A-40LMDB-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [0147]: #x-28LMDB-3ACOMMIT-TXN-20FUNCTION-29 "(LMDB:COMMIT-TXN FUNCTION)"
  [01d2]: #x-28LMDB-3AENV-MAP-SIZE-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29 "(LMDB:ENV-MAP-SIZE (MGL-PAX:READER LMDB:ENV))"
  [05cd]: #x-28LMDB-3A-2AVALUE-DECODER-2A-20-28VARIABLE-29-29 "(LMDB:*VALUE-DECODER* (VARIABLE))"
  [084c]: #x-28LMDB-3ACURSOR-NEXT-DUP-20FUNCTION-29 "(LMDB:CURSOR-NEXT-DUP FUNCTION)"
  [097c]: #x-28LMDB-3A-40CONDITIONS-20MGL-PAX-3ASECTION-29 "Conditions"
  [0fe6]: #x-28LMDB-3ALMDB-MAP-RESIZED-ERROR-20CONDITION-29 "(LMDB:LMDB-MAP-RESIZED-ERROR CONDITION)"
  [1175]: #x-28LMDB-3ACURSOR-VALUE-20FUNCTION-29 "(LMDB:CURSOR-VALUE FUNCTION)"
  [1233]: #x-28LMDB-3ALMDB-TXN-FULL-ERROR-20CONDITION-29 "(LMDB:LMDB-TXN-FULL-ERROR CONDITION)"
  [1283]: #x-28LMDB-3ACURSOR-KEY-VALUE-20FUNCTION-29 "(LMDB:CURSOR-KEY-VALUE FUNCTION)"
  [15a5]: #x-28LMDB-3A-40UNNAMED-DATABASE-20MGL-PAX-3ASECTION-29 "The unnamed database"
  [1604]: #x-28LMDB-3AWITH-IMPLICIT-CURSOR-20-28MGL-PAX-3AMACRO-29-29 "(LMDB:WITH-IMPLICIT-CURSOR (MGL-PAX:MACRO))"
  [175f]: #x-28LMDB-3A-40MISC-CURSOR-20MGL-PAX-3ASECTION-29 "Miscellaneous cursor operations"
  [1852]: #x-28LMDB-3ADEL-20FUNCTION-29 "(LMDB:DEL FUNCTION)"
  [1ca8]: #x-28LMDB-3ACURSOR-FIRST-20FUNCTION-29 "(LMDB:CURSOR-FIRST FUNCTION)"
  [1e9b]: #x-28-23A-28-284-29-20BASE-CHAR-20-2E-20-22lmdb-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((4) BASE-CHAR . \"lmdb\") ASDF/SYSTEM:SYSTEM)"
  [1fa4]: #x-28LMDB-3A-40NESTING-TRANSACTIONS-20MGL-PAX-3ASECTION-29 "Nesting transactions"
  [21b9]: #x-28LMDB-3A-2AKEY-DECODER-2A-20-28VARIABLE-29-29 "(LMDB:*KEY-DECODER* (VARIABLE))"
  [22cc]: #x-28LMDB-3A-40CREATING-ENV-20MGL-PAX-3ASECTION-29 "Creating environments"
  [258b]: #x-28LMDB-3A-40DATABASES-20MGL-PAX-3ASECTION-29 "Databases"
  [2a7d]: #x-28LMDB-3ALMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR-20CONDITION-29 "(LMDB:LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR CONDITION)"
  [2fff]: #x-28LMDB-3A-40DATABASE-API-20MGL-PAX-3ASECTION-29 "Database API"
  [30a6]: #x-28LMDB-3AENV-MAX-DBS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29 "(LMDB:ENV-MAX-DBS (MGL-PAX:READER LMDB:ENV))"
  [3489]: #x-28LMDB-3A-40ENCODINGS-20MGL-PAX-3ASECTION-29 "Encoding and decoding data"
  [353b]: #x-28LMDB-3ACURSOR-SET-KEY-20FUNCTION-29 "(LMDB:CURSOR-SET-KEY FUNCTION)"
  [37f9]: #x-28LMDB-3ALMDB-CURSOR-THREAD-ERROR-20CONDITION-29 "(LMDB:LMDB-CURSOR-THREAD-ERROR CONDITION)"
  [3b48]: #x-28LMDB-3AOPEN-TXN-P-20FUNCTION-29 "(LMDB:OPEN-TXN-P FUNCTION)"
  [3d52]: #x-28LMDB-3A-40BASIC-CURSOR-OPERATIONS-20MGL-PAX-3ASECTION-29 "Basic cursor operations"
  [4247]: #x-28LMDB-3AENV-MAX-KEY-SIZE-20FUNCTION-29 "(LMDB:ENV-MAX-KEY-SIZE FUNCTION)"
  [4607]: #x-28LMDB-3ALMDB-FOREIGN-VERSION-20FUNCTION-29 "(LMDB:LMDB-FOREIGN-VERSION FUNCTION)"
  [4746]: #x-28LMDB-3A-40LMDB-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [4850]: #x-28LMDB-3A-40ADDITIONAL-CONDITIONS-20MGL-PAX-3ASECTION-29 "Additional conditions"
  [52b7]: #x-28LMDB-3AENV-MAX-READERS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29 "(LMDB:ENV-MAX-READERS (MGL-PAX:READER LMDB:ENV))"
  [5349]: #x-28LMDB-3ALMDB-BAD-RSLOT-ERROR-20CONDITION-29 "(LMDB:LMDB-BAD-RSLOT-ERROR CONDITION)"
  [53fa]: #x-28LMDB-3A-2AENV-2A-20-28VARIABLE-29-29 "(LMDB:*ENV* (VARIABLE))"
  [563b]: #x-28LMDB-3AWITH-MDB-VAL-SLOTS-20-28MGL-PAX-3AMACRO-29-29 "(LMDB:WITH-MDB-VAL-SLOTS (MGL-PAX:MACRO))"
  [5a39]: #x-28LMDB-3ALMDB-INCOMPATIBLE-ERROR-20CONDITION-29 "(LMDB:LMDB-INCOMPATIBLE-ERROR CONDITION)"
  [5ab9]: #x-28LMDB-3ALMDB-TXN-READ-ONLY-ERROR-20CONDITION-29 "(LMDB:LMDB-TXN-READ-ONLY-ERROR CONDITION)"
  [5ba5]: #x-28LMDB-3ACURSOR-NEXT-20FUNCTION-29 "(LMDB:CURSOR-NEXT FUNCTION)"
  [5dfb]: #x-28LMDB-3AWITH-ENV-20-28MGL-PAX-3AMACRO-29-29 "(LMDB:WITH-ENV (MGL-PAX:MACRO))"
  [6382]: #x-28LMDB-3AMAKE-ENV-20FUNCTION-29 "(LMDB:MAKE-ENV FUNCTION)"
  [6474]: #x-28LMDB-3ACURSOR-PREV-20FUNCTION-29 "(LMDB:CURSOR-PREV FUNCTION)"
  [64b5]: #x-28LMDB-3ASYNC-ENV-20FUNCTION-29 "(LMDB:SYNC-ENV FUNCTION)"
  [67a2]: #x-28LMDB-3A-40DESIGN-AND-IMPLEMENTATION-20MGL-PAX-3ASECTION-29 "Design and implementation"
  [6b56]: #x-28LMDB-3ACURSOR-KEY-20FUNCTION-29 "(LMDB:CURSOR-KEY FUNCTION)"
  [6c6e]: #x-28LMDB-3A-40MISC-ENV-20MGL-PAX-3ASECTION-29 "Miscellaneous environment functions"
  [6f1e]: #x-28LMDB-3ALMDB-BAD-VALSIZE-ERROR-20CONDITION-29 "(LMDB:LMDB-BAD-VALSIZE-ERROR CONDITION)"
  [6f4e]: #x-28LMDB-3A-40DUPSORT-20MGL-PAX-3ASECTION-29 "DUPSORT"
  [7258]: #x-28LMDB-3AG3T-20FUNCTION-29 "(LMDB:G3T FUNCTION)"
  [7351]: #x-28LMDB-3ALMDB-ERROR-20CONDITION-29 "(LMDB:LMDB-ERROR CONDITION)"
  [7652]: #x-28LMDB-3A-40TRANSACTIONS-20MGL-PAX-3ASECTION-29 "Transactions"
  [783a]: #x-28LMDB-3AENV-20CLASS-29 "(LMDB:ENV CLASS)"
  [7bb1]: #x-28LMDB-3AWITH-CURSOR-20-28MGL-PAX-3AMACRO-29-29 "(LMDB:WITH-CURSOR (MGL-PAX:MACRO))"
  [803c]: #x-28LMDB-3A-2AKEY-ENCODER-2A-20-28VARIABLE-29-29 "(LMDB:*KEY-ENCODER* (VARIABLE))"
  [822f]: #x-28LMDB-3ADROP-DB-20FUNCTION-29 "(LMDB:DROP-DB FUNCTION)"
  [8400]: #x-28LMDB-3ADB-20CLASS-29 "(LMDB:DB CLASS)"
  [85dd]: #x-28LMDB-3ALMDB-BAD-TXN-ERROR-20CONDITION-29 "(LMDB:LMDB-BAD-TXN-ERROR CONDITION)"
  [893b]: #x-28LMDB-3ACURSOR-PUT-20FUNCTION-29 "(LMDB:CURSOR-PUT FUNCTION)"
  [8aed]: #x-28LMDB-3ALMDB-CURSOR-UNINITIALIZED-ERROR-20CONDITION-29 "(LMDB:LMDB-CURSOR-UNINITIALIZED-ERROR CONDITION)"
  [9298]: #x-28LMDB-3A-40DEFAULT-CURSOR-20MGL-PAX-3AGLOSSARY-TERM-29 "(LMDB:@DEFAULT-CURSOR MGL-PAX:GLOSSARY-TERM)"
  [944d]: #x-28LMDB-3ACURSOR-PREV-DUP-20FUNCTION-29 "(LMDB:CURSOR-PREV-DUP FUNCTION)"
  [9697]: #x-28LMDB-3A-40SPECIAL-ENCODINGS-20MGL-PAX-3ASECTION-29 "Special encodings"
  [97d7]: #x-28LMDB-3ACLOSE-ENV-20FUNCTION-29 "(LMDB:CLOSE-ENV FUNCTION)"
  [a030]: #x-28LMDB-3AWITH-TXN-20-28MGL-PAX-3AMACRO-29-29 "(LMDB:WITH-TXN (MGL-PAX:MACRO))"
  [a492]: #x-28LMDB-3ACURSOR-NEXT-NODUP-20FUNCTION-29 "(LMDB:CURSOR-NEXT-NODUP FUNCTION)"
  [a68b]: #x-28LMDB-3ALMDB-MAP-FULL-ERROR-20CONDITION-29 "(LMDB:LMDB-MAP-FULL-ERROR CONDITION)"
  [a8aa]: #x-28LMDB-3AGET-DB-20FUNCTION-29 "(LMDB:GET-DB FUNCTION)"
  [a9f7]: #x-28LMDB-3A-40SAFETY-20MGL-PAX-3ASECTION-29 "Safety"
  [ac4c]: #x-28LMDB-3A-40VERSION-20MGL-PAX-3ASECTION-29 "Library versions"
  [b084]: #x-28LMDB-3AOPEN-ENV-20FUNCTION-29 "(LMDB:OPEN-ENV FUNCTION)"
  [b2ae]: #x-28LMDB-3ACURSOR-DB-20FUNCTION-29 "(LMDB:CURSOR-DB FUNCTION)"
  [b471]: #x-28LMDB-3A-40BASIC-OPERATIONS-20MGL-PAX-3ASECTION-29 "Basic operations"
  [c260]: #x-28LMDB-3A-40LMDB-ERROR-CODE-CONDITIONS-20MGL-PAX-3ASECTION-29 "Conditions for C lmdb error codes"
  [ca56]: #x-28LMDB-3ARENEW-TXN-20FUNCTION-29 "(LMDB:RENEW-TXN FUNCTION)"
  [cb1c]: #x-28LMDB-3A-40DEVIATIONS-FROM-THE-LMDB-API-20MGL-PAX-3ASECTION-29 "Deviations from C lmdb API"
  [d610]: #x-28LMDB-3A-40CURSORS-20MGL-PAX-3ASECTION-29 "Cursors"
  [da9c]: #x-28LMDB-3ACURSOR-PREV-NODUP-20FUNCTION-29 "(LMDB:CURSOR-PREV-NODUP FUNCTION)"
  [e12f]: #x-28LMDB-3A-40OPENING-AND-CLOSING-ENV-20MGL-PAX-3ASECTION-29 "Opening and closing environments"
  [e21e]: #x-28LMDB-3APUT-20FUNCTION-29 "(LMDB:PUT FUNCTION)"
  [e4d5]: #x-28LMDB-3A-40ENVIRONMENTS-20MGL-PAX-3ASECTION-29 "Environments"
  [e544]: #x-28LMDB-3ACURSOR-RENEW-20FUNCTION-29 "(LMDB:CURSOR-RENEW FUNCTION)"
  [ea3b]: #x-28LMDB-3ARESET-TXN-20FUNCTION-29 "(LMDB:RESET-TXN FUNCTION)"
  [eab0]: #x-28LMDB-3AABORT-TXN-20FUNCTION-29 "(LMDB:ABORT-TXN FUNCTION)"
  [ecad]: #x-28LMDB-3ACURSOR-20CLASS-29 "(LMDB:CURSOR CLASS)"
  [ed71]: #x-28LMDB-3A-40POSITIONING-CURSORS-20MGL-PAX-3ASECTION-29 "Positioning cursors"
  [ee1c]: #x-28LMDB-3AENV-FLAGS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29 "(LMDB:ENV-FLAGS (MGL-PAX:READER LMDB:ENV))"
  [f43c]: #x-28LMDB-3ACURSOR-LAST-20FUNCTION-29 "(LMDB:CURSOR-LAST FUNCTION)"
  [f4de]: #x-28LMDB-3ALMDB-KEY-EXISTS-ERROR-20CONDITION-29 "(LMDB:LMDB-KEY-EXISTS-ERROR CONDITION)"
  [f8f6]: #x-28LMDB-3AENV-PATH-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29 "(LMDB:ENV-PATH (MGL-PAX:READER LMDB:ENV))"
  [fad5]: #x-28LMDB-3A-40ACTIVE-TRANSACTION-20MGL-PAX-3AGLOSSARY-TERM-29 "(LMDB:@ACTIVE-TRANSACTION MGL-PAX:GLOSSARY-TERM)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
