ACC Race Control
================

Windows
-------

Installs mingw dependencies 

```bash
export PATH=/usr/x86_64-w64-mingw32/sys-root/mingw/bin:$PATH
opam install depext-cygwinports depext
opam depext -i sqlite3
```

binutils and pkg-config paths are likely not set correctly 

```bash
export PATH=/usr/x86_64-w64-mingw32/bin/:$PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
```

Compile zlib and sqlite3

Windows Dune
------------

```
(flags (:standard -ccopt "-I/usr/local/include" -ccopt "-L/usr/local/lib" ))
```

Still compiles with a dynlink to `/usr/local/bin/cygsqlite3-0.dll`