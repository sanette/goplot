# goplot
Mathematical plotter for the desktop

![goplot GUI](goplot.png)

gOplot is powered by [oplot](https://github.com/sanette/oplot)

## Install

### Dependencies

gOplot needs the external libraries SDL2 and GTK2 (with GL
extension). It also uses the GSL (Gnu Scientific Library). They are
all very common and stable, and should be easy to install.

In principle using `opam install goplot` will take care of these
required external dependencies.

To enable exporting to EPS or PDF you need `fig2dev`:
```
sudo apt install fig2dev
```

Finally for an optimal use, you should also consider installing LaTeX
and `fig2ps`:
```
sudo apt install texlive fig2ps
```

### Binaries

Linux binaries are in the `repo` directory. Debian-like systems can
use the latest `.deb` file. Install it with `sudo dpkg -i <filename>`
and then run `goplot`.

For others, just create a new dir (or use `/usr/local`), extract the
archive there, and run `bin/goplot`.

You may specify a specific scale for graphics (if the DPI is not
correcty detected) with the '-s' option. For instance

```
goplot -s 1.6
```

### Source

If you have the usual `ocaml` development stack (`dune`), you can
easily build `goplot` by cloning this repository and
```
dune exec bin/goplot.exe
```

### Examples

Two examples are in gOplot's "Gallery" menu.

Other examples are in the share/examples directory. Use gOplot's
"File/Load" menu to load them. Or directly from the command line, example:

```
goplot gamma.gpl
```
