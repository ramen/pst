# pst - A pstree-like script in several languages

This was an experiment I did to learn how to read Linux's /proc filesystem to
generate a text-mode visualization of the current process tree. The output was
intended to match as closely as possible the output of the Linux command
pstree when run as "pstree -paulA", and I studied the pstree source to
accomplish this.

There are three versions: pst.py, pst.rb, and pst.ml, written in Python, Ruby,
and OCaml, respectively. As they use the /proc filesystem, they are most
likely only functional on Linux.
