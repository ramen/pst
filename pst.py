#!/usr/bin/env python

import os
import pwd
import sys

from io import StringIO

def escape(cmd):
    buf = StringIO()
    for c in cmd:
        if c == '\\':
            buf.write('\\\\')
        elif c > ' ' and c <= '~':
            buf.write(c)
        else:
            buf.write('\\%03o' % ord(c))
    return buf.getvalue()

def arg_generator(seq):
    buf = StringIO()
    for c in seq:
        if c == '\0':
            s = buf.getvalue()
            buf.seek(0)
            buf.truncate()
            yield s
        else:
            buf.write(c)

def build_proc(pid):
    proc_dir = '/proc/%d' % pid
    stat = os.stat(proc_dir)
    uid = stat.st_uid
    args = list(arg_generator(open('%s/cmdline' % proc_dir).read()))
    proc_stat = open('%s/stat' % proc_dir).read()
    cmd = proc_stat[proc_stat.index('(') + 1 : proc_stat.rindex(')')]
    ppid = int(proc_stat[proc_stat.index(')') + 4 : ].split()[0])
    return {'cmd': cmd, 'args': args, 'pid': pid, 'ppid': ppid, 'uid': uid}

def get_pids(proc_dir='/proc'):
    pids = []
    for name in os.listdir(proc_dir):
        try:
            pids.append(int(name))
        except ValueError:
            pass
    return pids

def read_procs(proc_dir='/proc'):
    return [build_proc(pid) for pid in get_pids(proc_dir)]

def get_username(uid, _cache={}):
    try:
        return _cache[uid]
    except KeyError:
        name = pwd.getpwuid(uid).pw_name
        _cache[uid] = name
        return name

def show_tree(start, procs):
    def show_children(depth, more_at_depth, parent, last_uid):
        children = (proc for proc in procs if proc['ppid'] == parent)
        depth += 1
        items = sorted(children, key=lambda proc: (proc['cmd'], proc['pid']))
        if items:
            last = items[-1]
            rest = items[:-1]
            for item in rest:
                more = more_at_depth.copy()
                more[depth] = True
                show_proc(depth, False, more, last_uid, item)
            more = more_at_depth.copy()
            more[depth] = False
            show_proc(depth, True, more, last_uid, last)
    def show_proc(depth, last, more_at_depth, last_uid, proc):
        if depth != 0:
            for i in range(1, depth + 1):
                more = more_at_depth[i]
                sys.stdout.write('  ')
                if i == depth:
                    sys.stdout.write('`-' if last else '|-')
                else:
                    sys.stdout.write('| ' if more else '  ')
        sys.stdout.write('%s,%d%s%s\n'
                         % (proc['cmd'], proc['pid'],
                            (',' + get_username(proc['uid'])
                             if proc['uid'] != last_uid
                             else ''),
                            (' ' + ' '.join(map(escape, proc['args'][1:]))
                             if len(proc['args']) > 1
                             else '')))
        show_children(depth, more_at_depth, proc['pid'], proc['uid'])
    root = [proc for proc in procs if proc['pid'] == start][0]
    show_proc(0, True, {}, 0, root)

if __name__ == '__main__':
    show_tree(1, read_procs())
