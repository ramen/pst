#!/usr/bin/ruby -w

require 'etc'
require 'stringio'

def escape(cmd)
  buf = StringIO.new
  cmd.each_char do |c|
    if c == '\\'
      buf << '\\\\'
    elsif c > ' ' and c <= '~'
      buf << c
    else
      buf << '\\%03o' % c.ord
    end
  end
  buf.string
end

def arg_generator(str)
  buf = StringIO.new
  str.each_char do |c|
    if c == "\0"
      s = buf.string.clone
      buf.seek 0
      buf.truncate 0
      yield s
    else
      buf << c
    end
  end
end

def build_proc(pid)
  proc_dir = '/proc/%d' % pid
  stat = File.stat(proc_dir)
  uid = stat.uid
  args = []
  arg_generator(open('%s/cmdline' % proc_dir).read()) { |arg| args << arg }
  proc_stat = open('%s/stat' % proc_dir).read()
  cmd = proc_stat[proc_stat.index('(') + 1 ... proc_stat.rindex(')')]
  ppid = proc_stat[proc_stat.index(')') + 4 .. -1].split()[0].to_i
  return {cmd: cmd, args: args, pid: pid, ppid: ppid, uid: uid}
end

def get_pids(proc_dir='/proc')
  pids = []
  Dir.foreach(proc_dir) do |name|
    pid = name.to_i
    pids << pid if pid != 0
  end
  pids
end

def read_procs(proc_dir='/proc')
  get_pids(proc_dir).collect { |pid| build_proc(pid) }
end

class UsernameCache
  @cache = {}
  def self.get_username(uid)
    begin
      @cache.fetch uid
    rescue KeyError
      name = Etc.getpwuid(uid).name
      @cache[uid] = name
      name
    end
  end
end

def show_proc(procs, depth, last, more_at_depth, last_uid, proc)
  if depth != 0
    (1 .. depth).each do |i|
      more = more_at_depth[i]
      print('  ')
      if i == depth
        print(last ? '`-' : '|-')
      else
        print(more ? '| ' : '  ')
      end
    end
  end
  print("%s,%d%s%s\n" %
        [proc[:cmd], proc[:pid],
         (proc[:uid] != last_uid ?
          ',' + UsernameCache.get_username(proc[:uid]) :
          ''),
         (proc[:args].length > 1 ?
          ' ' + proc[:args][1..-1].collect { |arg| escape arg }.join(' ') :
          '')])
  show_children(procs, depth, more_at_depth, proc[:pid], proc[:uid])
end

def show_children(procs, depth, more_at_depth, parent, last_uid)
  children = procs.select { |proc| proc[:ppid] == parent }
  depth += 1
  items = children.sort_by { |proc| [proc[:cmd], proc[:pid]] }
  if !items.empty?
    last = items[-1]
    rest = items[0...-1]
    rest.each do |item|
      more = more_at_depth.clone
      more[depth] = true
      show_proc(procs, depth, false, more, last_uid, item)
    end
    more = more_at_depth.clone
    more[depth] = false
    show_proc(procs, depth, true, more, last_uid, last)
  end
end

def show_tree(start, procs)
  root = procs.find { |proc| proc[:pid] == start }
  show_proc(procs, 0, true, {}, 0, root)
end

show_tree(1, read_procs())
