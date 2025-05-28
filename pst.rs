use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{self, BufRead, Read};
use std::os::unix::fs::MetadataExt;
use std::sync::OnceLock;

#[derive(Debug, PartialEq, PartialOrd)]
struct Process {
    cmd: String,
    pid: i32,
    ppid: i32,
    uid: u32,
    args: Vec<String>,
}

fn escape(cmd: &str) -> String {
    let mut buf = String::with_capacity(cmd.len() + 8);
    for c in cmd.chars() {
        match c {
            '\\' => buf.push_str("\\\\"),
            c if c > ' ' && c <= '~' => buf.push(c),
            c => buf.push_str(&format!("\\{:03o}", c as u32)),
        }
    }
    buf
}

fn split_args(content: &str) -> Vec<String> {
    content.strip_suffix('\0').unwrap_or(content)
        .split('\0')
        .map(String::from)
        .collect()
}

fn build_proc(pid: i32) -> io::Result<Process> {
    let proc_dir = format!("/proc/{}", pid);
    let metadata = fs::metadata(&proc_dir)?;
    let uid = metadata.uid();

    let cmdline_path = format!("{}/cmdline", proc_dir);
    let mut cmdline = String::new();
    File::open(cmdline_path)?.read_to_string(&mut cmdline)?;
    let args = split_args(&cmdline);

    let stat_path = format!("{}/stat", proc_dir);
    let mut stat = String::new();
    File::open(stat_path)?.read_to_string(&mut stat)?;

    let cmd_start = stat.find('(').unwrap_or(0) + 1;
    let cmd_end = stat.rfind(')').unwrap_or(stat.len());
    let cmd = stat[cmd_start..cmd_end].to_string();

    let stat_parts: Vec<&str> = stat[cmd_end + 4..].split_whitespace().collect();
    let ppid = stat_parts.get(0).and_then(|s| s.parse().ok()).unwrap_or(0);

    Ok(Process { cmd, args, pid, ppid, uid })
}

fn get_pids() -> io::Result<Vec<i32>> {
    let entries = fs::read_dir("/proc")?;
    let mut pids = Vec::new();
    for entry in entries {
        if let Ok(entry) = entry {
            if let Ok(pid) = entry.file_name().to_string_lossy().parse::<i32>() {
                pids.push(pid);
            }
        }
    }
    Ok(pids)
}

fn read_procs() -> io::Result<Vec<Process>> {
    get_pids()?.into_iter().map(build_proc).collect()
}

static USERNAME_MAP: OnceLock<HashMap<u32, String>> = OnceLock::new();

fn get_username(uid: u32) -> Option<String> {
    let username_map = USERNAME_MAP.get_or_init(|| {
        let mut map = HashMap::new();
        if let Ok(passwd_file) = File::open("/etc/passwd") {
            let reader = io::BufReader::new(passwd_file);
            for line in reader.lines().flatten() {
                let parts: Vec<&str> = line.split(':').collect();
                if parts.len() > 2 {
                    if let Ok(id) = parts[2].parse::<u32>() {
                        map.insert(id, parts[0].to_string());
                    }
                }
            }
        }
        map
    });
    username_map.get(&uid).cloned()
}

fn show_children(
    procs: &[Process],
    depth: i32,
    more_at_depth: HashSet<i32>,
    parent: i32,
    last_uid: u32
) -> io::Result<()> {
    let mut children: Vec<&Process> =
        procs.iter().filter(|p| p.ppid == parent).collect();
    if children.is_empty() {
        return Ok(());
    }
    let depth = depth + 1;
    children.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let last_child = children.pop().unwrap();
    for child in children {
        let mut more = more_at_depth.clone();
        more.insert(depth);
        show_proc(procs, depth, false, more, last_uid, &child)?;
    }
    let mut more = more_at_depth.clone();
    more.remove(&depth);
    show_proc(procs, depth, true, more, last_uid, &last_child)
}

fn show_proc(
    procs: &[Process],
    depth: i32,
    last: bool,
    more_at_depth: HashSet<i32>,
    last_uid: u32,
    proc: &Process
) -> io::Result<()> {
    let mut prefix = String::new();
    for i in 1..=depth {
        let more = more_at_depth.contains(&i);
        prefix.push_str("  ");
        if i == depth {
            prefix.push_str(if last { "└─" } else { "├─" });
        } else {
            prefix.push_str(if more { "│ " } else { "  " });
        }
    }
    let username = get_username(proc.uid).unwrap_or_else(|| proc.uid.to_string());
    let args = proc.args[1..].iter().map(|arg| escape(arg)).collect::<Vec<String>>().join(" ");
    println!(
        "{}{},{}{}{}",
        prefix,
        proc.cmd,
        proc.pid,
        if proc.uid == last_uid { "".to_string() } else { format!(",{}", username) },
        if args.is_empty() { "".to_string() } else { format!(" {}", args) },
    );
    show_children(procs, depth, more_at_depth, proc.pid, proc.uid)
}

fn show_tree(start: i32, procs: Vec<Process>) -> io::Result<()> {
    let root = procs.iter().find(|p| p.pid == start).unwrap();
    show_proc(&procs, 0, true, HashSet::new(), 0, root)
}

fn main() -> io::Result<()> {
    show_tree(1, read_procs()?)
}
