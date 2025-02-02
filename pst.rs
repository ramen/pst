use std::collections::HashSet;
use std::fs;
use std::io::{self, Read};
use std::os::unix::fs::MetadataExt;

#[derive(PartialEq, PartialOrd)]
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
        if c == '\\' {
            buf.push_str("\\\\");
        } else if c > ' ' && c <= '~' {
            buf.push(c);
        } else {
            buf.push_str(&format!("\\{:03o}", c as u32));
        }
    }
    buf
}

fn split_args(content: &str) -> Vec<String> {
    content.split('\0').map(String::from).collect()
}

fn build_proc(pid: i32) -> io::Result<Process> {
    let proc_dir = format!("/proc/{}", pid);
    let metadata = fs::metadata(&proc_dir)?;
    let uid = metadata.uid();

    let cmdline_path = format!("{}/cmdline", proc_dir);
    let mut cmdline = String::new();
    fs::File::open(cmdline_path)?.read_to_string(&mut cmdline)?;
    let args = split_args(&cmdline);

    let stat_path = format!("{}/stat", proc_dir);
    let mut stat = String::new();
    fs::File::open(stat_path)?.read_to_string(&mut stat)?;

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
    let pids = get_pids()?;
    let mut procs = Vec::new();
    for pid in pids {
        if let Ok(proc) = build_proc(pid) {
            procs.push(proc);
        }
    }
    Ok(procs)
}

// fn get_username(uid: u32) -> Option<String> {
//     use std::ffi::CStr;
//     use std::os::raw::c_char;
//     use std::ptr;
//
//     unsafe {
//         let passwd = libc::getpwuid(uid);
//         if passwd.is_null() {
//             None
//         } else {
//             let name = CStr::from_ptr((*passwd).pw_name as *const c_char);
//             Some(name.to_string_lossy().into_owned())
//         }
//     }
// }

fn show_children(
    procs: &Vec<Process>,
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
    procs: &Vec<Process>,
    depth: i32,
    last: bool,
    more_at_depth: HashSet<i32>,
    last_uid: u32,
    proc: &Process
) -> io::Result<()> {
    let mut prefix = String::new();
    if depth > 0 {
        for i in 1..=depth {
            let more = more_at_depth.contains(&i);
            prefix.push_str("  ");
            if i == depth {
                prefix.push_str(if last { "└─" } else { "├─" });
            } else {
                prefix.push_str(if more { "│ " } else { "  " });
            }
        }
    }
    println!(
        "{}{},{}{} {}",
        prefix,
        proc.cmd,
        proc.pid,
        if proc.uid == last_uid {
            "".to_string()
        } else {
            format!(",{}", proc.uid)
        },
        if proc.args.is_empty() {
            "".to_string()
        } else {
            proc.args[1..].iter().map(|arg| escape(arg)).collect::<Vec<String>>().join(" ")
        }
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
