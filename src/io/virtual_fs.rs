use std::{io, thread};
use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, Read, Seek, SeekFrom, Write},
};

pub trait FileSystem {
    /// Opens a file and returns it's file descriptor
    /// If the file does not exist (and is being read), this returns -1
    fn open(&mut self, name: String, mode: FileMode) -> i32;

    /// Closes a file.
    fn close(&mut self, fd: i32);

    /// Write to a file
    fn write(&mut self, fd: i32, bytes: &[u8]);

    /// Read bytes from a file, up to the length given
    /// If the end of the file is reached, returns an empty vector
    fn read_buffered(&mut self, fd: i32, length: usize) -> Vec<u8>;

    /// Read line from a file.
    /// If the end of the file is reached, returns an empty vector
    fn read_line(&mut self, fd: i32) -> Vec<u8>;

    /// Changes the position of the cursor in a file to a new position
    /// Mostly used for do and undo
    fn seek(&mut self, fd: i32, position: u64);

    /// Returns the position of the cursor in a file.
    /// Mostly used for do and undo
    fn get_pos(&mut self, fd: i32) -> u64;

    /// Returns the name of a file in the file descriptor
    fn get_path(&mut self, fd: i32) -> String;

    /// Prints a string to stdout
    fn stdout_str(&mut self, output: &str);

    /// Prints a sequence of bytes to stdout
    fn stdout_bytes(&mut self, output: &[u8]);

    /// Reads a line from stdin, pauses if no line is available
    fn stdin_line(&mut self) -> Vec<u8>;

    /// Reads buffered bytes from stdin, up to the length specified
    /// And stops on a newline
    /// Equivalent to fgets in C
    fn stdin_bytes_buffered(&mut self, length: usize) -> Vec<u8>;
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum FileMode {
    Read,
    Write,
    Append,
}

pub struct DefaultFileSystem {
    file_descriptors: BTreeMap<i32, (String, FileMode, File)>,
}

impl FileSystem for DefaultFileSystem {
    fn open(&mut self, path: String, mode: FileMode) -> i32 {
        let assigned_fd = self
            .file_descriptors
            .last_entry()
            .map(|v| *v.key())
            .unwrap_or(2)
            + 1;
        let file = match mode {
            FileMode::Append => std::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(&path),
            FileMode::Read => std::fs::File::open(&path),
            FileMode::Write => std::fs::File::create(&path),
        };
        match file {
            Ok(file) => {
                self.file_descriptors
                    .insert(assigned_fd, (path, mode, file));
                assigned_fd
            }
            Err(_) => -1,
        }
    }

    fn close(&mut self, fd: i32) {
        let _ = self.file_descriptors.remove(&fd);
    }

    fn write(&mut self, fd: i32, bytes: &[u8]) {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, mode, file)) => {
                if *mode == FileMode::Read {
                    return;
                }
                let _ = file.write(bytes);
            }
            None => {
                if fd == 1 {
                    let _ = std::io::stdout().write(bytes);
                }
                if fd == 2 {
                    let _ = std::io::stderr().write(bytes);
                }

            }
        }
    }
    fn read_buffered(&mut self, fd: i32, length: usize) -> Vec<u8> {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, mode, file)) => {
                if *mode != FileMode::Read {
                    return vec![];
                }
                let mut output = vec![0; length];
                match file.read(&mut output) {
                    Ok(num_read) => {
                        output.resize(num_read, 0);
                        output
                    }
                    Err(_) => {
                        vec![]
                    }
                }
            }
            None => {
                if fd == 0 {
                    return self.stdin_bytes_buffered(length);
                }
                vec![]
            }
        }
    }

    fn read_line(&mut self, fd: i32) -> Vec<u8> {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, mode, file)) => {
                if *mode != FileMode::Read {
                    return vec![];
                }
                let mut line: Vec<u8> = vec![];
                let num_read = std::io::BufReader::new(&mut *file).read_until(0xA, &mut line);
                match num_read {
                    Ok(num_read) => {
                        let cur_position = file.stream_position().unwrap_or(0);
                        let _ = file.seek(SeekFrom::Start(cur_position + line.len() as u64));
                        line
                    }
                    Err(_) => {
                        vec![]
                    }
                }
            }
            None => {
                if fd == 0 {
                    return self.stdin_line().into();
                }
                vec![]
            }
        }
    }

    fn seek(&mut self, fd: i32, position: u64) {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, file)) => {
                let _ = file.seek(SeekFrom::Start(position));
            }
            None => {}
        }
    }

    fn get_pos(&mut self, fd: i32) -> u64 {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, file)) => file.stream_position().unwrap_or(0),
            None => 0,
        }
    }
    fn get_path(&mut self, fd: i32) -> String {
        match self.file_descriptors.get_mut(&fd) {
            Some((path, _mode, _)) => path.clone(),
            None => "".to_string(),
        }
    }
    fn stdin_line(&mut self) -> Vec<u8> {
        let mut buf = String::new();
        let _ = std::io::stdin().read_line(&mut buf);
        buf.into()
    }
    fn stdin_bytes_buffered(&mut self, length: usize) -> Vec<u8> {
        let mut buf = vec![0; length];
        let mut buf_1 = [0u8];
        while std::io::stdin().read(&mut buf_1).is_ok() {
            buf.push(buf_1[0]);
            if buf_1[0] == b'\n' {
                break;
            }
            if buf.len() == length {
                break;
            }
        }
        buf
    }
    fn stdout_str(&mut self, output: &str) {
        println!("{}",output);
    }
    fn stdout_bytes(&mut self, output: &[u8]) {
        let _ = std::io::stdout().write(output);
    }
}

pub struct VirtualFileSystem {
    pub files: BTreeMap<String, Vec<u8>>,
    pub file_descriptors: BTreeMap<i32, (String, FileMode, u64)>,
    pub cwd: Vec<String>,
    pub stdin_cursor: usize,
    pub stdin: Vec<u8>,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>

}

impl Default for VirtualFileSystem {
    fn default() -> Self {
        Self {
            cwd: vec![],
            files: BTreeMap::new(),
            file_descriptors: BTreeMap::new(),
            stdin_cursor: 0,
            stdin: vec![],
            stdout: vec![],
            stderr: vec![],
        }
    }
}

impl VirtualFileSystem {
    fn make_absolute_path(&self, mut path: &str) -> Option<String> {
        let mut out = self.cwd.clone();
        while path.starts_with("../") {
            out.pop()?;
            path = &path[3..];
        }
        path.split('/').for_each(|f| out.push(f.to_string()));
        Some(out.into_iter().collect::<String>())
    }
}

impl FileSystem for VirtualFileSystem {
    fn open(&mut self, path: String, mode: FileMode) -> i32 {
        let assigned_fd = self
            .file_descriptors
            .last_entry()
            .map(|v| *v.key())
            .unwrap_or(2)
            + 1;
        let Some(absolute_path) = self.make_absolute_path(path.as_str()) else {
            return -1;
        };
        let Some(file) = self.files.get(&path) else {
            return -1;
        };
        let cursor = match mode {
            FileMode::Append => file.len() as u64,
            _ => 0,
        };
        self.file_descriptors
            .insert(assigned_fd, (absolute_path, mode, cursor));
        assigned_fd
    }

    fn close(&mut self, fd: i32) {
        let _ = self.file_descriptors.remove(&fd);
    }

    fn write(&mut self, fd: i32, bytes: &[u8]) {
        match self.file_descriptors.get_mut(&fd) {
            Some((abs_path, mode, cursor)) => {
                if *mode == FileMode::Read {
                    return;
                }
                let file = self.files.get_mut(abs_path).unwrap();
                let end = *cursor as usize + bytes.len();
                let new_file = file
                    .splice((*cursor as usize)..end, bytes.iter().cloned())
                    .collect::<Vec<u8>>();
                *file = new_file;
            }
            None => {
                if fd == 1 {
                    self.stdout.extend_from_slice(bytes);
                }
                if fd == 2 {
                    self.stdout.extend_from_slice(bytes);
                }
            }
        }
    }

    fn read_buffered(&mut self, fd: i32, length: usize) -> Vec<u8> {
        match self.file_descriptors.get_mut(&fd) {
            Some((abs_path, mode, cursor)) => {
                if *mode != FileMode::Read {
                    return vec![];
                }
                let file = self.files.get_mut(abs_path).unwrap();
                let mut output = vec![];
                loop {
                    if *cursor as usize >= file.len() {
                        break;
                    }
                    output.push(file[*cursor as usize]);
                    *cursor += 1;
                    if output.len() == length {
                        break;
                    }
                }
                output
            }
            None => {
                if fd == 0 {
                    return self.stdin_bytes_buffered(length);
                }
                vec![]
            }
        }
    }

    fn read_line(&mut self, fd: i32) -> Vec<u8> {
        match self.file_descriptors.get_mut(&fd) {
            Some((abs_path, mode, cursor)) => {
                if *mode != FileMode::Read {
                    return vec![];
                }
                let file = self.files.get_mut(abs_path).unwrap();
                let mut output = vec![];
                loop {
                    if *cursor as usize >= file.len() {
                        break;
                    }
                    output.push(file[*cursor as usize]);
                    *cursor += 1;
                    if *output.last().unwrap() == b'\n' {
                        break;
                    }
                }
                output
            }
            None => {
                if fd == 0 {
                    return self.stdin_line().into();
                }
                vec![]
            }
        }
    }

    fn seek(&mut self, fd: i32, position: u64) {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, cursor)) => {
                *cursor = position;
            }
            None => {}
        }
    }

    fn get_pos(&mut self, fd: i32) -> u64 {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, cursor)) => *cursor,
            None => 0,
        }
    }
    fn get_path(&mut self, fd: i32) -> String {
        match self.file_descriptors.get_mut(&fd) {
            Some((path, _, _)) => path.clone(),
            None => "".to_string(),
        }
    }
    fn stdin_bytes_buffered(&mut self, length: usize) -> Vec<u8> {
        let mut output_vec = vec![];
        loop {
            if output_vec.len() == length {
                break;
            }
            while self.stdin.len() == self.stdin_cursor {
                std::thread::park();
            }
            output_vec.push(self.stdin[self.stdin_cursor]);
            if *output_vec.last().unwrap() == b'\n' {
                break;
            }
            self.stdin_cursor += 1;
        }
        output_vec
        
    }
    fn stdin_line(&mut self) -> Vec<u8> {
        let mut output_vec = vec![];
        loop {
            while self.stdin.len() == self.stdin_cursor {
                std::thread::park();
            }
            output_vec.push(self.stdin[self.stdin_cursor]);
            if *output_vec.last().unwrap() == b'\n' {
                break;
            }
            self.stdin_cursor += 1;
        }
        output_vec
    }
}
