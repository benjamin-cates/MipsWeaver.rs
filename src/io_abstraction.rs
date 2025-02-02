use std::{
    collections::BTreeMap,
    fmt::Debug,
    fs::File,
    io::{BufRead, Read, Seek, SeekFrom, Write},
    sync::{Arc, Mutex},
};

#[derive(Clone, PartialEq, Debug)]
pub enum IOSystem {
    Standard(StandardIoSystem),
    Virtual(VirtualIoSystem),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum FileMode {
    Read,
    Write,
    Append,
}

#[derive(Debug, Clone, Default)]
pub struct StandardIoSystem {
    pub file_descriptors: BTreeMap<i32, (String, FileMode, Arc<Mutex<File>>)>,
}
impl PartialEq for StandardIoSystem {
    fn eq(&self, other: &Self) -> bool {
        self.file_descriptors.len() == other.file_descriptors.len()
            && self
                .file_descriptors
                .iter()
                .zip(other.file_descriptors.iter())
                .all(
                    |((fd1, (str1, mode1, file1)), (fd2, (str2, mode2, file2)))| {
                        str1 == str2 && fd1 == fd2 && mode1 == mode2 && Arc::ptr_eq(file1, file2)
                    },
                )
    }
}

impl StandardIoSystem {
    pub fn open(&mut self, path: String, mode: FileMode) -> i32 {
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
                    .insert(assigned_fd, (path, mode, Arc::new(Mutex::new(file))));
                assigned_fd
            }
            Err(_) => -1,
        }
    }

    pub fn close(&mut self, fd: i32) {
        let _ = self.file_descriptors.remove(&fd);
    }

    pub fn write(&mut self, fd: i32, bytes: &[u8]) -> Result<usize, ()> {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, mode, file)) => {
                if *mode == FileMode::Read {
                    return Err(());
                }
                file.lock().unwrap().write(bytes).ok().ok_or(())
            }
            None => {
                if fd == 1 {
                    std::io::stdout().write(bytes).ok().ok_or(())
                } else if fd == 2 {
                    std::io::stderr().write(bytes).ok().ok_or(())
                } else {
                    Err(())
                }
            }
        }
    }
    pub fn read_buffered(&mut self, fd: i32, length: usize) -> Result<Vec<u8>, ()> {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, mode, file)) => {
                if *mode != FileMode::Read {
                    return Err(());
                }
                let mut output = vec![0; length];
                match file.lock().unwrap().read(&mut output) {
                    Ok(num_read) => {
                        output.resize(num_read, 0);
                        Ok(output)
                    }
                    Err(_) => Err(()),
                }
            }
            None => {
                if fd == 0 {
                    return Ok(self.stdin_bytes_buffered(length));
                }
                Err(())
            }
        }
    }

    pub fn read_line(&mut self, fd: i32) -> Vec<u8> {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, mode, file)) => {
                if *mode != FileMode::Read {
                    return vec![];
                }
                let mut line: Vec<u8> = vec![];
                let mut lock = file.lock().unwrap();
                let num_read = std::io::BufReader::new(&mut *lock).read_until(0xA, &mut line);
                match num_read {
                    Ok(_num_read) => {
                        let cur_position = lock.stream_position().unwrap_or(0);
                        let _ = lock.seek(SeekFrom::Start(cur_position + line.len() as u64));
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

    pub fn seek(&mut self, fd: i32, position: u64) {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, file)) => {
                let _ = file.lock().unwrap().seek(SeekFrom::Start(position));
            }
            None => {}
        }
    }

    pub fn get_pos(&mut self, fd: i32) -> u64 {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, file)) => file.lock().unwrap().stream_position().unwrap_or(0),
            None => 0,
        }
    }
    pub fn get_path(&mut self, fd: i32) -> String {
        match self.file_descriptors.get_mut(&fd) {
            Some((path, _mode, _)) => path.clone(),
            None => "".to_string(),
        }
    }
    pub fn stdin_line(&mut self) -> Vec<u8> {
        let mut buf = String::new();
        let _ = std::io::stdin().read_line(&mut buf);
        buf.into()
    }
    pub fn stdin_bytes_buffered(&mut self, length: usize) -> Vec<u8> {
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
    pub fn stdout_bytes(&mut self, output: &[u8]) {
        let _ = std::io::stdout().write(output);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VirtualIoSystem {
    pub files: BTreeMap<String, Vec<u8>>,
    pub file_descriptors: BTreeMap<i32, (String, FileMode, u64)>,
    pub cwd: Vec<String>,
    pub stdin_cursor: usize,
    pub stdin: Vec<u8>,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl Default for VirtualIoSystem {
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

impl VirtualIoSystem {
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

impl VirtualIoSystem {
    pub fn open(&mut self, path: String, mode: FileMode) -> i32 {
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

    pub fn close(&mut self, fd: i32) {
        let _ = self.file_descriptors.remove(&fd);
    }

    pub fn stdout_bytes(&mut self, output: &[u8]) {
        self.stdout.extend_from_slice(output);
    }
    pub fn write(&mut self, fd: i32, bytes: &[u8]) -> Result<usize, ()> {
        match self.file_descriptors.get_mut(&fd) {
            Some((abs_path, mode, cursor)) => {
                if *mode == FileMode::Read {
                    return Err(());
                }
                let file = self.files.get_mut(abs_path).unwrap();
                let end = *cursor as usize + bytes.len();
                let new_file = file
                    .splice((*cursor as usize)..end, bytes.iter().cloned())
                    .collect::<Vec<u8>>();
                *file = new_file;
                return Ok(bytes.len());
            }
            None => {
                if fd == 1 {
                    self.stdout.extend_from_slice(bytes);
                    return Ok(bytes.len());
                }
                if fd == 2 {
                    self.stdout.extend_from_slice(bytes);
                    return Ok(bytes.len());
                }
                return Err(());
            }
        }
    }

    pub fn read_buffered(&mut self, fd: i32, length: usize) -> Result<Vec<u8>, ()> {
        match self.file_descriptors.get_mut(&fd) {
            Some((abs_path, mode, cursor)) => {
                if *mode != FileMode::Read {
                    return Err(());
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
                Ok(output)
            }
            None => {
                if fd == 0 {
                    return Ok(self.stdin_bytes_buffered(length));
                }
                Err(())
            }
        }
    }

    pub fn read_line(&mut self, fd: i32) -> Vec<u8> {
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

    pub fn seek(&mut self, fd: i32, position: u64) {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, cursor)) => {
                *cursor = position;
            }
            None => {}
        }
    }

    pub fn get_pos(&mut self, fd: i32) -> u64 {
        match self.file_descriptors.get_mut(&fd) {
            Some((_path, _mode, cursor)) => *cursor,
            None => 0,
        }
    }
    pub fn get_path(&mut self, fd: i32) -> String {
        match self.file_descriptors.get_mut(&fd) {
            Some((path, _, _)) => path.clone(),
            None => "".to_string(),
        }
    }
    pub fn stdin_bytes_buffered(&mut self, length: usize) -> Vec<u8> {
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
    pub fn stdin_line(&mut self) -> Vec<u8> {
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

impl IOSystem {
    /// Closes a file.
    pub fn close(&mut self, fd: i32) {
        match self {
            Self::Standard(s) => s.close(fd),
            Self::Virtual(v) => v.close(fd),
        }
    }
    /// Returns the name of a file in the file descriptor
    pub fn get_path(&mut self, fd: i32) -> String {
        match self {
            Self::Standard(s) => s.get_path(fd),
            Self::Virtual(v) => v.get_path(fd),
        }
    }
    /// Returns the position of the cursor in a file.
    /// Mostly used for do and undo
    pub fn get_pos(&mut self, fd: i32) -> u64 {
        match self {
            Self::Standard(s) => s.get_pos(fd),
            Self::Virtual(v) => v.get_pos(fd),
        }
    }
    /// Opens a file and returns it's file descriptor
    /// If the file does not exist (and is being read), this returns -1
    pub fn open(&mut self, name: String, mode: FileMode) -> i32 {
        match self {
            Self::Standard(s) => s.open(name, mode),
            Self::Virtual(v) => v.open(name, mode),
        }
    }
    /// Read bytes from a file, up to the length given
    /// If the end of the file is reached, returns an empty vector
    pub fn read_buffered(&mut self, fd: i32, length: usize) -> Result<Vec<u8>, ()> {
        match self {
            Self::Standard(s) => s.read_buffered(fd, length),
            Self::Virtual(v) => v.read_buffered(fd, length),
        }
    }
    /// Read line from a file.
    /// If the end of the file is reached, returns an empty vector
    pub fn read_line(&mut self, fd: i32) -> Vec<u8> {
        match self {
            Self::Standard(s) => s.read_line(fd),
            Self::Virtual(v) => v.read_line(fd),
        }
    }
    /// Changes the position of the cursor in a file to a new position
    /// Mostly used for do and undo
    pub fn seek(&mut self, fd: i32, position: u64) {
        match self {
            Self::Standard(s) => s.seek(fd, position),
            Self::Virtual(v) => v.seek(fd, position),
        }
    }

    /// Reads buffered bytes from stdin, up to the length specified
    /// And stops on a newline
    /// Equivalent to fgets in C
    pub fn stdin_bytes_buffered(&mut self, length: usize) -> Vec<u8> {
        match self {
            Self::Standard(s) => s.stdin_bytes_buffered(length),
            Self::Virtual(v) => v.stdin_bytes_buffered(length),
        }
    }
    /// Reads a line from stdin, pauses if no line is available
    pub fn stdin_line(&mut self) -> Vec<u8> {
        match self {
            Self::Standard(s) => s.stdin_line(),
            Self::Virtual(v) => v.stdin_line(),
        }
    }
    /// Prints a sequence of bytes to stdout
    pub fn stdout_bytes(&mut self, output: &[u8]) {
        match self {
            Self::Standard(s) => s.stdout_bytes(output),
            Self::Virtual(v) => v.stdout_bytes(output),
        }
    }
    /// Write bytes to the file. Returns number of bytes written on success or Err(()) if the file descriptor is invalid.
    pub fn write(&mut self, fd: i32, bytes: &[u8]) -> Result<usize, ()> {
        match self {
            Self::Standard(s) => s.write(fd, bytes),
            Self::Virtual(v) => v.write(fd, bytes),
        }
    }
}
