use chumsky::Parser;
use mips_weaver::{parse::program_parser, Config};

#[test]
fn test_afl_crashes() {
    let cfg = Config::default();
    let parser = program_parser(&cfg);
    if let Ok(dir) = std::fs::read_dir("fuzz/out/parse/crashes") {
        for file in dir {
            let data = std::fs::read(format!(
                "fuzz/out/default/crashes/{}",
                file.unwrap().file_name().into_string().unwrap().as_str()
            ))
            .unwrap();
            if let Ok(s) = std::str::from_utf8(data.as_ref()) {
                println!("{:?}", s);
                if let Ok(mut mem) = parser.parse(s) {
                    mem.program_counter = 0x0040_0000;
                    let _ = mem.run();
                    while let Some(_) = mem.undo() {}
                }
            }
        }
    }
}
