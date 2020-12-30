use syntax::TextRange;

#[derive(Copy, Clone, Eq, PartialEq)]
struct Pos {
    line: usize,
    col: usize,
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.line, self.col)
    }
}

struct Range {
    lo: Pos,
    hi: Pos,
}

fn to_line_col(contents: &str, at: TextRange) -> Range {
    let bytes = contents.as_bytes();

    let mut lo = PosState::AtLeastLine(0);
    let mut hi = PosState::AtLeastLine(0);

    #[derive(Copy, Clone)]
    enum PosState {
        AtLeastLine(usize),
        Found(Pos),
    }

    impl PosState {
        pub fn line_start(self, line: usize) -> PosState {
            match self {
                PosState::AtLeastLine(_) => {
                    PosState::AtLeastLine(line)
                },
                good => good,
            }
        }

        pub fn line(self, line: usize, line_start_ix: usize, next_line_start_ix: usize, line_end: usize, looking_pos: usize) -> PosState {
            match self {
                PosState::AtLeastLine(at_least_line) => {
                    if looking_pos >= line_start_ix && looking_pos < next_line_start_ix {
                        PosState::Found(Pos {
                            line,
                            col: if looking_pos >= line_end { line_end - line_start_ix } else { looking_pos - line_start_ix },
                        })
                    } else {
                        PosState::AtLeastLine(at_least_line)
                    }
                },
                good => good,
            }
        }
    }

    let looking_start = u32::from(at.start()) as usize;
    let looking_end = u32::from(at.end()) as usize;

    let mut line: usize = 0;
    let mut ix: usize = 0;
    'lp: loop {
        lo = lo.line_start(line);
        hi = hi.line_start(line);

        if ix as usize == bytes.len() {
            break 'lp;
        }

        let slice_starting_at_line = &bytes[ix as usize..];
        let mut found = false;
        for (i, c) in slice_starting_at_line.into_iter().enumerate() {
            if *c == b'\n' {
                let next_line_start_ix = ix + i + 1;
                let line_end_ix = if i > 0 && slice_starting_at_line[i - 1] == b'\r' {
                    ix + i - 1
                } else {
                    ix + i
                };

                lo = lo.line(line, ix, next_line_start_ix, line_end_ix, looking_start);
                hi = hi.line(line, ix, next_line_start_ix, line_end_ix, looking_end);

                ix = next_line_start_ix;
                line += 1;

                found = true;
                break;
            }
        }

        if !found {
            ix = bytes.len();
            line += 1;
        }

        if let (PosState::Found(_), PosState::Found(_)) = (lo, hi) {
            break;
        }
    }

    Range {
        lo: match lo {
            PosState::AtLeastLine(line) => Pos { line, col: 0 },
            PosState::Found(pos) => pos,
        },
        hi: match hi {
            PosState::AtLeastLine(line) => Pos { line, col: 0 },
            PosState::Found(pos) => pos,
        }
    }
}

pub fn display(contents: &str, at: TextRange, desc: &str) -> String {
    let range = to_line_col(contents, at);
    let mut extra_message = None;

    let mut lines: Option<Vec<String>> = None;

    for (i, rd_line) in contents.lines().enumerate() {
        if i + 5 > range.lo.line && i <= range.lo.line {
            let line = if rd_line.len() > 80 {
                format!("{}..", &rd_line[..78])
            } else {
                rd_line.to_string()
            };
            if let Some(ref mut lines) = lines {
                lines.push(line);
            } else {
                lines = Some(vec![line])
            }
        }
    }
    if let None = lines {
        lines = Some(vec![String::from("")]);
    }

    if let Some(lines) = lines {
        let mut sb = String::new();

        // print lines

        let lines_len = lines.len();
        let mut num_len = 0;
        for (i, line) in lines.into_iter().enumerate() {
            let num = format!("{} ", range.lo.line + i + 2 - lines_len);
            num_len = num.len();

            sb.push_str(&num);
            sb.push_str("| ");
            sb.push_str(&line);
            sb.push_str("\n");
        }

        // print arrow

        for _ in 0..num_len {
            sb.push_str(" ");
        }
        sb.push_str("| ");

        for _ in 0..range.lo.col {
            sb.push_str(" ");
        }
        sb.push_str("^");
        for _ in range.lo.col + 1..range.hi.col {
            sb.push_str("^");
        }

        sb.push_str("\n");

        // print message

        for _ in 0..num_len {
            sb.push_str(" ");
        }
        sb.push_str("| ");

        for _ in 0..range.lo.col {
            sb.push_str(" ");
        }
        sb.push_str(&format!("{}", desc));

        extra_message = Some(sb);
    }

    if let Some(extra_message) = extra_message {
        format!("{}", extra_message)
    } else {
        if range.lo == range.hi {
            format!("{} at {}", desc, range.lo)
        } else {
            format!(
                "{} at {} - {}",
                desc, range.lo, range.hi
            )
        }
    }
}