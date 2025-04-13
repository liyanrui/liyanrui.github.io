#[derive(Debug)]
enum Status {
    Init, // 初始状态
    Empty, // 为空或不少于 1 个空格
    Append, // 前后可能存在空格的 +
    Prepend, // 前后可能存在空格的 ^+
    MaybePrepend // 可能是 prepend 状态
}

fn main() {
    let s = "    ^+ ";
    let mut m = Status::Init;
    for c in s.chars() {
        match m {
            Status::Init => {
                match c {
                    ' ' => m = Status::Empty,
                    _ => panic!("Unknown text!")
                }
            },
            Status::Empty => {
                match c {
                    ' ' => {},
                    '+' => m = Status::Append,
                    '^' => m = Status::MaybePrepend,
                    _ => panic!("Unknown text!")
                }
            },
            Status::Append => {
                match c {
                    ' ' => {},
                    _ => panic!("Unknown text!")
                }
            },
            Status::MaybePrepend => {
                match c {
                    '+' => m = Status::Prepend,
                    _ => panic!("Unknown text!")
                }
            },
            Status::Prepend => {
                match c {
                    ' ' => {},
                    _ => panic!("Unknown text!")
                }
            }
        }
    }
    println!("{:?}", m);
}
