use regex::{Regex, Error};

#[derive(Debug)]
struct TreeNode<'a> {
    data: &'a str,
    children: Vec<Box<TreeNode<'a>>>
}

fn main() -> Result<(), Error>{
    let mut root = Box::new(TreeNode {data: "以下 Rust 程序
@ hello world #
fn main() {
    println!(\"Hello world!\");
}
@
可在终端打印「Hello world!」。", children: vec![]});
    // 第一次分割
    let re = Regex::new("\n[ \t]*@[ \t\n]*")?;
    let v = re.split(root.data);
    for i in v {
        let child = Box::new(TreeNode {data: i, children: vec![]});
        root.children.push(child);
    }
    // 第二次分割
    let re2 = Regex::new("[ \t]*#[ \t]*\n")?;
    let child_2nd = &mut root.children[1];
    let v2 = re2.split(child_2nd.data);
    for i in v2 {
        let child = Box::new(TreeNode {data: i, children: vec![]});
        child_2nd.children.push(child);
    }
    println!("{:?}", root);
    return Ok(());
}
