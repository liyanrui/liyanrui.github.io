
#[derive(Debug, serde::Deserialize)]
struct Separator {
    border: String,
    code_snippet_neck: String
}

fn main() -> Result<(), std::io::Error>  {
    let f = std::fs::File::open("rzeo.conf")?;
    let foo: Separator = serde_yaml::from_reader(f).unwrap();
    println!("{:?}", foo);
    return Ok(()); 
}
