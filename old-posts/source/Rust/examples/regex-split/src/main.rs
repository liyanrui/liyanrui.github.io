use regex::{Regex, Error};

fn main() -> Result<(), Error> {
    let re = Regex::new("[ \t]*#[ \t]*\n")?;
    let v = re.split("num
  @ 123 #
  adsfa asdflja fasdlfj 
    @456
  @ 789");
    for i in v {
        println!("{}", i);
    }
    return Ok(());
}
