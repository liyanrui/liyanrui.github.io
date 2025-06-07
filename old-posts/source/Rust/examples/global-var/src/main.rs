use regex::Regex;
static mut A: Option<Regex> = None;

fn main() {
    unsafe {
        let re = Regex::new(" *@ *").unwrap();
        A = Some(re);
        let v = A.as_ref().unwrap().split("num@ 123@456  @ 789");
        for i in v {
            println!("{}", i);
        }        
    }
}

