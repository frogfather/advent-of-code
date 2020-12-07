use std::io::Read;
use std::collections::HashMap;

fn
main(){
  //String containing all the data
  let mut bags_and_contents: HashMap<String,String> = HashMap::new();
  let file_content:String = read_file("data.txt");
  let lines: Vec<&str> = file_content.trim().split("\n").collect();
  for line in lines.iter() {
     let bag_with_contents = line.trim().split("contain");
     let word: Vec<&str> = bag_with_contents.collect();
     let bag: String = word[0].to_string();
     let contents: String = word[1].to_string();
     bags_and_contents.insert(bag, contents);
  }
  let hash_len = bags_and_contents.len();
  println!("hash length {}", hash_len.to_string())
}

fn
read_file(file_name: &str) -> String {
    let mut file = std::fs::File::open(file_name).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    return contents;
}
