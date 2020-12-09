use std::io::Read;
use std::collections::HashMap;

fn
main(){
  let file_content:String = read_file("data.txt");
  let bags_and_contents = file_content.trim()
                        .split("\n")
                        .map(|line| line.split(" contain"))
                        .map(|mut line| (line.next().unwrap().into(), line.next().unwrap().into()))
                        .collect::<HashMap<String, String>>();

  let mut bags_already_found: Vec<String> = Vec::new();
  let my_bag: String = String::from("shiny gold bag");
  //my bag is not valid as an outer bag
  find_my_bag(&my_bag, &bags_and_contents, &mut bags_already_found);
  let res_after_len = bags_already_found.len();
  println!("results length before calling {}", res_after_len);
  let mut bag_count:u32 = 0;
  let mut bag_multiplier:u32 = 1;
  find_my_bag_count(&my_bag, &bags_and_contents, &mut bag_count, &mut bag_multiplier);
  println!("count of bags is {}", bag_count);
}

fn
find_my_bag(bag: &str, bags_and_contents: &HashMap<String, String>, results: &mut Vec<String>) {
    for (k,v) in bags_and_contents {
        if v.contains(bag) && !results.contains(k) {
            results.push(k.to_string());
            let mut key = k.to_string();
            key.remove(key.len()-1);
            find_my_bag(&key, bags_and_contents, results);
        }
    }
}

fn
find_my_bag_count(bag: &str, bags_and_contents: &HashMap<String, String>, count: &mut u32, multiplier: &mut u32) {
    //get the contents of the bag, sum the items and for each item call this
    const RADIX: u32 = 10;
    let mut str = String::from(bag);
    let last_char: char = str.chars().last().unwrap();
    if last_char != 's' {
        println!("last char is not s");
        str.push('s');
    }
    println!("calling with |{}| ", str);

    let s_contents = &bags_and_contents[&str];
    let mut contents = s_contents.to_string();
    contents.pop();
    println!("found |{}| ", contents);
    if contents.trim() != "no other bags" {
    let bags_inside: Vec<&str> = contents.trim().split(",").collect();
    for bag in bags_inside.iter() {
        let bag_and_number: String = bag.trim().to_string();
        //separate the first char from the rest and trim
        let bag_number = bag_and_number.trim().chars().next().unwrap_or('0');
        //convert to int
        let bag_no: u32 = bag_number.to_digit(RADIX).unwrap_or(0);
        //now lets get the rest of the string
        let bag_colour = &bag_and_number[1..].trim();
        //now increment the counter and call this method again
        *count += bag_no;
        *multiplier = *multiplier * bag_no;
        println!("count is now {} ", count);
        find_my_bag_count(&bag_colour, bags_and_contents, count, multiplier);
    }
}
}

fn
read_file(file_name: &str) -> String {
    let mut file = std::fs::File::open(file_name).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    return contents;
}
