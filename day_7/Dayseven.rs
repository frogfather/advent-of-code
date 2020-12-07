use std::io::Read;
use std::collections::HashMap;

fn
main(){
  //String containing all the data
  let file_content:String = read_file("data.txt");
  let bags_and_contents = file_content
                        .split("\n")
                        .map(|line| line.split("contain"))
                        .map(|mut line| (line.next().unwrap().into(), line.next().unwrap().into()))
                        .collect::<HashMap<String, String>>();




  // let lines: Vec<&str> = file_content.split("\n").collect();
  //
  // let bags_and_contents = lines.split("contain")
  //                   .map(|mut line| (line.next().unwrap().into(), line.next().unwrap().into()))
  //                   .collect::<HashMap<String, String>>();


  // for line in lines.iter() {
  //     let bags_and_contents: Vec<&str> = line.split("contain").collect();
  //     for (index, element) in bags_and_contents.iter().enumerate() {
  //         print!(" index {} is {}", index.to_string(), element);
  //     }
  // }



  // let bags_and_contents = file_content
  //                       .split("\n")
  //                       .map(|line| line.split("contain"))
  //                       .map(|mut line| (line.next().unwrap().into(), line.next().unwrap().into()))
  //                       .collect::<HashMap<String, String>>();

  let item_count = file_content.len();
  print!("{}", item_count.to_string());

}

fn
read_file(file_name: &str) -> String {
    let mut file = std::fs::File::open(file_name).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    return contents;
}
