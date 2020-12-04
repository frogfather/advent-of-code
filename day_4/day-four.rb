

def readFile
  requiredKeys = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]
  file = File.open("data.txt")
  contents = file.read.split("\n\n")
  separated = contents.map {|item| item.split(/\s+/).flat_map { |e| e.split(":")[0]  }}
  p("Length before filtering #{separated.length()}")
  separated.select!{|n| (n & requiredKeys).length() == requiredKeys.length }
  p("Length after filtering #{separated.length()}")
  file.close
end

readFile
