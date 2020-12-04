
def validateHeight(height)
  heightValue = height.delete('^0-9')
  heightValue.length > 0 &&
  ((height.end_with?("cm") &&  heightValue.to_i >= 150 && heightValue.to_i <= 193) ||
  (height.end_with?("in") && heightValue.to_i >= 59 && heightValue.to_i <= 76))
end

def validateHairColour(colour)
  colour.match(/^#[0-9a-f]{6}/)
end

def validateEyeColour(colour)
  options = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  colour.length == 3 && options.include?(colour)
end

def validateBirthYear(year)
  year.length == 4 && year.to_i >= 1920 && year.to_i <= 2002
end

def validateIssueYear(year)
  year.length == 4 && year.to_i >= 2010 && year.to_i <= 2020
end

def validateExpireYear(year)
  year.length == 4 && year.to_i >= 2020 && year.to_i <= 2030
end

def validatePassport(passport)
  passport.length == 9
end

def validate(doc)
  validateBirthYear(doc["byr"]) &&
  validateIssueYear(doc["iyr"]) &&
  validateExpireYear(doc["eyr"]) &&
  validateHeight(doc["hgt"]) &&
  validateHairColour(doc["hcl"]) &&
  validateEyeColour(doc["ecl"]) &&
  validatePassport(doc["pid"])

#   byr (Birth Year) - four digits; at least 1920 and at most 2002.
#   iyr (Issue Year) - four digits; at least 2010 and at most 2020.
#   eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
#   hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
#   hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
#   ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
#   pid (Passport ID) - a nine-digit number, including leading zeroes.
#   cid (Country ID) - ignored, missing or not.
end


def readFile
  requiredKeys = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]
  file = File.open("data.txt")
  contents = file.read.split("\n\n")
  documents = contents.map do |doc|
    doc_hash = {}
    doc.split(/\s+/).map do |e|
      kv = e.split(":")
      doc_hash[kv[0]] = kv[1]
    end
    doc_hash
  end
  p(documents[0].keys)
  p("Length before filtering #{documents.length()}")
  documents.select!{|doc| (doc.keys & requiredKeys).length() == requiredKeys.length && validate(doc)}
  p("Length after filtering #{documents.length()}")
  file.close
end

readFile
