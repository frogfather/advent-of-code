const express = require('express')
const fs = require('fs')
const app = express()
const port = 3000

app.get('/', (req, res) => {
  //read the file and do the calculation
  fs.readFile('data.txt', 'utf-8', (err, data) => {
    if (err) throw err;
    var answer1 = processTheData(data, 1);
    var answer2 = processTheData(data, 2);
    res.send('answer to part one is '+ answer1+' and answer to part two is '+answer2);
  })
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})

function processTheData(data, part) {
  //split into array
  var array = data.split("\n");
  //separate each item into three parts
  //first is range, second is letter, third is password
  var validPasswords1 = 0;
  array.forEach((item, index) => {
    var sections = item.split(' ');
    if (sections.length === 3) {
      var range = sections[0];
      var rangeArr = range && range.split("-");
      if (rangeArr && rangeArr.length ===2) {
        var low = rangeArr[0];
        var high = rangeArr[1];
        var element = sections[1].slice(0, -1);
        var password = sections[2];
        //code up to here is common for both part 1 and part 2
        if (part === 1) {
          var re = new RegExp(element, 'g');
          var occurrences = (password.match(re) || []).length;
          if (occurrences <= high && occurrences >= low) {
            validPasswords1 += 1;
          }
        } else if (part === 2) {
          var firstLetterMatch = password[low -1] === element;
          var secondLetterMatch = password[high -1] === element;
          //and exactly one of these should match the required element
          //so if the two booleans are the same the password is invalid
          if (firstLetterMatch !== secondLetterMatch) {
            validPasswords1 += 1;
          }
        }
      }
    }
  })
  return validPasswords1;
}
