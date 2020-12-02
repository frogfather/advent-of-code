const express = require('express')
const fs = require('fs')
const app = express()
const port = 3000

app.get('/', (req, res) => {
  //read the file and do the calculation
  var data = readTheFile();
  res.send("something");
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})

function readTheFile() {
  console.log('reading the file');
  fs.readFile('data.txt', 'utf-8', (err, data) => {
    if (err) throw err;
    return processTheData(data);
})
}

function processTheData(data) {
  //split into array
  var array = data.split("\n");
  //separate each item into three parts
  //first is range, second is letter, third is password
  var validPasswords = 0;
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
        var re = new RegExp(element, 'g');
        var occurrences = (password.match(re) || []).length;
        if (occurrences <= high && occurrences >= low) {
          console.log(item + ' valid');
          validPasswords += 1;
        }
      }
    }
  })
  console.log(validPasswords);
}
