package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)


func unique(strSlice []string, groupSize int) []string {
    keys := make(map[string]int)
    list := []string{}
    for _, entry := range strSlice {
        //we want to count the number of occurrences of each letter
        //if that matches the number in the group the letter is included
        keys[entry] += 1
        if keys[entry] == groupSize {
        list = append(list, entry)  
        }
    }
    return list
}


func main() {
  file, err := os.Open("data.txt")
  if err != nil {
    // handle the error here
    return
  }
  defer file.Close()

  // get the file size
  stat, err := file.Stat()
  if err != nil {
    return
  }
  // read the file
  bs := make([]byte, stat.Size())
  _, err = file.Read(bs)
  if err != nil {
    return
  }

  str := string(bs)
  parts := strings.Split(str,"\n")
  sumOfResults := 0
  groupSize := 0
  //slice to hold results
  results := []string{}
  for i := range parts {
      //convert the element to string
      sLine := parts[i]
      //turn it into an slice
      answers:= strings.Split(sLine, "")
      //if its length is 0 it's a blank Line
      if len(sLine) == 0 {
        //run the results through the unique function
        sumOfResults = sumOfResults + len(unique(results, groupSize))
        //empty the results slice
        results = nil
        groupSize = 0
      } else {
        //append it to the results
        groupSize = groupSize + 1
        results = append(results, answers...)
      }
  }
  fmt.Println("Total of all answers is " + strconv.Itoa(sumOfResults))
}
