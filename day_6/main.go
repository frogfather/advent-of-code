package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)


func unique(strSlice []string) []string {
    keys := make(map[string]bool)
    list := []string{}
    for _, entry := range strSlice {
        if _, value := keys[entry]; !value {
            keys[entry] = true
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
        sumOfResults = sumOfResults + len(unique(results))
        //empty the results slice
        results = nil
      } else {
        //append it to the results
        results = append(results, answers...)
      }
  }
  fmt.Println("Total of all answers is " + strconv.Itoa(sumOfResults))
}
