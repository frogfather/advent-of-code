
def FindTheRow(input):
    rowData = input[:7]
    sum = 0
    for i in range( len(rowData) ):
        divisor = 2 ** i
        bitValue = 64 // divisor
        if rowData[i] == "B":
            sum += bitValue
    print('Sum for '+rowData+' is '+str(sum))        
    return sum  

def FindTheSeat(input):
    seatData = input[-4:]
    print('seat data from '+input+' is '+seatData);
    sum = 0
    for i in range( len(seatData) ):
        divisor = 2 ** i
        bitValue = 4 // divisor
        if seatData[i] == "R":
            sum += bitValue
    print('Sum for '+seatData+' is '+str(sum))
    return sum

def ReadTheFile(filename):
    file = open(filename)
    fileList = file.readlines()
    return fileList

def FindMaxSeatId(fileList, max):
    maxSeatId = 0
    occupiedSeats = []
    for entry in fileList:
       calculatedRow = FindTheRow(entry)
       calculatedSeat = FindTheSeat(entry)
       seatId = calculatedRow * 8 + calculatedSeat
       occupiedSeats.append(seatId)
       if seatId > maxSeatId:
           maxSeatId = seatId
    if max == True:
        return maxSeatId
    else:
        print('returning list');
        return occupiedSeats

def FindUnoccupiedSeats(occupiedList):
    unoccupiedSeats = []
    for x in range(1024):
        if x not in occupiedList:
            print(str(x)+' not in occupiedList')
            unoccupiedSeats.append(x)
    return unoccupiedSeats        


fileList = ReadTheFile("data.txt")
maxSeatId = FindMaxSeatId(fileList, True)
occupiedList = FindMaxSeatId(fileList, False)
print('The max seat id is '+str(maxSeatId))
unoccupiedList = FindUnoccupiedSeats(occupiedList)
print('The unoccupied list contains '+str(len(unoccupiedList))+' items')
