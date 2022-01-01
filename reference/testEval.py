with open("./day_18_1.txt") as fin:
    raw_data = fin.read().strip().split("\n")
data = eval(raw_data[0]) 

print(data)