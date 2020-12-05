class Node:

    def __init__(self, name, low, high):
    	self.left = None
	self.right = None
	self.name = name
        self.low = low
	self.high = high

    def insertLeft(self, name, low, high):
        print('insert left node called '+name+' into '+self.name+' with range '+str(low)+':'+str(high))
        self.left = Node(name, low, high) 
        return self.left        

    def insertRight(self, name, low, high):
        print('insert right node called '+name+' into '+self.name+' with range '+str(low)+':'+str(high))
        self.right = Node(name, low, high)
        return self.right

    def PrintTree(self):
        print('Node: '+self.name+' has range: '+str(self.low)+ ':' + str(self.high))
        if self.left:
            self.left.PrintTree()
        # print('Node: '+self.name+' has range: '+str(self.low)+ ':' + str(self.high)),
        if self.right:
            self.right.PrintTree()

    def AddNodes(self, node):
        if  node.high != node.low:
            leftLow = node.low
            rightLow = node.low + (node.high + 1 - node.low)//2 
            rightHigh = node.high
            leftHigh = rightLow - 1
            leftName = str(leftLow) + '-' + str(leftHigh)
            rightName = str(rightLow) + '-' + str(rightHigh)
            newLeft = node.insertLeft(leftName, leftLow, leftHigh)
            node.AddNodes(newLeft)
            newRight = node.insertRight(rightName, rightLow, rightHigh)
            node.AddNodes(newRight)

    def FindTheRow(self, input):
        rowData = input[:7]
        print('Input is '+rowData)
        currentNode = self
        for letter in rowData:
            if letter == "F":
                currentNode = currentNode.left
            else:
                currentNode = currentNode.right            
        print('Input corresponds to row '+str(currentNode.low))        

    def ReadTheFile(self, filename):
        file = open(filename)
        fileList = file.readlines()
        print('List contains '+str(len(fileList))+' items')
        return fileList

    def FindTheRows(self, fileList):
       results = []
       for row in fileList:
           results.append(root.FindTheRow(row))
       print('there are '+str(len(results))+' results')
       return results

root = Node('root', 0, 127)
root.AddNodes(root)
# root.PrintTree()
fileList = root.ReadTheFile("data.txt")
rowsFound = root.FindTheRows(fileList)

