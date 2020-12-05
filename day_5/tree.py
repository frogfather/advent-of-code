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
        rowData = input.strip("LR");
        print('Input is '+rowData)
        currentNode = self
        for letter in rowData:
            if letter == "F":
                print('Selecting node '+currentNode.left.name) 
                currentNode = currentNode.left
            else:
                print('Selecting node '+currentNode.right.name)
                currentNode = currentNode.right            
        print('Input corresponds to row '+str(currentNode.low))        


root = Node('root', 0, 127)
root.AddNodes(root)
# root.PrintTree()
root.FindTheRow("FBFBBFFRLR")
