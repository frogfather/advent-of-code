using System;
using System.Collections;
using System.IO;

namespace day3
{
    class Program
    {
        static void Main(string[] args)
        {
            int xpos = 0;
            int ypos = 0;
            int mapWidth = 0;
            int mapHeight = 0;
            int blockNumber = 0;
            int trees = 0;
            ArrayList map = readFile("/Users/johnPersonal/Documents/code/advent/day_3/day3/data.txt");
            if (map.Count > 0)
            {
                mapWidth = (map[0].ToString()).Length;
                mapHeight = map.Count;
            }
            while (ypos < mapHeight - 1)
            {
                ypos += 1;
                xpos += 3;
                if (xpos >= mapWidth)
                {
                    xpos = xpos - mapWidth;
                    blockNumber += 1;
                }
                //have we hit a tree?
                if ((map[ypos].ToString())[xpos] == '#')
                {
                    trees += 1;
                }
            }
            Console.WriteLine("We have hit " + trees + " trees");
        }


        static ArrayList readFile(string filename)
        {
            ArrayList data = new ArrayList();
            using (FileStream fileStream =
            File.Open(@filename, FileMode.Open, FileAccess.Read))
            using (StreamReader sr = new StreamReader(fileStream))
            {
                while (sr.Peek() >= 0)
                {
                    data.Add(sr.ReadLine());
                }
                return data;
            }

        }

    }
}
