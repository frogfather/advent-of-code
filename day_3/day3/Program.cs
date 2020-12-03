using System;
using System.Collections;
using System.IO;

namespace day3
{
    class Program
    {
        static void Main(string[] args)
        {
            ArrayList map = readFile("/Users/johnPersonal/Documents/code/advent/day_3/day3/data.txt");
            int[] xOffsets = { 1, 3, 5, 7, 1 };
            int[] yOffsets = { 1, 1, 1, 1, 2 };
            int[] results = new int[5];
            for (int run = 0; run < 5; run ++)
            {
                results.SetValue(goTobogganing(map, xOffsets[run], yOffsets[run]), run);
            }

            Console.WriteLine("Results {0}, {1}, {2}, {3}, {4}", results.GetValue(0), results.GetValue(1), results.GetValue(2), results.GetValue(3), results.GetValue(4));
            long product = results[0];
            for (int i = 1; i < 5; i++)
            {
                product = product * results[i];
            }
            Console.WriteLine("Product of all 5 answers is {0}", product);
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

        static int goTobogganing(ArrayList map, int xOffset, int yOffset)
        {
            int xpos = 0;
            int ypos = 0;
            int mapWidth = 0;
            int mapHeight = 0;
            int blockNumber = 0;
            int trees = 0;

            if (map.Count > 0)
            {
                mapWidth = (map[0].ToString()).Length;
                mapHeight = map.Count;
            }
            Console.WriteLine("map is {0} by {1}", mapWidth, mapHeight);
            Console.WriteLine("offsets are {0} and {1}", xOffset, yOffset);
            while (ypos < mapHeight - 1)
            {
                ypos += yOffset;
                xpos += xOffset;
                if (xpos >= mapWidth)
                {
                    xpos = xpos - mapWidth;
                    blockNumber += 1;
                }
                //have we hit a tree?
                if ((ypos < mapHeight) && (map[ypos].ToString())[xpos] == '#')
                {
                    trees += 1;
                }
            }
            return trees;
        }

    }
}
