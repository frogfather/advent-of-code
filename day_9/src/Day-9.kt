import java.io.File

fun main(args: Array<String>) {
   var  numbers: List<Long> = readFileAsLinesUsingReadLines("/Users/johnPersonal/Documents/code/advent/day_9/data.txt")
	.map{it.toLong()};
	
	for (i in 25..numbers.size -1) {
		var seeds: List<Long> = numbers.subList(i-25, i);
		var target = numbers[i];
		if (!seedsCanMakeTarget(seeds, target)) {
			println(target.toString()+ " cannot be formed from the given seeds")
		}
	}
	var results: List<Long> =  factorsOfTarget(numbers, 25918798);
	//find smallest and largest
	var min: Long= results[0];
	var max: Long = 0;
	for (i in 0..results.size - 1) {
		var element = results[i];
		if (element < min) {min = element}
		if (element > max) {max = element}
	}
	println("Min value is "+min.toString());
	println("Max value is "+ max.toString());
	println("Sum of these is "+ (min+max).toString());
}

fun readFileAsLinesUsingReadLines(fileName: String): List<String>
		= File(fileName).readLines();

fun seedsCanMakeTarget (seeds: List<Long>, target: Long): Boolean {
	//can the given target be formed from the given seeds?
	//if we find a match we can quit immediately
	for(i in 0..24) {
		var first: Long = seeds[i];
		if ((first * 2 != target) && (seeds.contains(target - first))) {
			return true;
		}
	}
	return false;
}

fun factorsOfTarget (numbers: List<Long>, target: Long): List<Long> {
	//start at 0 and continue to numbers.size - 2 (since we need at least two factors
	var start: Int = 0;
	var sum: Long = 0
	var targetFound: Boolean = false;
	var outOfRange: Boolean;
	var sumTooLarge: Boolean;
	var done: Boolean = false;
	var pos: Int;
	
	while(!targetFound) {
		pos = start;
		sum = 0;
		done = false;
		while (!done) {
			 sum += numbers[pos];
			 targetFound = (sum == target);
			 outOfRange = (pos > numbers.size -1)
			 sumTooLarge = (sum > target);
			 done = ((targetFound) || (sumTooLarge) || (outOfRange));
			if (targetFound) {
				//take a subList from start to pos
				return numbers.subList(start, pos);
			} else {
				pos += 1;
		    }
		}
		if (!targetFound) {
			start += 1;
		}
	}	
	return numbers;
}
//25918798 is the target