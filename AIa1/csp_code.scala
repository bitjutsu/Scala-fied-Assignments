import java.io.PrintWriter

import scala.io.Source
import scala.collection.mutable.Map
import scala.util.Random

/*
	AI Assignment 1
	Goal: Solve n by n crossword puzzles using hillclimbing with random restarts.

	Specification: http://cs.mcgill.ca/~jpineau/comp424/Homework/homework1.pdf (question 5)
*/

/* Parse input file */
object CSP {
	/* Returns word list and a mapping from "A1", "D1", etc to their initial values */
	def parseInputFile(fileName: String) : (List[String], Map[String, String]) = {
		/* Partition the list - assume all words are lower case */
		var (statesList, words) = Source.fromFile(fileName).getLines.toList.partition(_.contains(' '))

		/* Drop "STATE START" and "STATE END" */
		statesList = statesList.filter((x: String) => x(0) == 'A' || x(0) == 'D')

		/* Fold the statesList into a Map from variables to initial values */
		val state = statesList.foldLeft(Map.empty[String, String]) {
			(map: Map[String, String], assignment: String) => map + (assignment.split(' ')(0) -> assignment.split(' ')(1))
		}

		/* Return the tuple */
		(words, state)
	}

	/* Generate a random state */
	def randomState(state: Map[String, String], words: List[String]) : Map[String, String] = {
		/* Generate new random map */
		val rand : Random = new Random()
		state.foreach { case (key, value) => state(key) = words(rand.nextInt(words.size)) }
		state
	}

	/* Print state */
	def printState(state: Map[String, String], outFile: PrintWriter) = {
		outFile.println("STATE START")
		state.foreach { case (key, value) => outFile.println(key + " " + value) }
		outFile.println("STATE END")
	}

	/* Parse state file */
	def parseStateFile(fileName : String) = {
		var statesList = Source.fromFile(fileName).getLines.toList

		/* Find number of variables in this puzzle configuration */
		val numVars = statesList.indexOf("STATE END") - statesList.indexOf("STATE START") - 1

		/* Remove the state delimeters */
		statesList = statesList.filterNot((line: String) => line.equals("STATE END") || line.equals("STATE START"))

		/* Partition into numVars groups */
		val configurations = statesList.grouped(numVars)

		/* Map each state list to a Map */
		val states = configurations.foldLeft(List[Map[String, String]]()) {
			(maps: List[Map[String, String]], states: List[String]) => states.foldLeft(Map.empty[String, String]) {
				(map: Map[String, String], assignment: String) => map + (assignment.split(' ')(0) -> assignment.split(' ')(1))
			} :: maps
		}

		states
	}

	def main(args: Array[String]) {
		val (words, state) = parseInputFile("example_input.txt")
		val outFile: PrintWriter = new PrintWriter("example_output.txt", "UTF-8")
		try {
			printState(state, outFile)

			println(randomState(state, words))
			printState(state, outFile)
		} finally {
			outFile.close()
		}
	}
}