/**
 * Introduction to Neural Networks with Java, 2nd Edition
 * Copyright 2008 by Heaton Research, Inc. 
 * http://www.heatonresearch.com/books/java-neural-2/
 * 
 * ISBN13: 978-1-60439-008-7  	 
 * ISBN:   1-60439-008-5
 *   
 * This class is released under the:
 * GNU Lesser General Public License (LGPL)
 * http://www.gnu.org/copyleft/lesser.html
 */
package org.berlin.neural.heaton.tictac.players.neural;

import org.berlin.neural.heaton.tictac.nn.FeedforwardNetwork;
import org.berlin.neural.heaton.tictac.nn.NeuralNetworkError;
import org.berlin.neural.heaton.tictac.nn.train.NeuralChromosome;
import org.berlin.neural.heaton.tictac.nn.train.NeuralGeneticAlgorithm;

/**
 * Training using a Genetic Algorithm
 * 
 * TicTacToeGenetic: Use a genetic algorithm to teach a neural network
 * to play Tic-Tac-Toe.  The cost of each chromosome is calculated
 * by playing the neural network against a computer player.
 * 
 * @author Jeff Heaton
 * @version 2.1
 */
public class TicTacToeGenetic extends NeuralGeneticAlgorithm<TicTacToeGenetic> {

	/**
	 * Field opponent.
	 */
	private Class<?> opponent;

	/**
	 * Constructor for TicTacToeGenetic.
	 * @param network FeedforwardNetwork
	 * @param reset boolean
	 * @param populationSize int
	 * @param mutationPercent double
	 * @param percentToMate double
	 * @param opponent Class<?>
	 * @throws NeuralNetworkError
	 */
	public TicTacToeGenetic(final FeedforwardNetwork network,
			final boolean reset, final int populationSize,
			final double mutationPercent, final double percentToMate,
			final Class<?> opponent) throws NeuralNetworkError {

	    final long tStart = System.currentTimeMillis();
	    
		this.setOpponent(opponent);
		this.setMutationPercent(mutationPercent);
		this.setMatingPopulation(percentToMate * 2);
		this.setPopulationSize(populationSize);
		this.setPercentToMate(percentToMate);

		setChromosomes(new TicTacToeChromosome[getPopulationSize()]);
		for (int i = 0; i < getChromosomes().length; i++) {
		    System.out.println("[TicTacToeGenetic] - {constructor} = size=" + getChromosomes().length + " index=" + i);
			final FeedforwardNetwork chromosomeNetwork = (FeedforwardNetwork) network.clone();
			if (reset) {
				chromosomeNetwork.reset();
			}

			final TicTacToeChromosome c = new TicTacToeChromosome(this, chromosomeNetwork);
			c.updateGenes();
			setChromosome(i, c);
			System.out.println("TicTacToeGenetic [getChromosomes]");
		} // End of the for //
		sortChromosomes();
		
		final long tEnd = System.currentTimeMillis();
		final long tDiff = tEnd - tStart;
		
		System.out.println("Runtime performance, create [NeuralGeneticAlgorithm] constructor, diff=" + tDiff);
	}

	/**
	 * @return the opponent
	 */
	public Class<?> getOpponent() {
		return this.opponent;
	}

	/**
	 * Method getScore.
	 * @return double
	 */
	public double getScore() {
		final NeuralChromosome<TicTacToeGenetic> c = getChromosome(0);
		return c.getCost();
	}

	/**
	 * @param opponent
	 *            the opponent to set
	 */
	public void setOpponent(final Class<?> opponent) {
		this.opponent = opponent;
	}

} // End of Class //
