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

import org.berlin.neural.heaton.tictac.game.ScorePlayer;
import org.berlin.neural.heaton.tictac.nn.FeedforwardNetwork;
import org.berlin.neural.heaton.tictac.nn.NeuralNetworkError;
import org.berlin.neural.heaton.tictac.nn.train.NeuralChromosome;
import org.berlin.neural.heaton.tictac.players.Player;

/**
 * Training using a Genetic Algorithm
 * 
 * TicTacToeChromosome: Implements a chromosome for the neural network
 * that plays tic-tac-toe.
 * 
 * @author Jeff Heaton
 * @version 2.1
 */
public class TicTacToeChromosome extends NeuralChromosome<TicTacToeGenetic> {

	/**
	 * The constructor, takes a list of cities to set the initial "genes" to.
	 * 
	 * @param genetic TicTacToeGenetic
	 * @param network FeedforwardNetwork
	 * @throws NeuralNetworkError
	 * @throws NeuralNetworkException
	 */
	public TicTacToeChromosome(final TicTacToeGenetic genetic,
			final FeedforwardNetwork network) throws NeuralNetworkError {
	    
		this.setGeneticAlgorithm(genetic);
		this.setNetwork(network);

		initGenes(network.getWeightMatrixSize());
		updateGenes();
	}

	/**
	 * Method calculateCost.
	 */
	@Override
	public void calculateCost() {

		try {
			// update the network with the new gene values
			this.updateNetwork();

			final PlayerNeural player1 = new PlayerNeural(getNetwork());			
			// Play games
			final Player player2 = (Player) this.getGeneticAlgorithm().getOpponent().newInstance();
			final ScorePlayer score = new ScorePlayer(player1, player2, false);
			setCost(score.score());
			
		} catch (final InstantiationException e) {
			e.printStackTrace();
		} catch (final IllegalAccessException e) {
			e.printStackTrace();
		}

	} // End of the Method //

} // End of the Class //
