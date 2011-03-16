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

import org.berlin.neural.heaton.tictac.game.Board;
import org.berlin.neural.heaton.tictac.game.Move;
import org.berlin.neural.heaton.tictac.game.TicTacToe;
import org.berlin.neural.heaton.tictac.nn.FeedforwardNetwork;
import org.berlin.neural.heaton.tictac.players.Player;

/**
 * Training using a Genetic Algorithm
 * 
 * PlayerNeural: Play Tic-Tac-Toe using a neural network. This class can be
 * played against any of the other player types that implement the Player
 * interface.
 * 
 * @author Jeff Heaton
 * @version 2.1
 */
public class PlayerNeural implements Player {

    /**
     * Field network.
     */
    private final FeedforwardNetwork network;

    /**
     * Constructor for PlayerNeural.
     * @param network FeedforwardNetwork
     */
    public PlayerNeural(final FeedforwardNetwork network) {
        this.network = network;
    }

    /**
     * Method getMove.
     * @param board byte[][]
     * @param prev Move
     * @param color byte
     * @return Move
     * @see org.berlin.neural.heaton.tictac.players.Player#getMove(byte[][], Move, byte)
     */   
    public Move getMove(final byte[][] board, final Move prev, final byte color) {
        Move bestMove = null;
        double bestScore = Double.MIN_VALUE;

        // Iterate through all of the possible moves 
        // and determine a best move //
        for (int x = 0; x < board.length; x++) {
            for (int y = 0; y < board.length; y++) {
                
                final Move move = new Move((byte) x, (byte) y, color);
                
                if (Board.isEmpty(board, move)) {
                    
                    // Test the move based on the current state of the board //
                    final double d = tryMoveOnEmpty(board, move);
                    if ((d > bestScore) || (bestMove == null)) {
                        bestScore = d;
                        bestMove = move;
                    } // End of if, check best score //
                } // End of the if - is empty //
                
            }
        } // End of the for //

        return bestMove;

    }

    /**
     * Method tryMove.
     * @param board byte[][]
     * @param move Move
     * @return double
     */
    private double tryMoveOnEmpty(final byte[][] board, final Move move) {
        final double input[] = new double[9];
        int index = 0;

        // Iterate through all of the pieces on the board
        // We only reach this date on empty //
        for (int x = 0; x < board.length; x++) {
            for (int y = 0; y < board.length; y++) {
                
                if (board[x][y] == TicTacToe.NOUGHTS) {
                    input[index] = -1;
                } else if (board[x][y] == TicTacToe.CROSSES) {
                    input[index] = 1;
                } else if (board[x][y] == TicTacToe.EMPTY) {
                    input[index] = 0;
                } // End of the if - else //

                if ((x == move.x) && (y == move.y)) {
                    input[index] = -1;
                }
                index++;
            }
        }
        
        // Based on the inputs, compute the output, which should be one
        // move.
        // The board will consist of 1s or -1s
        /*
         1.0  | 0.0  | -1.0 |
        -1.0  | 1.0  | 0.0  |
         0.0  | -1.0 | 0.0  |
         or
         x  | .  | o  |
         o  | x  | .  |
         .  | o  | .  |
        */        
        final double output[] = this.network.computeOutputs(input);
        
        // Return a score //
        return output[0];
    }

} // End of the Class //
