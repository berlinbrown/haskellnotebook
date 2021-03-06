/**
 * Modification to Jeff Heaton's Neural Network Book Code:
 * Modified By: Berlin Brown
 * Date: 8/31/2009
 */
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
package org.berlin.neural.heaton.tictac.nn.train;

import java.util.Arrays;

import org.berlin.neural.heaton.tictac.nn.FeedforwardNetwork;
import org.berlin.neural.heaton.tictac.nn.NeuralNetworkError;
import org.berlin.neural.heaton.tictac.nn.genetic.Chromosome;
import org.berlin.neural.heaton.tictac.nn.genetic.GeneticAlgorithm;
import org.berlin.neural.heaton.tictac.nn.matrix.MatrixCODEC;

/**
 * NeuralChromosome: Implements a chromosome that allows a feedforward neural
 * network to be trained using a genetic algorithm. The chromosome for a feed
 * forward neural network is the weight and threshold matrix.
 * 
 * This class is abstract. If you wish to train the neural network using
 * training sets, you should use the TrainingSetNeuralChromosome class. If you
 * wish to use a cost function to train the neural network, then implement a
 * subclass of this one that properly calculates the cost.
 * 
 * The generic type GA_TYPE specifies the GeneticAlgorithm derived class that
 * implements the genetic algorithm that this class is to be used with.
 * 
 * @author Jeff Heaton
 * @version 2.1
 */
abstract public class NeuralChromosome<GA_TYPE extends GeneticAlgorithm<?>> extends Chromosome<Double, GA_TYPE> {

    private static final Double ZERO = Double.valueOf(0);

    private static final double RANGE = 20.0;

    private FeedforwardNetwork network;

    /////////////////////////////////////////////////////////////////
    
    /**
     * @return the network
     */
    public FeedforwardNetwork getNetwork() {
        return this.network;
    }

    public void initGenes(final int length) {
        final Double result[] = new Double[length];
        Arrays.fill(result, ZERO);
        this.setGenesDirect(result);
    }

    /**
     * Mutate this chromosome randomly
     */
    @Override
    public void mutate() {
        
        final int length = getGenes().length;
        for (int i = 0; i < length; i++) {
            double d = getGene(i);
            final double ratio = (int) ((RANGE * Math.random()) - RANGE);
            d *= ratio;
            // set the gene with the ratio
            this.setGene(i, d);
        } // End of the for //
        
        // Print the genes
        // Genes print out, only on training.
        /*
...
...
i: 99 // -7.309353895817676
i: 100 // 0.3293668010547659
i: 101 // 2.857301000764527
i: 102 // -13.512293059595823
i: 103 // -10.850319299898004
i: 104 // 2.2378376752712485
i: 105 // 13.743926957201232
i: 106 // 0.9535471757765739
i: 107 // 3.7858729727967892
i: 108 // 14.140238312859065
i: 109 // -3.1389938815787404
i: 110 // -0.9304334974208146
         */
        /*
        final Double [] g = this.getGenes();
        System.out.print("genes: ");
        for (int i = 0; i < length; i++) {
            System.out.println("i: " + i + " // " + g[i] + " ");
        } // End of for print
        System.out.println();
        */
    }

    /**
     * Set all genes.
     * 
     * @param list
     *            A list of genes.
     * @throws NeuralNetworkException
     */
    @Override
    public void setGenes(final Double[] list) throws NeuralNetworkError {

        // copy the new genes
        super.setGenes(list);

        calculateCost();
    }

    /**
     * @param network
     *            the network to set
     */
    public void setNetwork(final FeedforwardNetwork network) {
        this.network = network;
    }

    public void updateGenes() throws NeuralNetworkError {
        this.setGenes(MatrixCODEC.networkToArray(this.network));
    }

    public void updateNetwork() {
        MatrixCODEC.arrayToNetwork(getGenes(), this.network);
    }

} // End of Class //
