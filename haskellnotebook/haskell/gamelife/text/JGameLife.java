/**
 * JGameLife.java
 */
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Date:    9/23/2008
 * Short Descr: Conway's Game of Life in Java
 * Contact: Berlin Brown <berlin dot brown at gmail.com>
 * Source License: See New BSD license at:
 * http://www.opensource.org/licenses/bsd-license.php
 *
 * Copyright (C) 2008 Botnode.com (Berlin Brown). All Rights Reserved.
 *
 * Additional Resources:
 * [1] http://haskell.org/ghc/docs/latest/html/libraries/GLUT/Graphics-UI-GLUT.html
 * [2] http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/rect.html
 *
 * Random Note:
 *
 * It took me 2 hours to create this Java version and a week to finish the Haskell text version (2-3 hours a day).  This
 * is mainly due to the fact that I have been working with Java a lot longer.
 *
 * Possible Bug:
 * Printed output is not looking the same as the haskell version (where the haskell version is correct).  No common
 * game of life patterns found?
 */
public class JGameLife {

	// Default parameter, maxIter = 300
    public static final int maxIter       = 140;
    // Default parameter, randThres = 0.35
    private static final double randThres  = 0.65;
    private static final int boardSize     = 20;

    private static final boolean alive     = true;
    private static final boolean dead      = false;

    private String board;
    private boolean [][] cellWorld;

    private Random generator = new Random();

    private class Cell {
        int x = 0;
        int y = 0;
        private Cell() { }
        Cell(final int x, final int y) {
        	this.x = x;
        	this.y = y;
        }
    }

    /**
     * Determine if cell pos is within bounds.
     */
    private boolean boundsCheck(final Cell cell) {
    	final int x = cell.x;
    	final int y = cell.y;
        if (   (x < 0)
        	|| (x > (boardSize-1))
        	|| (y < 0)
        	|| (y > (boardSize-1))) {
        	return false;
        } else {
        	return true;
        }
    }
    /**
     * Get a temporary index list of cells surrounded around a this particular cell position.
     *
     * Haskell version:
     * <pre>
     * allCells :: Cell -> [Cell]
     * allCells (x,y) = [(nx,ny) | nx <- [x+1,x-1,x], ny <- [y+1,y-1,y],
     *                  ((x,y) /= (nx,ny) && boundsChk (nx,ny)) ]
     * </pre>
     */
    private Object [] allCells(final Cell cell) {
    	final int x = cell.x;
    	final int y = cell.y;
    	final List<Cell> cells = new ArrayList<Cell>();
    	int pos = 0;
    	// Row 0
    	cells.add(new Cell(x-1 , y-1));
    	cells.add(new Cell(x ,   y-1));
    	cells.add(new Cell(x+1 , y-1));

    	// Row 1
    	cells.add(new Cell(x-1 , y  ));
    	cells.add(new Cell(x   , y  ));
    	cells.add(new Cell(x+1 , y  ));

    	// Row 2
    	cells.add(new Cell(x-1 , y+1));
    	cells.add(new Cell(x   , y+1));
    	cells.add(new Cell(x-1 , y+1));
    	// Remove cells out of bounds.
    	for (int i = 0; i < cells.size(); i++) {
    		final Object c = cells.get(i);
    		if (!boundsCheck((Cell) c)) {
    			cells.remove(c);
    		}
    	}
    	return cells.toArray(new Cell[cells.size()]);
    }
    /**
     * Get Cell State at this location.
     */
    private boolean getState(final Cell cell) {
    	if (boundsCheck(cell)) {
    		return cellWorld[cell.y][cell.x];
    	} else {
    		return false;
    	}
    }

    /**
     * Get the state of the cells around one cell
     * Haskell version:
     * <pre>
     * allCellsState :: CellWorld -> Cell -> [CellState]
     * allCellsState world loc = map (\pos -> (getState world pos)) (allCells loc)
     * </pre>
     */
    private Object [] allCellsState(final Cell cell) {
    	final List<Boolean> cell_states = new ArrayList<Boolean>();
    	final Object [] cells = allCells(cell);
    	for (int i = 0; i < cells.length; i++) {
    		cell_states.add(getState((Cell) cells[i]));
    	}
    	return cell_states.toArray(new Boolean[cell_states.size()]);
    }

    /**
     * Use the game of life rules to determine of the cell state of this cell.
     */
    private boolean lifeRules(final Object [] cellStates, final boolean cur) {
    	int ct = 0;
    	for (int i = 0; i < cellStates.length; i++) {
    		if (((Boolean) cellStates[i])) {
    			ct++;
    		}
    	}
        if ((alive == cur)         && (ct <  2)) {
        	return false;
        } else if ((alive == cur)  && (ct >  3)) {
        	return false;
        } else if ((alive == cur)  && (ct == 2) || (ct == 3)) {
        	return true;
		} else if ((dead  == cur)  && (ct == 3)) {
			return true;
		} else {
			return false;
		} // End of if - rules
    }

    /**
     * What is the state of the cell based on what is around.
     */
    private boolean nextGenCell(final Cell cell) {
    	return lifeRules(allCellsState(cell), getState(cell));
    }
    /**
     * Given the state of the current world, recreate the environment.
     * Call map twice, to iterate through all of the cells.
     */
    public boolean [][] nextGeneration() {
    	// Instead of updating the state of the cellWorld field, return
    	// a newly created world and letter the caller determine if the main
    	// world needs to be updated.
    	final boolean [][] newWorld = new boolean[cellWorld.length][];
    	for (int j = 0; j < cellWorld.length; j++) {
    		// Create a new world row.
    		final boolean [] worldRow = new boolean[cellWorld[j].length];
    		for (int i = 0; i < cellWorld[j].length; i++) {
    			// Update the state of this world col with the new gen cell data.
    			worldRow[i] = nextGenCell(new Cell(j, i));
    		}
    		// Set the new row
    		newWorld[j] = worldRow;
    	} // End of for
    	return newWorld;
    }
    public void setNextGeneration() {
    	cellWorld = nextGeneration();
    }
    //===============================================
    // Board Utilities
    // ===============================================

    public boolean [][] getWorld() {
    	return this.cellWorld;
    }

    private void getCellWorld() {
    	int c = 0;
    	StringBuffer buf = new StringBuffer();
    	for (int j = 0; j < cellWorld.length; j++) {
    		for (int i = 0; i < cellWorld[j].length; i++) {
    			c++;
    			if (cellWorld[j][i]) {
    				buf.append('#');
    			} else {
    				buf.append('.');
    			} // End of if - else
    		}
    	} // End of outer for.
    	board = buf.toString();
    }

    /**
     * Purpose: Pretty print the game of life board.
     */
    public void pprintWorld() {
    	getCellWorld();
    	for (int j = 0; j < boardSize; j++) {
    		for (int i = 0; i < boardSize; i++) {
    			final int pos = (j * boardSize) + i;
    			System.out.print(board.charAt(pos));
    		}
    		System.out.println();
    	} // End of for
    	System.out.println();
    }

    /**
     * Create a row of random cell states.
     * @return
     */
    private boolean [] generateState() {
    	final boolean cellStateRow [] = new boolean [boardSize];
    	for (int i = 0; i < cellStateRow.length; i++) {
    		double r = generator.nextGaussian();
    		if (r > randThres) {
    			cellStateRow[i] = true;
    		} else {
    			cellStateRow[i] = false;
    		} // End if - else

    	} // End of the For
    	return cellStateRow;
    }

    public boolean [][] generateWorld() {
    	cellWorld = new boolean[boardSize][];
    	for (int i = 0; i < cellWorld.length; i++) {
    		cellWorld[i] = generateState();
    	}
    	return cellWorld;
    }

    /**
     * Main entry program for the Java game of life application.
     * @param args
     */
    public static void main(final String [] args) {

    	System.out.println("# GameLife");
    	for (int i = 0; i < 20; i++) { System.out.print('='); }
    	System.out.println();

    	// Begin life simulation.
    	JGameLife life = new JGameLife();
    	life.generateWorld();
    	System.out.println("** Original Board **");
    	for (int i = 0; i < 20; i++) { System.out.print('='); } ; System.out.println();
    	life.pprintWorld();
    	//*******************
    	// Continue with game loop
    	//*******************
    	for (int i = 0; i < maxIter; i++) {
    		System.out.println("** Board " + i + "**");
    		for (int z = 0; z < 20; z++) { System.out.print('='); } ; System.out.println();

    		life.setNextGeneration();
    		life.pprintWorld();
    	}
    }

} // End of Class
