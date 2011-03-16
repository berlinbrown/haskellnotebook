/**
 * JGameLife.java
 */
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

/**
 * Date:    9/23/2008
 * Short Descr: OpenGL version, Conway's Game of Life in Java
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
public class JGameGLLife implements GLEventListener, KeyListener {

	private JRenderCanvas render;
	private JGameLife life = new JGameLife();

	public JGameGLLife(final JRenderCanvas rcanvas) {
		render = rcanvas;
		life.generateWorld();
	}

	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
			render.setQuit(true);
			System.exit(0);
		}
	}

	public void init(GLAutoDrawable glDrawable) {
        final GL gl = glDrawable.getGL();
        gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

        gl.glDepthFunc(GL.GL_LESS);
        gl.glEnable(GL.GL_DEPTH_TEST);
        gl.glShadeModel(GL.GL_FLAT);
        gl.glDepthRange(0.0, 1.0); /* The default z mapping */

        gl.glLoadIdentity();
        glDrawable.addKeyListener(this);
    }

	public void keyReleased(KeyEvent e) {
	}

	public void keyTyped(KeyEvent e) {
	}

	public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {

	}

	public void reshape(GLAutoDrawable arg0, int arg1, int arg2, int arg3,
			int arg4) {

	}

	private void renderVertLine(final GL gl, final Double x) {
		gl.glColor3f(0.0f, 0.0f, 1.0f);
		gl.glRectd(x, (-0.8), (x+0.004), 0.8);
	}
	private void renderHorizLine(final GL gl, final double y) {
		gl.glColor3f(0.0f, 0.0f, 1.0f);
		gl.glRectd((-0.8), y, 0.8, (y+0.004));
	}

	private void renderCell(final GL gl, final double i, final double j) {
	  final double sz    = 0.055;
	  final double x     = (i * sz) - 0.8;
	  final double y     = (j * sz) - 0.8;
	  gl.glColor3f(1.0f, 0.0f, 0.0f);
	  gl.glRectd(x, y, (sz+x), (sz+y));
	}

	private void renderBoard(final GL gl) {

		final boolean [][] cellWorld = life.getWorld();
    	for (int j = 0; j < cellWorld.length; j++) {
    		for (int i = 0; i < cellWorld[j].length; i++) {
    			if (cellWorld[j][i]) {
    				renderCell(gl, i, j);
    			}
    		}
    	} // End of outer for.

    	life.setNextGeneration();
	}

	/**
	  * Core display method.
	  */
	public void display(GLAutoDrawable glDrawable) {

        final GL gl = glDrawable.getGL();
        gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
        gl.glColor3f(0.0f, 0.0f, 1.0f);
        for (int i = 0; i < 32; i++) {
        	double pos = (i * 0.05) - 0.8;
        	renderVertLine (gl, pos);
        	renderHorizLine (gl, pos);
        }

        renderBoard(gl);

	}

} // End of Class
