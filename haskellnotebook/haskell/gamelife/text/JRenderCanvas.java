import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GLCanvas;

/**
 * Date:    9/23/2008
 * Short Descr: OpenGL version, Conway's Game of Life in Java
 * Contact: Berlin Brown <berlin dot brown at gmail.com>
 * Source License: See New BSD license at:
 * http://www.opensource.org/licenses/bsd-license.php
 *
 * Copyright (C) 2008 Botnode.com (Berlin Brown). All Rights Reserved.
 *
 */
public class JRenderCanvas implements Runnable {

	private boolean quit = false;

	public void run() {
		Frame frame = new Frame("Game of Life with Jogl");
		GLCanvas canvas = new GLCanvas();
		canvas.addGLEventListener(new JGameGLLife(this));
		frame.add(canvas);
		frame.setSize(640, 480);
		//frame.setUndecorated(true);
		//int size = frame.getExtendedState();
		//size |= Frame.MAXIMIZED_BOTH;
		//frame.setExtendedState(size);
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				 quit = true;
			}
		});
		frame.setVisible(true);
		canvas.requestFocus();
		while (!quit) {
			canvas.display();
		}
	} // End of the method

	public void setQuit(boolean quit) {
		this.quit = quit;
	}
}
