
/**
 * Date:    9/23/2008
 * Short Descr: OpenGL version, Conway's Game of Life in Java
 * Contact: Berlin Brown <berlin dot brown at gmail.com>
 * Source License: See New BSD license at:
 * http://www.opensource.org/licenses/bsd-license.php
 *
 * Copyright (C) 2008 Botnode.com (Berlin Brown). All Rights Reserved.
 */
public class GLLifeMain {

	/**
	 * Main entry program for the Java game of life application.
	 * @param args
	 */
	public static void main(final String[] args) {

		System.out.println("# GameLife, GL Version");
		for (int i = 0; i < 20; i++) {
			System.out.print('=');
		}
		System.out.println();

		final Thread displayThread = new Thread(new JRenderCanvas());
		displayThread.start();
	}

}
