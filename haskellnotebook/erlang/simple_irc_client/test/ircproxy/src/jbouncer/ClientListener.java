/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of JBouncer.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: ClientListener.java,v 1.3 2004/03/01 19:13:37 pjm2 Exp $

*/

package jbouncer;

import java.io.*;
import java.net.*;

public class ClientListener extends Thread {

    public ClientListener(JBouncerManager manager, int port) throws IOException {
        this.manager = manager;
        this.port = port;
        ss = new ServerSocket(port);
    }

    public void run() {
        boolean running = true;
        while (running) {
            try {
                Socket socket = ss.accept();
                JBouncerManager.log("Connection received from " + socket.getInetAddress().getHostName());
                Thread thread = new ClientConnection(socket, manager);
                thread.start();
            }
            catch (IOException e) {
                // JBouncerManager.log("Error establishing socket. Don't panic.");
            }
        }
    }

    private JBouncerManager manager;
    private int port;
    private ServerSocket ss = null;

}