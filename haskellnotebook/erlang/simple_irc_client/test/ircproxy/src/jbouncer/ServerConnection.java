/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of JBouncer.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: ServerConnection.java,v 1.5 2004/03/02 00:32:27 pjm2 Exp $

*/

package jbouncer;

import org.jibble.pircbot.*;
import java.io.*;
import java.net.*;
import java.util.*;

public class ServerConnection extends PircBot {

    public ServerConnection(String server, int port, String password, String nick, User user) {
        this.server = server;
        this.port = port;
        this.password = password;
        this.user = user;

        setName(nick);
        setLogin("JBouncer");
        setVersion("JBouncer IRC Proxy 1.0 www.jibble.org");
        setVerbose(false);
        setAutoNickChange(true);

        JBouncerManager.log(user.getLogin() + " initiated new connection to " + server);
    }

    public void connect() {
        int tries = 0;
        boolean trying = true;
        while (trying) {
            tries++;
            try {
                sendToOtherClients(null, ClientConnection.METHOD + "JBouncer is trying to connect to " + server + " (retry #" + tries + ")");
                connect(server, port, password);
                sendToOtherClients(null, ClientConnection.METHOD + "JBouncer is connected to " + server);
                return;
            }
            catch (Exception e) {
                sendToOtherClients(null, ClientConnection.METHOD + "JBouncer could not connect. Trying again in 5 minutes.");
            }

            try {
                Thread.sleep(5 * 60 * 1000);
            }
            catch (InterruptedException e) {
                // This should not happen.
            }
        }
    }

    public void onServerResponse(int code, String message) {
        if (code >= 0 && code <= 5) {
            serverMessages[code] = message;
        }
    }

    public void onDisconnect() {
        connect();
    }

    public void handleLine(String line) {
        addToHistory(line);
        sendToOtherClients(null, line);
        super.handleLine(line);
    }

    public void sendToOtherClients(ClientConnection sendingClient, String line) {
        synchronized (clients) {
            Iterator it = clients.iterator();
            while (it.hasNext()) {
                try {
                    ClientConnection client = (ClientConnection) it.next();
                    if (client != sendingClient) {
                        client.sendRawLine(line);
                    }
                }
                catch (IOException e) {
                    it.remove();
                }
            }
        }
    }

    public void removeClient(ClientConnection client) {
        synchronized (clients) {
            clients.remove(client);
        }
        JBouncerManager.log(user.getLogin() + " detached from session on " + server);
    }

    public int getClientCount() {
        return clients.size();
    }

    public void add(ClientConnection client) {
        synchronized (clients) {
            clients.add(client);
        }
        JBouncerManager.log(user.getLogin() + " attached to session on " + server);
        JBouncerManager.log(user.getLogin() + " is now using " + getClientCount() + " sessions.");
    }

    public String[] getServerMessages() {
        return serverMessages;
    }

    public void addToHistory(String line) {
        String[] parts = line.split("\\s+", 4);
        if (parts.length == 4) {
            String type = parts[1];
            if (type.equals("PRIVMSG")) {
                String from = parts[0];
                int index = from.indexOf('!');
                if (index > 1) {
                    from = from.substring(1, index);
                }
                String target = parts[2];
                String message = parts[3].substring(1);
                message = Colors.removeFormattingAndColors(message);

                String where = from;
                if ("#&!+".indexOf(target.charAt(0)) >= 0) {
                    where = target;
                }

                log(where, "<" + from + "> " + message);

                history.add(line);
            }
        }
        if (history.size() > JBouncerMain.HISTORY_LIMIT) {
            history.removeFirst();
        }
    }

    // Returns a new collection to avoid blocking or thread safety issues.
    public LinkedList getHistory() {
        return new LinkedList(history);
    }

    private synchronized void log(String where, String line) {
        try {
            File dir = new File("./logs/" + user.getLogin());
            if (dir.isDirectory() || dir.mkdirs()) {
                BufferedWriter writer = new BufferedWriter(new FileWriter(new File(dir, where + "." + server + ".log"), true));
                writer.write(line + "\r\n");
                writer.flush();
                writer.close();
            }
        }
        catch (IOException e) {
            // Do nothing.
        }
    }

    private String server;
    private int port;
    private String password;
    private User user;

    private LinkedList clients = new LinkedList();

    private String[] serverMessages = new String[6];
    private LinkedList history = new LinkedList();

}