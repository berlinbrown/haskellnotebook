/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of JBouncer.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: JBouncerMain.java,v 1.3 2004/03/01 19:13:37 pjm2 Exp $

*/

package jbouncer;

import java.util.*;
import java.io.*;

public class JBouncerMain {

    public static int HISTORY_LIMIT = 10;
    public static boolean TAKE_LOGS = false;

    public static void main(String[] args) throws Exception {

        Properties p = new Properties();
        try {
            p.load(new FileInputStream("./config.ini"));
        }
        catch (IOException e) {
            JBouncerManager.log("Could not read the config file.");
            System.exit(1);
        }

        String portStr = p.getProperty("Port");
        int port = 6667;
        if (portStr != null) {
            try {
                port = Integer.parseInt(portStr);
            }
            catch (NumberFormatException e) {
                // Keep the default value;
            }
        }

        String historyStr = p.getProperty("HistoryLimit");
        if (historyStr != null) {
            try {
                HISTORY_LIMIT = Integer.parseInt(historyStr);
            }
            catch (NumberFormatException e) {
                // Keep the default value;
            }
        }
        JBouncerManager.log("The message history limit for each session is " + HISTORY_LIMIT);

        String logStr = p.getProperty("TakeLogs");
        if (logStr != null) {
            TAKE_LOGS = Boolean.valueOf(logStr).booleanValue();
        }
        if (TAKE_LOGS) {
            JBouncerManager.log("Logging activated.");
        }

        // Populate the HashMap of bouncers (one per user).
        HashMap bouncers = new HashMap();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("./accounts.ini"));
            String line = null;
            int count = 0;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split("\\s+");
                if (parts.length == 2) {
                    String login = parts[0];
                    String password = parts[1];
                    User user = new User(login, password);
                    if (!login.startsWith("#") && !bouncers.containsKey(user)) {
                        bouncers.put(user, new JBouncer(user));
                        JBouncerManager.log("Created bouncer for " + login);
                    }
                }
            }
        }
        catch (Exception e) {
            System.out.println("Unable to process accounts.ini.");
            System.exit(1);
        }

        JBouncerManager manager = new JBouncerManager(bouncers);

        // Now allow clients to connect.
        try {
            ClientListener listener = new ClientListener(manager, port);
            listener.start();
        }
        catch (IOException e) {
            System.out.println("Cannot listen on port " + port);
            System.exit(1);
        }

        JBouncerManager.log("*** JBouncer ready to accept connections on port " + port);

    }

}