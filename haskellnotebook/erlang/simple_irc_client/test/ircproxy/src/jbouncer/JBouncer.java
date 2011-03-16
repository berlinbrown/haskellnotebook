/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of JBouncer.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: JBouncer.java,v 1.2 2004/03/01 19:13:37 pjm2 Exp $

*/

package jbouncer;

import java.io.*;
import java.net.*;
import java.util.*;

public class JBouncer {

    public JBouncer(User user) {
        this.user = user;
    }

    public void add(String key, ServerConnection server) {
        synchronized (servers) {
            servers.put(key, server);
        }
        JBouncerManager.log(user.getLogin() + " is now connected to " + servers.size() + " servers.");
        server.connect();
    }

    public void remove(String key) {
        synchronized (servers) {
            ServerConnection server = (ServerConnection) servers.get(key);
            if (server != null) {
                servers.remove(key);
                server.quitServer();
                server.dispose();
                JBouncerManager.log(user.getLogin() + " removed server " + server.getServer());
            }
        }
    }

    public int getClientCount() {
        int total = 0;
        synchronized (servers) {
            Iterator it = servers.values().iterator();
            while (it.hasNext()) {
                ServerConnection server = (ServerConnection) it.next();
                total += server.getClientCount();
            }
        }
        return total;
    }

    public HashMap getServers() {
        return servers;
    }

    private User user;
    private HashMap servers = new HashMap();

}